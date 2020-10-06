-module(typer).
-export([type/2]).

type(AST, Options) -> 
    PurgeScannerMod = maps:get(purge_scanner_module, Options, true),
    case prepare(AST) of
        {error, Errs}                       -> {error, Errs};
        {ok, {ArgsEnv, TypesEnv, TypedAST}} ->
            case gen(TypedAST, TypesEnv, ArgsEnv) of
                {error, Errs}                           -> {error, Errs};
                {ok, {Exported, ScannerMod, TypeMods}}  ->
                    Envs = scanner:scan(ScannerMod, AST),
                    case PurgeScannerMod of
                        true    ->
                            true = code:soft_purge(ScannerMod),
                            true = code:delete(ScannerMod),
                            {ok, {Envs, Exported, TypeMods}};
                        false   ->
                            {ok, {Envs, Exported, [ScannerMod | TypeMods]}}
                    end
            end
    end.

prepare(AST) ->
    error:flatmap2(collect_args(AST),
                   collect_types(AST),
                   fun({ArgsEnv, _}, {TypesEnv, TypedAST}) -> {ok, {ArgsEnv, TypesEnv, TypedAST}} end).

gen(AST, TypesEnv, ArgsEnv) ->
    case type_gen:gen(TypesEnv, ArgsEnv, AST) of
        {error, Errs}                           -> {error, Errs};
        {ok, {Exported, ScannerMod, TypeMods}}  ->
            AllModules = [ScannerMod | TypeMods],
            LoadedModules = error:collect([load_type_module(Name, Form) || {Name, Form} <- AllModules]),
            error:map(LoadedModules, fun([ScannerModule | Mods]) ->
                                             {Exported, ScannerModule, Mods} end)
    end.

load_type_module(Name, ModuleForm) ->
    case compile:forms(ModuleForm, [report, verbose, from_core]) of
        error -> error:format({compilation_error}, {typer, Name});
        {error, Err} -> error:format({compilation_error, Err}, {typer, Name});
        {ok, Name, TypeBin} ->
            BeamName = lists:flatten([atom_to_list(Name), ".beam"]),
            code:load_binary(Name, BeamName, TypeBin),
            {ok, Name}
    end.

collect_args({ast, _, _, _, Defs} = AST) ->
    TopLevelTypes = maps:from_list([{Name, []} || Name <- maps:keys(Defs)]),
    ast:traverse(fun args_pre/3, fun args_post/3, TopLevelTypes, AST).
collect_types({ast, _, _, _, Defs} = AST) ->
    Scope = maps:from_list([{[Name], Args} || {type_def, _, Name, Args, _} <- maps:values(Defs)]),
    ast:traverse(fun types_pre/3, fun types_post/3, Scope, AST).

% args_pre only tags the pair key to make sure we know its tag after the key
% has been converted to an empty list of args
args_pre(_, _, {def, _, _, _, _}) -> skip;
args_pre(_, _, {pair, _, {type, _, _, _} = T, _} = Term) ->
    {ok, ast:tag(tag, Term, symbol:tag(T))};
args_pre(_, _, _) -> ok.


% args_post compacts any term down to a list of the free variable present in
% the term itself or its children. For each term we map the list of free
% variables to the term id in the returned environemnt
args_post(expr, _, {variable, _, _, Tag}) -> {ok, [{var, Tag}]};
args_post(expr, Scope, {type, _, _, _} = T) ->
    Tag = symbol:tag(T),
    case maps:is_key(Tag, Scope) of
        true    -> {ok, []};
        false   -> {ok, Tag, []}
    end;
args_post(expr, _, {key, _, _}) -> {ok, []};
args_post(expr, _, {pair, _, _, Val} = Term) ->
    case ast:get_tag(tag, Term, undefined) of
        undefined -> {ok, get_vars(Term)};
        Tag -> {ok, Tag, Val}
    end;
args_post(_, _, {type_def, _, Name, Args, _Expr}) ->
    {ok, Name, [{var, symbol:tag(S)} || S <- Args]};
args_post(pattern, _, _) -> skip;
args_post(_, _, Term) when is_tuple(Term) ->
    {ok, get_vars(Term)}.

get_vars(Term) when is_tuple(Term) ->
    Args = [{var, V} || I <- lists:seq(3, size(Term)), {var, V} <- lists:flatten([element(I, Term)])],
    utils:unique(Args);
get_vars(Term) when is_list(Term) -> utils:unique([V || T <- Term, V <- get_vars(T)]).


% types_pre adds a `path` tag to the term contexts and checks a few
% assumptions about the type tree. It also renames `pairs` inside of a
% dict to `dict_pair` to make it easier to generate erlang core afterwards
types_pre(top_level, _, {ast, _, _, _, _})  -> ok;
types_pre(top_level, _, {def, _, _, _, _})  -> skip;
types_pre(top_level, _, {type_def, _, Name, _, _} = Term) -> {ok, ast:tag(path, Term, [Name])};
types_pre(_, _, {pair, Ctx, {type, _, _, Path} = T, Val}) ->
    F = fun(Tag) -> [symbol:tag(T) | Tag] end,
    Tagged = ast:tag(path, Val, F, []),
    {ok, {tagged, Ctx, Path, Tagged}};
types_pre(Type, _, {dict, Ctx, Elements} = Term) ->
    F = fun({pair, X, {key, _, _} = K, V})  -> {ok, {dict_pair, X, K, V}};
           ({variable, X, Name, _} = Val)   -> {ok, {dict_pair, X, {key, X, Name}, Val}};
           ({pair, _, K, _})                -> error:format({unrecognized_tag_type, K}, {typegen, Type, Term});
           (Elem)                           -> error:format({unrecognized_product_elem, Elem}, {typegen, Type, Term}) end,
    error:map(error:collect([F(Elem) || Elem <- Elements]),
              fun(TaggedElements) -> ast:tag(path, {dict, Ctx, TaggedElements}) end);
types_pre(pattern, _, {application, _, Expr, Args} = Term) ->
    error:format({pattern_application, Expr, Args}, {typegen, pattern, Term});
types_pre(_, _, {application, Ctx, {type, _, _, _} = T, Args}) ->
    {ok, ast:tag(path, {type_application, Ctx, symbol:tag(T), Args})};
types_pre(_, _, Term) -> {ok, ast:tag(path, Term)}.


% types_post collects all types defined by the type definitions in the AST.
% This includes the type defs and any tags
types_post(top_level, _, {type_def, _, Name, _, _} = Term)  -> {ok, [Name], Term};
types_post(expr, _, {tagged, _, Path, _} = Term)            -> {ok, Path, Term};
types_post(expr, Scope, {type, _, _, Path} = Term)          ->
    case maps:is_key(Path, Scope) of
        true    -> {ok, Term};
        false   -> {ok, Path, Term}
    end;
types_post(_, _, _)                                         -> ok.
