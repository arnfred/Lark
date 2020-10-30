-module(typer).
-export([type/2]).

-include_lib("eunit/include/eunit.hrl").

type(AST, Options) -> 
    case prepare(AST) of
        {error, Errs}     		-> {error, Errs};
        {ok, {_, _, TypedAST}} 	->
            case typecheck(TypedAST) of
                {error, Mods, Errs} 		-> unload_type_modules(Mods),
                                               {error, Errs};
                {ok, {TypesEnv, Modules}}   -> {ok, {TypedAST, TypesEnv, Modules}}
            end
    end.

prepare(AST) ->
    case collect_args(AST) of
        {error, Errs}               -> {error, Errs};
        {ok, {ArgsEnv, _}}          ->
            case collect_types(ArgsEnv, AST) of
                {error, Errs}               -> {error, Errs};
                {ok, {TypesEnv, TypesAST}}  ->
                    {ok, {ArgsEnv, TypesEnv, TypesAST}}
            end
    end.

gen(AST, TypesEnv, ArgsEnv) ->
    case type_gen:gen(TypesEnv, ArgsEnv, AST) of
        {error, Errs}                           -> {error, Errs};
        {ok, {Exported, ScannerMod, TypeMods}}  ->
            AllModules = [ScannerMod | TypeMods],
            LoadedModules = error:collect([load_type_module(Name, Form) || {Name, Form} <- AllModules]),
            error:map(LoadedModules, fun([ScannerModule | Mods]) ->
                                             {Exported, ScannerModule, Mods} end)
    end.

typecheck({ast, _, ASTModules, _, _} = AST) ->
    case domain_gen:gen(AST) of
        {error, Errs}       			-> {error, Errs};
        {ok, {TypesEnv, TypeModules}} 	->
            Ms = [load_type_module(Name, Module) || {Name, Module} <- TypeModules],
            case error:collect(Ms) of
                {error, Errs}       -> Loaded = lists:flatten(lists:map(fun({ok, Mod}) -> [Mod];
                                                                           ({error, _}) -> [] end, Ms)),
									   {error, Loaded, Errs};
                {ok, ModuleNames} ->
                    % For each export in the module, check that it works with `any` as parameter
                    MakeF = fun(Path, {export, _, [T], _Domain}) -> {module:beam_name(Path ++ [domain]), T};
                               (Path, {export, _, [S, T], _Domain}) -> {module:beam_name(Path ++ [domain]), T} end,
                    Exported = [MakeF(ModulePath, Export) || {module, _, ModulePath, Exports} <- ASTModules,
                                                             Export <- maps:values(Exports)],
                    case error:collect([typecheck_def(DomainMod, Name) || {DomainMod, Name} <- Exported]) of
                        {error, Errs}   -> {error, ModuleNames, Errs};
                        {ok, _}         -> {ok, {TypesEnv, ModuleNames}}
                    end
            end
    end.

typecheck_def(DomainMod, Name) ->
    Arity = utils:get_min_arity(DomainMod, Name),
    Anys = fun(N) -> [any || _ <- lists:seq(1, N)] end,
    erlang:apply(DomainMod, Name, [normal | Anys(Arity)]).

load_type_module(Name, ModuleForm) ->
    case compile:forms(ModuleForm, [report, verbose, from_core]) of
        error -> error:format({compilation_error}, {typer, Name});
        {error, Err} -> error:format({compilation_error, Err}, {typer, Name});
        {ok, Name, TypeBin} ->
            BeamName = lists:flatten([atom_to_list(Name), ".beam"]),
            code:load_binary(Name, BeamName, TypeBin),
            {ok, Name}
    end.

unload_type_modules(Modules) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].

collect_args({ast, _, _, _, Defs} = AST) ->
    TopLevelTypes = maps:from_list([{Name, []} || Name <- maps:keys(Defs)]),
    ast:traverse(fun args_pre/3, fun args_post/3, TopLevelTypes, AST).

collect_types(ArgsEnv, {ast, _, _, _, Defs} = AST) ->
    Scope = maps:from_list([{Name, true} || {type_def, _, Name, _} <- maps:values(Defs)]),
    ast:traverse(make_types_pre(ArgsEnv), fun types_post/3, Scope, AST).

% args_pre only tags the pair key to make sure we know its tag after the key
% has been converted to an empty list of args
args_pre(_, _, {def, _, _, _}) -> skip;
args_pre(_, _, {pair, _, {type, _, _, _} = T, _} = Term) ->
    {ok, ast:tag(tag, Term, symbol:tag(T))};
args_pre(_, _, _) -> ok.


% args_post compacts any term down to a list of the free variable present in
% the term itself or its children. For each term we map the list of free
% variables to the term id in the returned environemnt
args_post(expr, Scope, {variable, _, _, Tag} = Var) -> 
    Name = symbol:name(Var),
    case maps:is_key(Name, Scope) of
        true    -> {ok, []};
        false   -> {ok, [{var, Tag}]}
    end;
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
args_post(_, _, {type_def, _, Name, Expr}) ->
    {ok, Name, Expr};
args_post(_, _, {clause, _, Patterns, _}) ->
    {ok, [{var, symbol:id('')} || _ <- Patterns]};
args_post(_, _, {'fun', _, [Clause | _]}) ->
    {ok, Clause};
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
make_types_pre(ArgsEnv) -> fun(Type, Scope, Term) -> tag(types_pre(ArgsEnv, Type, Scope, Term)) end.

tag({error, Errs})      -> {error, Errs};
tag({ok, Term})         -> {ok, ast:tag(path, Term)}.


types_pre(_, top_level, _, {ast, _, _, _, _})  -> ok;
types_pre(_, _, _, {def, _, Name, _} = Term)  ->
    F = fun(Path) -> [Name | Path] end,
    Tagged = ast:tag(path, Term, F, []),
    {ok, Tagged};
types_pre(_, _, _, {type_def, _, Name, _} = Term) -> 
    F = fun(Path) -> [Name | Path] end,
    Tagged = ast:tag(path, Term, F, []),
    {ok, Tagged};
types_pre(ArgsEnv, _, _, {pair, Ctx, {type, _, _, Path} = T, Val}) ->
    F = fun(Tag) -> [symbol:tag(T) | Tag] end,
    Tagged = ast:tag(path, Val, F, []),
    NewCtx = maps:put(args, maps:get(symbol:tag(T), ArgsEnv, []), Ctx),
    {ok, {tagged, NewCtx, Path, Tagged}};
types_pre(_, Type, _, {dict, Ctx, Elements} = Term) ->
    F = fun({pair, X, {key, _, _} = K, V})  -> {ok, {dict_pair, X, K, V}};
           ({variable, X, Name, _} = Val)   -> {ok, {dict_pair, X, {key, X, Name}, Val}};
           ({pair, _, K, _})                -> error:format({unrecognized_tag_type, K}, {typegen, Type, Term});
           (Elem)                           -> error:format({unrecognized_product_elem, Elem}, {typegen, Type, Term}) end,
    error:map(error:collect([F(Elem) || Elem <- Elements]),
              fun(TaggedElements) -> {dict, Ctx, TaggedElements} end);

types_pre(_, _, _, {application, Ctx, {qualified_symbol, _, ModulePath, Name}, Args}) ->
    {ok, {qualified_application, Ctx, ModulePath, Name, Args}};

types_pre(_, _, _, {type, Ctx, Name, Path} = Term) ->
    Tag = symbol:tag(Term),
    PathTag = ast:get_tag(path, Term),
    IsRecursive = lists:member(Tag, lists:droplast(PathTag)),
    case IsRecursive of
        true    -> {ok, {recursive_type, Ctx, Name, Path}};
        false   -> {ok, Term}
    end;

types_pre(_, _, _, {application, Ctx, {type, _, _, _} = T, Args} = Term) ->
    Tag = symbol:tag(T),
    PathTag = ast:get_tag(path, Term),
    IsRecursive = lists:member(Tag, lists:droplast(PathTag)),
    case IsRecursive of
        true    -> {ok, {recursive_type_application, Ctx, Tag, Args}};
        false   -> {ok, {application, Ctx, T, Args}}
    end;

types_pre(_, _, _, Term) -> {ok, Term}.



% types_post collects all types defined by the type definitions in the AST.
% This includes the type defs and any tags
types_post(top_level, _, {type_def, _, Name, _} = Term) -> {ok, Name, Term};
types_post(expr, _, {tagged, _, Path, _} = Term)        -> {ok, symbol:tag(Path), Term};
types_post(expr, Scope, {type, _, _, _} = Term)      ->
    case maps:is_key(symbol:tag(Term), Scope) of
        true    -> {ok, Term};
        false   -> {ok, symbol:tag(Term), Term}
    end;
types_post(_, _, _)                                     -> ok.
