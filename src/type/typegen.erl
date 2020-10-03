-module(typegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").
gen(AST) ->
    error:flatmap2(collect_args(AST),
                   collect_types(AST),
                   fun({ArgsEnv, _}, {TypesEnv, TypedAST}) ->
                           case gen_types(TypesEnv, ArgsEnv, TypedAST) of
                               {error, Errs}    -> {error, Errs};
                               {ok, {Types, _}} -> gen_modules(Types, TypesEnv, ArgsEnv, TypedAST)
                           end
                   end).

gen_types(TypesEnv, ArgsEnv, AST) -> 
    Post = fun(Type, Scope, Term) -> gen_term(TypesEnv, ArgsEnv, Type, Scope, Term) end,
    ast:traverse(pre_gen(TypesEnv, ArgsEnv), Post, AST).

gen_modules(Types, TypesEnv, ArgsEnv, {ast, _, Modules, _, _} = AST) ->
    % Create a map from types to type members
    F = fun([Parent | _] = Path, {tagged, _, _, _} = Term) ->
                Tag = symbol:tag(Term),
                Name = lists:last(Path),
                Args = maps:get(Tag, ArgsEnv),
                {Parent, {Name, Tag, Args}};
           ([Parent | _], {type, _, Name, _} = T) ->
                Tag = symbol:tag(T),
                {Parent, {Name, Tag, []}}
        end,
    TypePaths = [F(Path, Term) || {Path, Term} <- maps:to_list(TypesEnv), length(Path) > 1],
    TypeParents = utils:group_by(fun({K, _}) -> K end,
                                 fun({_, V}) -> V end,
                                 TypePaths),

    % Inside a type module for a specific type we might refer to other types
    % created in the parent module. For this reason we include a domain
    % function of all module types within each type module
    DomainDef = gen_domain(Types),

    % For each top level type (e.g. `Boolean`) we create a module which exports
    % its type members (e.g. `True` and `False`). These type modules serve to
    % simplify the import logic by making it trivial to import type members
    % from compiled beam files. For a bit more context see commit
    % 34ea58050b0200236324e963cd14ceb58ac1a32d
    TypeModules = [gen_type_modules(Modules, Parent, Children, DomainDef) 
                   || {Parent, Children} <- TypeParents],

    % The scanner needs access to call all type functions. For this purpose a
    % temporary type module is created which exports these types.
    ScannerModule = gen_scanner_module(Types, AST),

    % A normal module must contain the type definitions that it exports. To do
    % so we generate a map of these exports that can be included in codegen
    % when the module is created
    case error:collect([gen_exported_types(Mod, Types, ArgsEnv) || Mod <- Modules]) of
        {error, Errs}           -> {error, Errs};
        {ok, ExportedTypeList}  ->
            ExportedTypes = lists:flatten(ExportedTypeList),
            {ok, {ExportedTypes, ScannerModule, lists:flatten(TypeModules), DomainDef}}
    end.


% For each module in the AST we create a type module which exports the children of the type
gen_type_modules(_, _, [], _) -> [];
gen_type_modules(Modules, Parent, Children, DomainDef) ->
    ModuleNames = [module:beam_name(Path ++ [Parent]) || {module, _, Path, Exports} <- Modules,
                                                         maps:is_key(Parent, Exports)],
    Defs = [gen_type_def(Name, Tag, Args) || {Name, Tag, Args} <- Children],
    {Exports, _} = lists:unzip(Defs),
    [{ModuleName, cerl:c_module(cerl:c_atom(ModuleName), Exports, [], [DomainDef | Defs])} ||
     ModuleName <- ModuleNames].

gen_exported_types({module, _, _, Exports}, Types, ArgsEnv) ->
    F = fun({Name, {type_export, Ctx, Key, _}}) ->
                Tag = symbol:tag(Key),
                case maps:is_key(Tag, Types) of
                    false   -> error:format({undefined_type_export, Tag}, {typegen, Ctx});
                    true    -> 
                        {ok, gen_type_def(Name, Tag, maps:get(Tag, ArgsEnv))}
                end
        end,
    error:collect([F(Entry) || {_, {type_export, _, _, _}} = Entry <- maps:to_list(Exports)]).

gen_scanner_module(Types, {ast, _, _, _, Defs}) ->
    ModuleName = symbol:id(types),
    DomainDef = gen_domain(Types),

    % Top level defs `TypeMod:T()` for `T` in `type T -> ...`
    ArgsF = fun(Args) -> 
                    [{var, symbol:tag(A)} || A <- Args] end,
    TopLevelDefs = [gen_type_def(Name, Name, ArgsF(Args)) || {type_def, _, Name, Args, _} <- maps:values(Defs)],

    {Exports, _} = lists:unzip([DomainDef | TopLevelDefs]),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), Exports, [], [DomainDef | TopLevelDefs])}.

% Generate the function for `TypeMod:domain(T)`
gen_domain(Types) when map_size(Types) =:= 0 ->
    {cerl:c_fname(domain, 1),
     cerl:c_fun([cerl:c_var('_')], cerl:c_atom(none))};
gen_domain(Types) ->
    Clauses = [cerl:c_clause([cerl:c_atom(Tag)], Form) || {Tag, Form} <- maps:to_list(Types)],
    Arg = cerl:c_var('_type'),
    Body = cerl:c_case(Arg, Clauses),
    Compacted = cerl:c_call(cerl:c_atom(domain), cerl:c_atom(compact), [Body]),
    FName = cerl:c_fname(domain, 1),
    {FName, cerl:c_fun([Arg], Compacted)}.

gen_type_def(Name, Tag, []) ->
    Body = cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)]),
    FName = cerl:c_fname(Name, 0),
    {FName, cerl:c_fun([], Body)};
gen_type_def(Name, Tag, Args) -> 
    ArgForms = [cerl:c_var(A) || {var, A} <- Args],
    Body = unsafe_call_form(cerl:c_atom(Tag), ArgForms),
    FName = cerl:c_fname(Name, length(Args)),
    {FName, cerl:c_fun(ArgForms, Body)}.

collect_args({ast, _, _, _, Defs} = AST) ->
    TopLevelTypes = maps:from_list([{Name, []} || Name <- maps:keys(Defs)]),
    ast:traverse(fun args_pre/3, fun args_post/3, TopLevelTypes, AST).
collect_types({ast, _, _, _, Defs} = AST) -> 
    Scope = maps:from_list([{[Name], Args} || {type_def, _, Name, Args, _} <- maps:values(Defs)]),
    ast:traverse(fun types_pre/3, fun types_post/3, Scope, AST).


% args_pre only tags the pair key to make sure we know it's tag after the key
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


pre_gen(TypesEnv, ArgsEnv) -> fun(Type, Scope, Term) -> pre_gen_term(TypesEnv, ArgsEnv, Type, Scope, Term) end.
pre_gen_term(_, _, top_level, _, {def, _, _, _, _}) -> skip;
pre_gen_term(_, ArgsEnv, expr, _, {type_application, _, Tag, Args} = Term) ->
    Vars = maps:get(Tag, ArgsEnv),
    case length(Vars) =:= length(Args) of
        true    -> ok;
        false   -> error:format({wrong_number_of_arguments, Tag, length(Args), length(Vars)}, {typegen, expr, Term})
    end;
pre_gen_term(TypesEnv, _, pattern, _, Term)         -> {change, pattern_gen:gen(TypesEnv), Term};
pre_gen_term(_, _, _, _, _)                         -> ok.



% gen_term translates types from kind AST to erlang core AST
gen_term(_, _, _, _, {ast, _, _, _, _}) -> ok;

gen_term(TypesEnv, ArgsEnv, Type, Scope, {type_def, Ctx, Name, ArgList, Clauses}) when is_list(Clauses) ->
    Args = [A || As <- ArgList, A <- As],
    ClauseList = lists:flatten(Clauses),
    AllClauses = case cerl_clauses:any_catchall(ClauseList) of
                     true -> ClauseList;
                     false -> ClauseList ++ [catchall(Args)]
                 end,
    Expr = cerl:c_case(cerl:c_values(Args), AllClauses),
    gen_term(TypesEnv, ArgsEnv, Type, Scope, {type_def, Ctx, Name, ArgList, Expr});

gen_term(_, _, _, _, {type_def, _, Name, ArgList, Expr}) ->
    Args = [A || As <- ArgList, A <- As],
    Form = case lists:flatten(Args) of
               []	-> Expr;
               _	-> gen_f(cerl:c_atom(Name), Args, Expr)
           end,
    {ok, Name, Form};

gen_term(_, ArgsEnv, expr, _, {tagged, _, _, Val} = Term) ->
    Tag = symbol:tag(Term),
    CoreForm = cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), Val]),
    TypeForm = case maps:get(Tag, ArgsEnv) of
                   []       -> CoreForm;
                   Args     -> gen_f(cerl:c_atom(Tag), [cerl:c_var(A) || {var, A} <- Args], CoreForm)
               end,
    {ok, Tag, TypeForm, CoreForm};

gen_term(_, _, expr, _, {sum, _, Elements}) ->
    SumElements = cerl:make_list(Elements),
    DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [SumElements]),
    {ok, cerl:c_tuple([cerl:c_atom(sum), DomainSet])};

gen_term(_, _, expr, _, {dict_pair, _, K, V}) -> {ok, cerl:c_map_pair(K, V)};

gen_term(_, _, expr, _, {dict, _, Elements}) ->
    {ok, cerl:c_tuple([cerl:c_atom(product), cerl:c_map(Elements)])};

gen_term(_, _, expr, _, {application, _, Expr, Args}) -> 
    {ok, unsafe_call_form(Expr, Args)};

gen_term(_, _, expr, _, {type_application, _, Tag, Args} = Term) -> 
    IsRecursive = lists:member(Tag, ast:get_tag(path, Term)),
    case IsRecursive of

        % Normal type function with no recursion
        false -> {ok, unsafe_call_form(cerl:c_atom(Tag), Args)};

        % type recursion e.g.: List a -> Nil | Cons: { head: a, tail: List(a) }
        true  -> 
            BranchFun = cerl:c_fun([], unsafe_call_form(cerl:c_atom(Tag), Args)),
            {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])}
    end;

gen_term(TypesEnv, ArgsEnv, expr, _, {type, _, _, Path} = Term) ->
    Tag = symbol:tag(Term),
    IsRecursive = lists:member(Tag, ast:get_tag(path, Term)),
    IsDefined = maps:is_key(Path, TypesEnv),
    case {IsRecursive, IsDefined} of
        {true, _}       -> BranchFun = cerl:c_fun([], cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)])),
                           {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};
        {_, true}       -> 
            GenTermF = fun(Type, Scope, T) -> gen_term(TypesEnv, ArgsEnv, Type, Scope, T) end,
            TraverseF = fun(T) -> error:map(ast:traverse_term(expr, pre_gen(TypesEnv, ArgsEnv), GenTermF, #{}, T),
                                            fun({_, Forms}) -> Forms end) end,
            case maps:get(Path, TypesEnv) of
                {tagged, _, _, T}          -> TraverseF(T);
                {type_def, _, _, _, _} = T -> TraverseF(T);
                {type, _, _, _} = T        -> {ok, symbol:tag(T), cerl:c_atom(symbol:tag(T))}
            end;

        % type constant from different module
        {false, false}  -> {ok, cerl:c_atom(Tag)}
    end;

gen_term(TypesEnv, ArgsEnv, expr, Scope, {qualified_type, Ctx, ModulePath, Name}) ->
    ModuleName = module:beam_name(ModulePath),
    case erlang:function_exported(ModuleName, Name, 0) of
        false   -> error:format({undefined_type_in_pattern, Name}, {typegen, Ctx});
        true    ->
            Domain = utils:domain_to_term(ModuleName:Name(), Ctx),
            GenTermF = fun(Type, LocalScope, T) -> gen_term(TypesEnv, ArgsEnv, Type, LocalScope, T) end,
            error:map(ast:traverse_term(expr, pre_gen(TypesEnv, ArgsEnv), GenTermF, Scope, Domain),
                      fun({_, Forms}) -> Forms end)
    end;

gen_term(_, _, expr, _, {key, _, _} = Term) -> {ok, cerl:c_atom(symbol:tag(Term))};

gen_term(_, _, expr, _, {variable, _, _, _} = Term) -> {ok, cerl:c_var(symbol:tag(Term))};

% Type Atom
gen_term(_, _, expr, _, Atom) when is_atom(Atom) -> {ok, cerl:c_atom(Atom)};

gen_term(_, _, expr, _, {clause, _, Patterns, Expr}) ->
    io:format("Patterns: ~p~n", [Patterns]),
    {ok, [cerl:c_clause(Ps, Expr) || Ps <- utils:combinations(Patterns)]}.

gen_f(Tag, Args, Expr) -> cerl:c_tuple([cerl:c_atom(f), Tag, cerl:c_fun(Args, Expr)]).

unsafe_call_form(NameForm, ArgForms) ->
    cerl:c_apply(
      cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), 
                  [cerl:c_int(3), cerl:c_apply(cerl:c_fname(domain, 1), [NameForm])]),
      ArgForms).

catchall(Args) ->
    Error = cerl:c_tuple([cerl:c_atom(error),
                          cerl:make_list(
                            [cerl:c_tuple(
                               [cerl:c_tuple(
                                  [cerl:c_atom(no_matching_pattern),
                                   cerl:make_list(Args)]),
                                cerl:c_tuple([cerl:c_atom(no_context)])])])]),
    cerl:c_clause([cerl:c_var(symbol:id('_')) || _ <- lists:seq(1, length(Args))], Error).
