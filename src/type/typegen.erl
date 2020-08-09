-module(typegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").

gen(AST) ->
    error:flatmap2(collect_args(AST),
                   collect_types(AST),
                   fun({ArgsEnv, _}, {TypesEnv, TypedAST}) ->
                           case gen_types(TypesEnv, ArgsEnv, TypedAST) of
                               {error, Errs}    -> {error, Errs};
                               {ok, {Types, _}} -> {ok, gen_modules(Types, TypesEnv, ArgsEnv, TypedAST)}
                           end
                   end).

gen_types(TypesEnv, ArgsEnv, AST) -> 
    Post = fun(Type, Scope, Term) -> gen_term(TypesEnv, ArgsEnv, Type, Scope, Term) end,
    ast:traverse(fun pre_gen_term/3, Post, AST).

gen_modules(Types, TypesEnv, ArgsEnv, {ast, _, Modules, _, _} = AST) ->

    % Create a map from types to type members
    F = fun([Parent | _] = Path, {tagged, _, _, _} = Term) ->
                Tag = symbol:tag(Term),
                Name = lists:last(Path),
                Args = maps:get(Tag, ArgsEnv),
                Form = maps:get(Tag, Types),
                {Parent, {Name, Args, Form}};
           ([Parent | _], {type, _, Name, _} = T) ->
                Tag = symbol:tag(T),
                {Parent, {Name, [], cerl:c_atom(Tag)}}
        end,
    TypePaths = [F(Path, Term) || {Path, Term} <- maps:to_list(TypesEnv), length(Path) > 1],
    K = fun({K, _}) -> K end,
    V = fun({_, V}) -> V end,
    TypeParents = utils:group_by(K, V, TypePaths),

    % Inside a type module for a specific type we might refer to other types
    % created in the parent module. For this reason we include a domain
    % function of all module types within each type module
    DomainDef = gen_domain(Types),

    TypeModules = [gen_type_modules(Modules, Parent, Children, DomainDef) || {Parent, Children} <- TypeParents],
    ScannerModule = gen_scanner_module(Types, AST),

    [ScannerModule | lists:flatten(TypeModules)].


% For each module in the AST we create a type module which exports the children of the type
gen_type_modules(_, _, [], _) -> [];
gen_type_modules(Modules, Parent, Children, DomainDef) ->
    ModuleNames = [module:beam_name(Path ++ [Parent]) || {module, _, Path, Exports} <- Modules,
                                                         maps:is_key(Parent, Exports)],
    Defs = [gen_type_module_def(Name, Args, Form) || {Name, Args, Form} <- Children],
    {Exports, _} = lists:unzip(Defs),
    [{ModuleName, cerl:c_module(cerl:c_atom(ModuleName), Exports, [], [DomainDef | Defs])} ||
     ModuleName <- ModuleNames].


gen_type_module_def(Name, Args, Form) ->
    ArgForms = [cerl:c_var(A) || {var, A} <- Args],
    FName = cerl:c_fname(Name, length(Args)),
    case length(Args) =:= 0 of
        true    -> {FName, cerl:c_fun(ArgForms, Form)};
        false   -> 
            % type is wrapped in {f, Name, fun}
            [_, _, Unpacked] = cerl:tuple_es(Form),
            {FName, Unpacked}
    end.



    
gen_scanner_module(Types, {ast, _, _, _, Defs}) ->
    ModuleName = symbol:id(types),
    DomainDef = gen_domain(Types),

    % Top level defs `TypeMod:T()` for `T` in `type T -> ...`
    TopLevelDefs = [gen_type_def(Name, Args) || {type_def, _, Name, Args, _} <- maps:values(Defs)],

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

gen_type_def(Name, []) ->
    Body = cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Name)]),
    FName = cerl:c_fname(Name, 0),
    {FName, cerl:c_fun([], Body)};
gen_type_def(Name, Args) -> 
    ArgForms = [cerl:c_var(symbol:tag(A)) || A <- Args],
    Body = unsafe_call_form(cerl:c_atom(Name), ArgForms),
    FName = cerl:c_fname(Name, length(Args)),
    {FName, cerl:c_fun(ArgForms, Body)}.

collect_args(AST) -> ast:traverse(fun args_pre/3, fun args_post/3, AST).
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
args_post(expr, _, {type, _, _, _}) -> {ok, []};
args_post(expr, _, {key, _, _}) -> {ok, []};
args_post(expr, _, {pair, _, _, Val} = Term) -> 
    case ast:get_tag(tag, Term, undefined) of
        undefined -> {ok, get_vars(Term)};
        Tag -> {ok, Tag, Val}
    end;
args_post(top_level, _, {type_def, _, Name, Args, _}) -> {ok, Name, [{var, A} || A <- Args]};
args_post(pattern, _, _) -> skip;
args_post(_, _, Term) when is_tuple(Term) -> 
    {ok, get_vars(Term)}.

get_vars(Term) when is_tuple(Term) ->
    Args = [{var, V} || I <- lists:seq(3, size(Term)), {var, V} <- lists:flatten([element(I, Term)])],
    utils:unique(Args).




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


pre_gen_term(top_level, _, {def, _, _, _, _}) -> skip;
pre_gen_term(_, _, _) -> ok.



% gen_term translates types from kind AST to erlang core AST
gen_term(_, _, _, _, {ast, _, _, _, _}) -> ok;
gen_term(_, _, pattern, _, {type_def, _, _, [], Expr}) -> {ok, Expr};

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

gen_term(_, _, expr, _, {tuple, _, Elements}) ->
    SumElements = cerl:make_list(Elements),
    DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [SumElements]),
    {ok, cerl:c_tuple([cerl:c_atom(sum), DomainSet])};

gen_term(_, _, expr, _, {dict_pair, _, K, V}) -> {ok, cerl:c_map_pair(K, V)};

gen_term(_, _, expr, _, {dict, _, Elements}) ->
    {ok, cerl:c_tuple([cerl:c_atom(product), cerl:c_map(Elements)])};

gen_term(_, _, expr, _, {application, _, Expr, Args}) -> 
    {ok, unsafe_call_form(Expr, Args)};

gen_term(_, ArgsEnv, expr, _, {type_application, _, Tag, Args} = Term) -> 
    IsRecursive = lists:member(Tag, ast:get_tag(path, Term)),
    Vars = maps:get(Tag, ArgsEnv),
    case IsRecursive of

        % Normal type function with no recursion
        false when length(Vars) =:= length(Args) -> 
            {ok, unsafe_call_form(cerl:c_atom(Tag), Args)};

        % type recursion e.g.: List a -> Nil | Cons: { head: a, tail: List(a) }
        true when length(Vars) =:= length(Args) -> 
            BranchFun = cerl:c_fun([], unsafe_call_form(cerl:c_atom(Tag), Args)),
            {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

        % The number of variables looks off
        _ ->                     
            error:format({wrong_number_of_arguments, Tag, length(Args), length(Vars)}, {typegen, expr, Term})
    end;

gen_term(TypesEnv, ArgsEnv, expr, _, {type, _, _, Path} = Term) ->
    Tag = symbol:tag(Term),
    IsRecursive = lists:member(Tag, Path),
    IsDefined = maps:is_key(Path, TypesEnv),
    case {IsRecursive, IsDefined} of
        {true, _}   -> BranchFun = cerl:c_fun([], cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)])),
                       {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};
        {_, true}   -> 
            GenTermF = fun(Type, Scope, T) -> gen_term(TypesEnv, ArgsEnv, Type, Scope, T) end,
            TraverseF = fun(T) -> error:map(ast:traverse_term(expr, fun pre_gen_term/3, GenTermF, #{}, T),
                                            fun({_, Forms}) -> Forms end) end,
            case maps:get(Path, TypesEnv) of
                {tagged, _, _, T}          -> TraverseF(T);
                {type_def, _, _, _, _} = T -> TraverseF(T);
                {type, _, _, _} = T        -> {ok, cerl:c_atom(symbol:tag(T))}
            end
    end;

gen_term(_, _, expr, _, {key, _, _} = Term) -> {ok, cerl:c_atom(symbol:tag(Term))};

gen_term(_, _, expr, _, {variable, _, _, _} = Term) -> {ok, cerl:c_var(symbol:tag(Term))};

gen_term(_, _, expr, _, {clause, _, Patterns, Expr}) ->
    {ok, [cerl:c_clause(Ps, Expr) || Ps <- combinations(Patterns)]};

% Pattern of shape: a
gen_term(_, _, pattern, _, {variable, _, _, _} = Term) -> {ok, [cerl:c_var(symbol:tag(Term))]};

% Pattern of shape: T
gen_term(TypesEnv, ArgsEnv, pattern, _, {type, _, _, Path} = Term) ->
    Tag = symbol:tag(Term),
    NewEnv = maps:remove(Tag, TypesEnv),
    GenTermF = fun(Type, Scope, T) -> gen_term(NewEnv, ArgsEnv, Type, Scope, T) end,
    TraverseF = fun(T) -> error:map(ast:traverse_term(pattern, fun pre_gen_term/3, GenTermF, #{}, T),
                                    fun({_, Forms}) -> Forms end) end,
    case maps:get(Path, TypesEnv, undefined) of
        undefined                  -> {ok, [cerl:c_atom(Tag)]};
        {type, _, _, _}            -> {ok, [cerl:c_atom(Tag)]};
        {tagged, _, _, T}          -> TraverseF(T);
        {type_def, _, _, _, _} = T -> TraverseF(T)
    end;

% Key like 'k' in '{k: a}'
gen_term(_, _, pattern, _, {key, _, _} = Term) -> {ok, [cerl:c_atom(symbol:tag(Term))]};

% Pattern like '{a, k: b}'
gen_term(_, _, pattern, _, {dict, _, ElemList}) ->
    {ok, [cerl:c_tuple([cerl:c_atom(product), cerl:c_map_pattern(Elems)]) || 
          Elems <- combinations(ElemList)]};

% Pattern of variable or pair inside product
gen_term(_, _, pattern, _, {dict_pair, _, Keys, Vals}) ->
    {ok, [cerl:c_map_pair_exact(K, V) || K <- Keys, V <- Vals]};

% Pattern of shape: 'a: T' or 'T {a, B: T}'
% (the latter is a lookup, but get translated to a pair before reaching this
% state of the typegen
gen_term(_, _, pattern, _, {pair, _, Keys, Vals}) ->
    {ok, [cerl:c_alias(K, V) || K <- Keys, V <- Vals]};

% Pattern of shape: 'T: S'
gen_term(_, _, pattern, _, {tagged, _, _, Vals} = Term) ->
    Tag = symbol:tag(Term),
    {ok, [cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), V]) || V <- Vals]};

% Pattern of shape: 'A | B'
gen_term(_, _, pattern, _, {tuple, _, ElemList}) ->
    {ok, [E || Elems <- combinations(ElemList), E <- Elems]}.


gen_f(Tag, Args, Expr) -> cerl:c_tuple([cerl:c_atom(f), Tag, cerl:c_fun(Args, Expr)]).

unsafe_call_form(NameForm, ArgForms) ->
    cerl:c_apply(
      cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), 
                  [cerl:c_int(3), cerl:c_apply(cerl:c_fname(domain, 1), [NameForm])]),
      ArgForms).

combinations(L) -> 
    Rs = lists:foldl(fun(Es, Accs) -> [[E | Acc] || E <- Es, Acc <- Accs] end, [[]], L),
    [lists:reverse(R) || R <- Rs].

catchall(Args) ->
    Error = cerl:c_tuple([cerl:c_atom(error),
                          cerl:make_list(
                            [cerl:c_tuple(
                               [cerl:c_tuple(
                                  [cerl:c_atom(no_matching_pattern),
                                   cerl:make_list(Args)]),
                                cerl:c_tuple([cerl:c_atom(no_context)])])])]),
    cerl:c_clause([cerl:c_var(symbol:id('_')) || _ <- lists:seq(1, length(Args))], Error).
