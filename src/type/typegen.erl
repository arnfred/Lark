-module(typegen).
-export([gen/2]).

-include_lib("eunit/include/eunit.hrl").

gen(Module, AST) when is_list(AST) ->
    error:flatmap2(collect_args(AST),
                   collect_types(AST),
                   fun({ArgsEnv, _}, {TypesEnv, TypedAST}) ->
                           io:format("TypesEnv: ~p~n", [TypesEnv]),
                           case gen_types(TypesEnv, ArgsEnv, TypedAST) of
                               {error, Errs}    -> {error, Errs};
                               {ok, {Types, _}} -> io:format("Types: ~p~n", [Types]),
                                                   gen_module(Module, Types, TypedAST)
                           end
                   end).

gen_types(TypesEnv, ArgsEnv, AST) -> 
    Post = fun(Type, Scope, Term) -> gen_term(TypesEnv, ArgsEnv, Type, Scope, Term) end,
    ast:traverse(fun pre_gen_term/3, Post, AST).

% Generate the function for `TypeMod:domain(T)`
gen_module(Module, Types, AST) ->
    DomainDef = gen_domain(Types),

    % Top level defs `TypeMod:T()` for `T` in `type T -> ...`
    TopLevelDefs = [gen_type_def(Name, Args) || {type_def, _, Name, Args, _} <- AST],

    {Exports, _} = lists:unzip([DomainDef | TopLevelDefs]),
    {ok, cerl:c_module(cerl:c_atom(Module), Exports, [], [DomainDef | TopLevelDefs])}.

gen_domain(Types) when map_size(Types) =:= 0 ->
    {cerl:c_fname(domain, 1),
     cerl:c_fun([cerl:c_var('_')], cerl:c_atom(none))};
gen_domain(Types) ->
    Clauses = [cerl:c_clause([cerl:c_atom(Tag)], Form) || {Tag, Form} <- maps:to_list(Types)],
    Arg = cerl:c_var('_type'),
    Body = cerl:c_case(Arg, Clauses),
    Compacted = cerl:c_call(cerl:c_atom(domain), cerl:c_atom(compact), [Body]),
    Name = cerl:c_fname(domain, 1),
    {Name, cerl:c_fun([Arg], Compacted)}.

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
collect_types(AST) -> ast:traverse(fun types_pre/3, fun types_post/3, AST).


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
    unique(Args).




% types_pre adds a `path` tag to the term contexts and checks a few
% assumptions about the type tree. It also renames `pairs` inside of a
% dict to `dict_pair` to make it easier to generate erlang core afterwards
types_pre(top_level, _, {def, _, _, _, _})  -> skip;
types_pre(top_level, _, {type_def, _, Name, _, _} = Term) -> {ok, ast:tag(path, Term, [Name])};
types_pre(_, _, {pair, Ctx, {type, _, _, _} = T, Val}) -> 
    F = fun(Tag) -> [symbol:tag(T) | Tag] end,
    Tagged = ast:tag(path, Val, F, []),
    {ok, {tagged, Ctx, symbol:tag(T), Tagged}};
types_pre(Type, _, {dict, Ctx, Elements} = Term) ->
    F = fun({pair, X, {key, _, _} = K, V})  -> {ok, {dict_pair, X, K, V}};
           ({variable, X, Name, _} = Val)   -> {ok, {dict_pair, X, {key, X, Name}, Val}};
           ({pair, _, K, _})                -> error:format({unrecognized_tag_type, K}, {typegen, Type, Term});
           (Elem)                           -> error:format({unrecognized_product_elem, Elem}, {typegen, Type, Term}) end,
    error:map(error:collect([F(Elem) || Elem <- Elements]),
              fun(TaggedElements) -> ast:tag(path, {dict, Ctx, TaggedElements}) end);
types_pre(pattern, _, {lookup, Ctx, {type, _, _, _} = T, Elements}) ->
    {ok, ast:tag(path, {tagged, Ctx, symbol:tag(T), {dict, Ctx, Elements}})};
types_pre(Type, _, {lookup, _, T, _} = Term) ->
    error:format({unrecognized_lookup_type, T}, {typegen, Type, Term});
types_pre(pattern, _, {application, _, Expr, Args} = Term) ->
    error:format({pattern_application, Expr, Args}, {typegen, pattern, Term});
types_pre(_, _, {application, Ctx, {type, _, _, _} = T, Args}) ->
    io:format("type application: ~p~n", [symbol:tag(T)]),
    {ok, ast:tag(path, {type_application, Ctx, symbol:tag(T), Args})};
types_pre(_, _, Term) -> {ok, ast:tag(path, Term)}.


% types_post collects all types defined by the type definitions in the AST.
% This includes the type defs and any tags
types_post(top_level, _, {type_def, _, Name, _, _} = Term) ->
    {ok, Name, Term};
types_post(expr, _, {tagged, _, Tag, _} = Term) ->
    {ok, Tag, Term};
types_post(_, _, _) -> ok.


pre_gen_term(top_level, _, {def, _, _, _, _}) -> skip;
pre_gen_term(_, _, _) -> ok.


% gen_term translates types from kind AST to erlang core AST
gen_term(_, _, pattern, _, {type_def, _, _, [], Expr}) -> {ok, Expr};

gen_term(TypesEnv, ArgsEnv, Type, Scope, {type_def, Ctx, Name, ArgList, Clauses}) when is_list(Clauses) ->
    Args = [A || As <- ArgList, A <- As],
    io:format("TypeDef with Clauses Args: ~p~n", [Args]),
    ClauseList = lists:flatten(Clauses),
    AllClauses = case cerl_clauses:any_catchall(ClauseList) of
                     true -> ClauseList;
                     false -> ClauseList ++ [catchall(Args)]
                 end,
    Expr = cerl:c_case(cerl:c_values(Args), AllClauses),
    gen_term(TypesEnv, ArgsEnv, Type, Scope, {type_def, Ctx, Name, ArgList, Expr});

gen_term(_, _, _, _, {type_def, _, Name, ArgList, Expr}) ->
    Args = [A || As <- ArgList, A <- As],
    io:format("Type def Args: ~p~n", [Args]),
    Form = case lists:flatten(Args) of
                   []	-> Expr;
                   _	-> gen_f(cerl:c_atom(Name), Args, Expr)
               end,
    {ok, Name, Form};

gen_term(_, ArgsEnv, expr, _, {tagged, _, Tag, Val}) ->
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
    IsRecursive = lists:member(Tag, path(Term)),
    Vars = maps:get(Tag, ArgsEnv),
    io:format("Is ~p member of ~p? -- ~p~n", [Tag, path(Term), IsRecursive]),
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

gen_term(TypesEnv, ArgsEnv, expr, _, {type, _, _, _} = Term) ->
    Tag = symbol:tag(Term),
    IsRecursive = lists:member(Tag, path(Term)),
    IsDefined = maps:is_key(Tag, TypesEnv),
    case {IsRecursive, IsDefined} of
        {true, _}   -> BranchFun = cerl:c_fun([], cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)])),
                       {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};
        {_, true}   -> Domain = case maps:get(Tag, TypesEnv) of
                                    {tagged, _, _, D} -> D;
                                    D -> D
                                end,
                       io:format("Type Domain: ~p~n", [Domain]),
                       Gen = fun(Type, Scope, T) -> gen_term(TypesEnv, ArgsEnv, Type, Scope, T) end,
                       error:map(ast:traverse(expr, fun pre_gen_term/3, Gen, #{}, Domain),
                                 fun({_, Forms}) -> Forms end);
        _           -> {ok, Tag, cerl:c_atom(Tag)}
    end;

gen_term(_, _, expr, _, {key, _, _} = Term) -> {ok, cerl:c_atom(symbol:tag(Term))};

gen_term(_, _, expr, _, {variable, _, _, _} = Term) -> {ok, cerl:c_var(symbol:tag(Term))};

gen_term(_, _, expr, _, {clause, _, Patterns, Expr}) ->
    {ok, [cerl:c_clause(Ps, Expr) || Ps <- combinations(Patterns)]};

% Pattern of shape: a
gen_term(_, _, pattern, _, {variable, _, _, _} = Term) -> {ok, [cerl:c_var(symbol:tag(Term))]};

% Pattern of shape: T
gen_term(TypesEnv, ArgsEnv, pattern, _, {type, _, _, _} = Term) ->
    Tag = symbol:tag(Term),
    io:format("Tag of type pattern: ~p~n", [Tag]),
    case maps:is_key(Tag, TypesEnv) of
        false   -> {ok, [cerl:c_atom(Tag)]};
        true    -> Env = maps:remove(Tag, TypesEnv),
                   Gen = fun(Type, Scope, T) -> gen_term(Env, ArgsEnv, Type, Scope, T) end,
                   Domain = case maps:get(Tag, TypesEnv) of
                                {tagged, _, _, D} -> D;
                                D -> D
                            end,
                   io:format("Type Domain: ~p~n", [Domain]),
                   error:map(ast:traverse(pattern, fun pre_gen_term/3, Gen, #{}, Domain),
                             fun({_, Forms}) -> Forms end)
    end;

% Key like 'k' in '{k: a}'
gen_term(_, _, pattern, _, {key, _, _} = Term) -> {ok, [cerl:c_atom(symbol:tag(Term))]};

% Pattern like '{a, k: b}'
gen_term(_, _, pattern, _, {dict, _, ElemList}) ->
    {ok, [cerl:c_tuple([cerl:c_atom(product), cerl:c_map_pattern(Elems)]) || 
          Elems <- combinations(ElemList)]};

% Pattern of variable or pair inside product
gen_term(_, _, pattern, _, {dict_pair, _, Keys, Vals}) ->
    io:format("Dict pair: Key: ~p, Val: ~p~n", [Keys, Vals]),
    {ok, [cerl:c_map_pair_exact(K, V) || K <- Keys, V <- Vals]};

% Pattern of shape: 'a: T' or 'T {a, B: T}'
% (the latter is a lookup, but get translated to a pair before reaching this
% state of the typegen
gen_term(_, _, pattern, _, {pair, _, Keys, Vals} = Term) ->
    io:format("Pair with Key in Context? ~p~n", [Term]),
    {ok, [cerl:c_alias(K, V) || K <- Keys, V <- Vals]};

% Pattern of shape: 'T: S'
gen_term(_, _, pattern, _, {tagged, _, Tag, Vals}) ->
    {ok, [cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), V]) || V <- Vals]};

% Pattern of shape: 'A | B'
gen_term(_, _, pattern, _, {tuple, _, ElemList}) ->
    {ok, [E || Elems <- combinations(ElemList), E <- Elems]}.


gen_f(Tag, Args, Expr) -> cerl:c_tuple([cerl:c_atom(f), Tag, cerl:c_fun(Args, Expr)]).

path(Term) -> ast:get_tag(path, Term).

unsafe_call_form(NameForm, ArgForms) ->
    cerl:c_apply(
      cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), 
                  [cerl:c_int(3), cerl:c_apply(cerl:c_fname(domain, 1), [NameForm])]),
      ArgForms).



unique(L) -> 
    {Out, _} = lists:foldl(fun(Elem, {Out, Seen}) -> 
                                      case ordsets:is_element(Elem, Seen) of
                                          true -> {Out, Seen};
                                          false -> {[Elem | Out], ordsets:add_element(Elem, Seen)}
                                      end end, {[], ordsets:new()}, L),
    lists:reverse(Out).


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
