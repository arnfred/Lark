-module(expr_gen).
-export([gen_expr/2]).

gen_expr(_, {def, _, Name, Expr}) ->
    case cerl:is_c_fun(Expr) of
        true    -> FName = cerl:c_fname(Name, cerl:fun_arity(Expr)),
                   {ok, Name, {FName, Expr}};
        false   -> FName = cerl:c_fname(Name, 0),
                   {ok, Name, {FName, cerl:c_fun([], Expr)}}
    end;

% expr of form: A | B | C
gen_expr(_, {sum, _, Elements}) ->
    SumElements = cerl:make_list(Elements),
    DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [SumElements]),
    Sum = cerl:c_tuple([cerl:c_atom(sum), DomainSet]),
    Normalized = cerl:c_call(cerl:c_atom(domain), cerl:c_atom(normalize), [Sum]),
    {ok, Normalized};

% expr of form: [1, 2, 3]
gen_expr(_, {list, _, Elements}) ->
    {ok, cerl:make_list(Elements)};

% expr of form: #{1, 2, 3}
gen_expr(_, {tuple, _, Elements}) ->
    {ok, cerl:c_tuple(Elements)};

% the pair of `k: a` in the form {k: a, ...}
gen_expr(_, {pair, _, {keyword, _, K}, V}) -> {ok, cerl:c_map_pair(cerl:c_atom(K), V)};

% the pair of `k: a` outside a dictionary
gen_expr(_, {pair, _, K, _}) -> {ok, K};

% the key `k` in {k: a}
gen_expr(_, {keyword, _, _}) -> skip;

% TODO: What does a dictionary value look like? -- Two approaches:
% 1. A dictionary is a fixed datatype (like javascript)
% 2. a dictionary is an interface that different datatypes can implement to inherit the dict syntax
% With the latter approach, it's not trivial to figure out what datatype a
% dictionary should resolve to.  Fundamentally this will depend on the domain
% of the value, but there's no guarantee that the domain at the point of
% instantiation can be narrowed down to a single type.

% literal of form: { ... }
gen_expr(_, {dict, _, Elements}) ->
    {ok, cerl:c_map(Elements)};

% expr of form: `f(a)` or `a.T`
gen_expr(_, {application, _, Expr, Args}) ->
    case cerl:is_c_fun(Expr) of
        true    -> {ok, cerl:c_apply(Expr, Args)};
        false   ->
            case cerl:is_c_fname(Expr) of
                true    -> {ok, cerl:c_apply(cerl:c_fname(cerl:fname_id(Expr), length(Args)), Args)};
                false   -> {ok, cerl:c_apply(Expr, Args)}
            end
    end;

% expr of form: `T(a)` where `T` is a recursive type (and so we want to not
% call the domain function in an infinite loop
gen_expr(_, {recursive_type_application, _, Tag, Args}) ->
    BranchFun = cerl:c_fun([], call_type_tag(Tag, Args)),
    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

% expr of Form: lark/prelude/match(a)
gen_expr(_, {qualified_application, _, ModulePath, Name, Args}) ->
    {ok, cerl:c_apply(cerl:c_fname(symbol:tag(ModulePath ++ [Name]), length(Args)), Args)};

% expr of Form: beam/lists/reverse(a)
gen_expr(_, {beam_application, _, ModulePath, Name, Args}) ->
    ModuleName = module:beam_name(ModulePath),
    {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), Args)};

% expr of form: T
gen_expr(Scope, {type, _, _, _} = Term) ->
    Tag = symbol:tag(Term),
    case maps:get(Tag, Scope, undefined) of
        undefined               -> {ok, cerl:c_atom(Tag)};
        T                       -> {ok, T}
    end;

% expr of form: lark/prelude/Boolean or lark/prelude/match
gen_expr(_, {qualified_symbol, _, ModulePath, Name}) ->

    % If the module function doesn't exist with zero arguments, try calling the domain function
    ModuleName = module:beam_name(ModulePath),
    case erlang:function_exported(ModuleName, Name, 0) of
        true    -> {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), [])};
        false   -> {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(domain), [cerl:c_atom(Name)])}
    end;

% expr of form `T` where T is recursive like `type T -> {a: Boolean, b: T}`
gen_expr(_, {recursive_type, _, _, _} = Term) ->
    Tag = symbol:tag(Term),
    BranchFun = cerl:c_fun([], domain(Tag)),
    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

% expr of form: `k` in {k: val}
gen_expr(_, {key, _, _} = Term) -> {ok, cerl:c_atom(symbol:tag(Term))};

% expr of form: a
gen_expr(_, {variable, _, _, _} = Term) -> {ok, cerl:c_var(symbol:tag(Term))};

% type atom
gen_expr(_, Atom) when is_atom(Atom) -> {ok, cerl:c_atom(Atom)};

% expr of form: ` | <Pattern> -> <Expr>`
gen_expr(_, {clause, _, Patterns, Expr}) ->
    {ok, cerl:c_clause(Patterns, Expr)};

% expr of form `(<Pattern> -> <Expr>)` which encodes an anonymous function
gen_expr(_, {'fun', Ctx, Clauses}) ->
    [Clause | _] = Clauses,
    Args = [cerl:c_var(S) || _ <- lists:seq(1, cerl:clause_arity(Clause)),
                             S <- [symbol:id('')]],
    {ok, cerl:c_fun(Args, cerl:c_case(cerl:c_values(Args), Clauses ++ [catchall(Args, Ctx)]))};

% expr of form `(<Pattern> = Expr, Other expression)`
gen_expr(_, {'let', Ctx, Expr, Clauses}) ->
    {ok, cerl:c_case(Expr, Clauses ++ [catchall([Expr], Ctx)])};

% exor of form `(Expr1(), Expr2)`
gen_expr(_, {seq, _, Expr, Acc}) -> {ok, cerl:c_seq(Expr, Acc)};

% expr of form: `"test string"` or `1.32`
gen_expr(_, {value, _, _, Val}) -> {ok, cerl:abstract(Val)}.

catchall(Args, Ctx) ->
    Error = cerl:c_tuple([cerl:c_atom(error),
                          cerl:make_list(
                            [cerl:c_tuple(
                               [cerl:c_tuple(
                                  [cerl:c_atom(no_matching_pattern),
                                   cerl:make_list(Args)]),
                                cerl:c_tuple([cerl:c_atom(expr_gen), cerl:abstract(Ctx)])])])]),
    cerl:c_clause([cerl:c_var(symbol:id('_')) || _ <- lists:seq(1, length(Args))], Error).

call_type_domain(ExprForm, ArgForms) -> cerl:c_apply(ExprForm, ArgForms).

domain(Tag) -> cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)]).

call_type_tag(Tag, ArgForms) ->
    call_type_domain(domain(Tag), ArgForms).
