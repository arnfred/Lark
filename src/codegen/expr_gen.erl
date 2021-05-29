-module(expr_gen).
-export([gen_expr/3, call_type_tag/2]).

gen_expr(expr, _, {def, _, Name, Expr}) ->
    case cerl:is_c_fun(Expr) of
        true    -> FName = cerl:c_fname(Name, cerl:fun_arity(Expr)),
                   {ok, Name, {FName, Expr}};
        false   -> FName = cerl:c_fname(Name, 0),
                   {ok, Name, {FName, cerl:c_fun([], Expr)}}
    end;

gen_expr(expr, _, {type_def, _, Name, Expr}) ->
    case cerl:is_c_fun(Expr) of
        true    -> FName = cerl:c_fname(Name, cerl:fun_arity(Expr)),
                   {ok, Name, {FName, Expr}};
        false   -> FName = cerl:c_fname(Name, 0),
                   {ok, Name, {FName, cerl:c_fun([], Expr)}}
    end;

% type expr of form: T: ...
gen_expr(expr, _, {tagged, Ctx, _, Val} = Term) ->
    Tag = symbol:tag(Term),
    CoreForm = cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), Val]),
    TypeForm = case maps:get(args, Ctx) of
                   []       -> CoreForm;
                   Args     -> cerl:c_fun([cerl:c_var(A) || {var, A} <- Args], CoreForm)
               end,
    {ok, Tag, TypeForm, CoreForm};

% expr of form: A | B | C
gen_expr(expr, _, {sum, _, Elements}) ->
    SumElements = cerl:make_list(Elements),
    DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [SumElements]),
    {ok, cerl:c_tuple([cerl:c_atom(sum), DomainSet])};

% expr of form: [1, 2, 3]
gen_expr(expr, _, {list, _, Elements}) ->
    {ok, cerl:make_list(Elements)};

% the pair of `k: a` in the form {k: a, ...}
gen_expr(expr, _, {dict_pair, _, K, V}) -> {ok, cerl:c_map_pair(K, V)};

% the pair of `k: a` outside a dictionary
gen_expr(expr, _, {pair, _, K, _}) -> {ok, K};

% TODO: What does a dictionary value look like? -- Two approaches:
% 1. A dictionary is a fixed datatype (like javascript)
% 2. a dictionary is an interface that different datatypes can implement to inherit the dict syntax
% With the latter approach, it's not trivial to figure out what datatype a
% dictionary should resolve to.  Fundamentally this will depend on the domain
% of the value, but there's no guarantee that the domain at the point of
% instantiation can be narrowed down to a single type.

% literal of form: { ... }
gen_expr(expr, _, {dict, _, Elements}) ->
    {ok, cerl:c_map(Elements)};

% expr of form: `f(a)` or `T(a)`
gen_expr(expr, _, {application, _, Expr, Args}) -> 
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
gen_expr(expr, _, {recursive_type_application, _, Tag, Args}) -> 
    BranchFun = cerl:c_fun([], call_type_tag(Tag, Args)),
    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

% expr of Form: kind/prelude/match(a)
gen_expr(expr, _, {qualified_application, _, ModulePath, Name, Args}) -> 
    ModuleName = module:beam_name(ModulePath),
    {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), Args)};

% expr of Form: beam/lists/reverse(a)
gen_expr(expr, _, {beam_application, _, ModulePath, Name, Args}) -> 
    ModuleName = module:beam_name(ModulePath),
    {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), Args)};

% expr of form: T
gen_expr(expr, Scope, {type, _, _, Path} = Term) ->
    Tag = symbol:tag(Term),
    case maps:get(Tag, Scope, undefined) of
        undefined               -> {ok, cerl:c_atom(Tag)};
        T                       -> {ok, T}
    end;

% expr of form: kind/prelude/Boolean or kind/prelude/match
gen_expr(expr, _, {qualified_symbol, _, ModulePath, Name}) ->
    % If the module function doesn't exist with zero arguments, try calling the domain function
    ModuleName = module:beam_name(ModulePath),
    case erlang:function_exported(ModuleName, Name, 0) of
        true    -> {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), [])};
        false   -> {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(domain), [cerl:c_atom(Name)])}
    end;

% expr of form `T` where T is recursive like `type T -> {a: Boolean, b: T}` 
gen_expr(expr, _, {recursive_type, _, _, _} = Term) ->
    Tag = symbol:tag(Term),
    BranchFun = cerl:c_fun([], domain(Tag)),
    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

% expr of form: `k` in {k: val}
gen_expr(expr, _, {key, _, _} = Term) -> {ok, cerl:c_atom(symbol:tag(Term))};

% expr of form: a
gen_expr(expr, _, {variable, _, _, _} = Term) -> {ok, cerl:c_var(symbol:tag(Term))};

% type atom
gen_expr(expr, _, Atom) when is_atom(Atom) -> {ok, cerl:c_atom(Atom)};

% expr of form: ` | <Pattern> -> <Expr>`
gen_expr(expr, _, {clause, _, Patterns, Expr}) ->
    {ok, [cerl:c_clause(Ps, Expr) || Ps <- utils:combinations(Patterns)]};

% expr of form `(<Pattern> -> <Expr>)` which encodes an anonymous function
gen_expr(expr, _, {'fun', Ctx, ClauseList}) ->
    ([Clause | _] = Clauses) = lists:flatten(ClauseList),
    Args = [cerl:c_var(S) || _ <- lists:seq(1, cerl:clause_arity(Clause)),
                             S <- [symbol:id('')]],
    {ok, cerl:c_fun(Args, cerl:c_case(cerl:c_values(Args), Clauses ++ [catchall(Args, Ctx)]))};

% expr of form `(<Pattern> = Expr, Other expression)`
gen_expr(expr, _, {'let', _, Patterns, Expr, Acc}) ->
    {ok, cerl:c_case(Expr, [cerl:c_clause([Ps], Acc) || Ps <- Patterns])};

% exor of form `(Expr1(), Expr2)`
gen_expr(expr, _, {seq, _, Expr, Acc}) -> {ok, cerl:c_seq(Expr, Acc)};

% expr of form: `"test string"` or `1.32`
gen_expr(expr, _, {value, _, _, Val}) -> {ok, cerl:abstract(Val)}.

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

traverse_term(Scope, Term, Tag) -> 
    case ast:traverse_term(expr, fun code_gen:pre_gen/3, fun code_gen:gen/3, Scope, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {_Env, Form}}  -> {ok, Tag, Form}
    end.
