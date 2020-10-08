-module(expr_gen).
-export([gen/1, call_type_tag/2]).
-include_lib("eunit/include/eunit.hrl").

gen(TypesEnv) ->
	fun(_Type, _Scope, Term) -> gen_expr(TypesEnv, Term) end.

gen_expr(TypesEnv, {type_def, Ctx, Name, ArgList, Clauses}) when is_list(Clauses) ->
    Args = [A || As <- ArgList, A <- As],
    ClauseList = lists:flatten(Clauses),
    AllClauses = case cerl_clauses:any_catchall(ClauseList) of
                     true -> ClauseList;
                     false -> ClauseList ++ [catchall(Args)]
                 end,
    Expr = cerl:c_case(cerl:c_values(Args), AllClauses),
    gen_expr(TypesEnv, {type_def, Ctx, Name, ArgList, Expr});

gen_expr(_, {type_def, _, Name, ArgList, Expr}) ->
    % Double expansion of args because the Arglist is treated as a pattern and
    % returned wrapped in a list
    Args = [A || As <- ArgList, A <- As],
    Form = case lists:flatten(Args) of
               []	-> Expr;
               _	-> gen_f(cerl:c_atom(Name), Args, Expr)
           end,
    {ok, Name, Form};

% type expr of form: T: ...
gen_expr(_, {tagged, Ctx, _, Val} = Term) ->
    Tag = symbol:tag(Term),
    CoreForm = cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), Val]),
    TypeForm = case maps:get(args, Ctx) of
                   []       -> CoreForm;
                   Args     -> gen_f(cerl:c_atom(Tag), [cerl:c_var(A) || {var, A} <- Args], CoreForm)
               end,
    {ok, Tag, TypeForm, CoreForm};

% expr of form: A | B | C
gen_expr(_, {sum, _, Elements}) ->
    SumElements = cerl:make_list(Elements),
    DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [SumElements]),
    {ok, cerl:c_tuple([cerl:c_atom(sum), DomainSet])};

% the pair of `k: a` in the form {k: a, ...}
gen_expr(_, {dict_pair, _, K, V}) -> {ok, cerl:c_map_pair(K, V)};

% TODO: What does a dictionary value look like? -- Two approaches:
% 1. A dictionary is a fixed datatype (like javascript)
% 2. a dictionary is an interface that different datatypes can implement to inherit the dict syntax
% With the latter approach, it's not trivial to figure out what datatype a
% dictionary should resolve to.  Fundamentally this will depend on the domain
% of the value, but there's no guarantee that the domain at the point of
% instantiation can be narrowed down to a single type.

% literal of form: { ... }
gen_expr(_, {dict, _, Elements}) ->
    {ok, cerl:c_tuple([cerl:c_atom(product), cerl:c_map(Elements)])};

% expr of form: `f(a)` or `T(a)`
% f can either be a type (encoded as {f, Tag, Function}) or a Function.
% If we knew the domain, we could separate the two out a compile time, but for now,
% we're assuming all applications is for types
gen_expr(_, {application, _, Expr, Args}) -> 
    case cerl:is_c_fun(Expr) orelse cerl:is_c_fname(Expr) of
        true    -> {ok, cerl:c_apply(Expr, Args)};
        false   -> 
            Var = cerl:c_var(symbol:id('')),
            F = cerl:c_var(symbol:id('F')),
            TypeClause = cerl:c_clause([cerl:c_tuple([cerl:c_atom('f'), cerl:c_var('_'), F])], cerl:c_apply(F, Args)),
            VarClause = cerl:c_clause([F], cerl:c_apply(F, Args)),
            {ok, cerl:c_let([Var], Expr, cerl:c_case(Var, [TypeClause, VarClause]))}
    end;

% expr of form: `T(a)` where `T` is a recursive type (and so we want to not
% call the domain function in an infinite loop
gen_expr(_, {recursive_type_application, _, Tag, Args}) -> 
    BranchFun = cerl:c_fun([], call_type_tag(Tag, Args)),
    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

% expr of Form: kind/prelude/Option(a)
gen_expr(_, {qualified_type_application, _, ModulePath, Name, Args}) -> 
    {ok, call_type_tag(ModulePath, Name, Args)};

% expr of Form: kind/prelude/match(a)
gen_expr(_, {qualified_variable_application, _, ModulePath, Name, Args}) -> 
    ModuleName = module:beam_name(ModulePath),
    {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), Args)};

% expr of form: T
gen_expr(TypesEnv, {type, _, _, Path} = Term) ->
    Tag = symbol:tag(Term),
    DomainForm = cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)]),
    case maps:get(Path, TypesEnv) of
        {tagged, _, _, _}       -> {ok, DomainForm};
        {type_def, _, _, _, _}  -> {ok, DomainForm};
        {type, _, _, _}         -> {ok, Tag, cerl:c_atom(Tag)}
    end;

% expr of form: kind/prelude/Boolean
gen_expr(_, {qualified_type, _, ModulePath, Name}) ->
    ModuleName = module:beam_name(ModulePath),
    {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(domain), [cerl:c_atom(Name)])};

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

% expr of form: | (t: T) -> Q(t) (or similar)
gen_expr(_, {clause, _, Patterns, Expr}) ->
    {ok, [cerl:c_clause(Ps, Expr) || Ps <- utils:combinations(Patterns)]}.

gen_f(Tag, Args, Expr) -> cerl:c_tuple([cerl:c_atom(f), Tag, cerl:c_fun(Args, Expr)]).

catchall(Args) ->
    Error = cerl:c_tuple([cerl:c_atom(error),
                          cerl:make_list(
                            [cerl:c_tuple(
                               [cerl:c_tuple(
                                  [cerl:c_atom(no_matching_pattern),
                                   cerl:make_list(Args)]),
                                cerl:c_tuple([cerl:c_atom(no_context)])])])]),
    cerl:c_clause([cerl:c_var(symbol:id('_')) || _ <- lists:seq(1, length(Args))], Error).

call_type_domain(ExprForm, ArgForms) ->
    cerl:c_apply(
      cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), 
                  [cerl:c_int(3), ExprForm]),
      ArgForms).

domain(Tag) -> cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)]).
domain(Module, Tag) -> cerl:c_call(cerl:c_atom(Module), cerl:c_atom(domain), [cerl:c_atom(Tag)]).

call_type_tag(ModulePath, Tag, ArgForms) ->
    Module = module:beam_name(ModulePath),
    call_type_domain(domain(Module, Tag), ArgForms).
call_type_tag(Tag, ArgForms) ->
    call_type_domain(domain(Tag), ArgForms).
