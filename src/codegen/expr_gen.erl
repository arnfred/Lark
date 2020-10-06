-module(expr_gen).
-export([gen/2]).

gen(TypesEnv, ArgsEnv) ->
	fun(_Type, _Scope, Term) -> gen_expr(TypesEnv, ArgsEnv, Term) end.

gen_expr(TypesEnv, ArgsEnv, {type_def, Ctx, Name, ArgList, Clauses}) when is_list(Clauses) ->
    Args = [A || As <- ArgList, A <- As],
    ClauseList = lists:flatten(Clauses),
    AllClauses = case cerl_clauses:any_catchall(ClauseList) of
                     true -> ClauseList;
                     false -> ClauseList ++ [catchall(Args)]
                 end,
    Expr = cerl:c_case(cerl:c_values(Args), AllClauses),
    gen_expr(TypesEnv, ArgsEnv, {type_def, Ctx, Name, ArgList, Expr});

gen_expr(_, _, {type_def, _, Name, ArgList, Expr}) ->
    Args = [A || As <- ArgList, A <- As],
    Form = case lists:flatten(Args) of
               []	-> Expr;
               _	-> gen_f(cerl:c_atom(Name), Args, Expr)
           end,
    {ok, Name, Form};

% type expr of form: T: ...
gen_expr(_, ArgsEnv, {tagged, _, _, Val} = Term) ->
    Tag = symbol:tag(Term),
    CoreForm = cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), Val]),
    TypeForm = case maps:get(Tag, ArgsEnv) of
                   []       -> CoreForm;
                   Args     -> gen_f(cerl:c_atom(Tag), [cerl:c_var(A) || {var, A} <- Args], CoreForm)
               end,
    {ok, Tag, TypeForm, CoreForm};

% expr of form: A | B | C
gen_expr(_, _, {sum, _, Elements}) ->
    SumElements = cerl:make_list(Elements),
    DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [SumElements]),
    {ok, cerl:c_tuple([cerl:c_atom(sum), DomainSet])};

% the pair of `k: a` in the form {k: a, ...}
gen_expr(_, _, {dict_pair, _, K, V}) -> {ok, cerl:c_map_pair(K, V)};

% TODO: What does a dictionary value look like? -- Two approaches:
% 1. A dictionary is a fixed datatype (like javascript)
% 2. a dictionary is an interface that different datatypes can implement to inherit the dict syntax
% With the latter approach, it's not trivial to figure out what datatype a
% dictionary should resolve to.  Fundamentally this will depend on the domain
% of the value, but there's no guarantee that the domain at the point of
% instantiation can be narrowed down to a single type.

% literal of form: { ... }
gen_expr(_, _, {dict, _, Elements}) ->
    {ok, cerl:c_tuple([cerl:c_atom(product), cerl:c_map(Elements)])};

% expr of form: `f(a)` or `kind/prelude/Boolean(a)`
% f can either be a type (encoded as {f, Tag, Function}) or a Function.
% If we knew the domain, we could separate the two out a compile time, but for now,
% we're assuming all applications is for types
gen_expr(_, _, {application, _, Expr, Args}) -> 
    {ok, type_gen:call_type_domain(Expr, Args)};

% expr of Form: T(a)
gen_expr(_, _, {type_application, _, Tag, Args} = Term) -> 
    IsRecursive = lists:member(Tag, ast:get_tag(path, Term)),
    case IsRecursive of

        % Normal type function with no recursion
        false -> {ok, type_gen:call_type_tag(Tag, Args)};

        % type recursion e.g.: List a -> Nil | Cons: { head: a, tail: List(a) }
        true  -> 
            BranchFun = cerl:c_fun([], type_gen:call_type_tag(Tag, Args)),
            {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])}
    end;

% expr of form: T
gen_expr(TypesEnv, _, {type, _, _, Path} = Term) ->
    Tag = symbol:tag(Term),
    IsRecursive = lists:member(Tag, ast:get_tag(path, Term)),
    IsDefined = maps:is_key(Path, TypesEnv),
    DomainForm = cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)]),
    case {IsRecursive, IsDefined} of
        {true, _}       -> BranchFun = cerl:c_fun([], DomainForm),
                           {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};
        {_, true}       ->
            case maps:get(Path, TypesEnv) of
                {tagged, _, _, _}       -> {ok, DomainForm};
                {type_def, _, _, _, _}  -> {ok, DomainForm};
                {type, _, _, _}         -> {ok, Tag, cerl:c_atom(Tag)}
            end;

        % type constant from different module
        {false, false}  -> {ok, cerl:c_atom(Tag)}
    end;

% expr of form: kind/prelude/Boolean
gen_expr(_, _, {qualified_type, _, ModulePath, Name}) ->
    ModuleName = module:beam_name(ModulePath),
    {ok, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(domain), [cerl:c_atom(Name)])};

% expr of form: `k` in {k: val}
gen_expr(_, _, {key, _, _} = Term) -> {ok, cerl:c_atom(symbol:tag(Term))};

% expr of form: a
gen_expr(_, _, {variable, _, _, _} = Term) -> {ok, cerl:c_var(symbol:tag(Term))};

% type atom
gen_expr(_, _, Atom) when is_atom(Atom) -> {ok, cerl:c_atom(Atom)};

% expr of form: | (t: T) -> Q(t) (or similar)
gen_expr(_, _, {clause, _, Patterns, Expr}) ->
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
