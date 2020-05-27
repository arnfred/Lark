-module(scanner).
-import(lists, [zip/2, unzip/1, unzip3/1, seq/2, nth/2]).
-import(domain, [intersection/1, intersection/2, union/1, union/2]).
-export([scan/2]).

-include_lib("eunit/include/eunit.hrl").

scan(TypeMod, AST) when is_list(AST) ->
    Scanned = [scan_top_level(TypeMod, Def) || Def <- AST],
    {_, Names, Domains} = unzip3([EnvFun(Scanned) || EnvFun <- Scanned]),
    maps:from_list(zip(Names, Domains)).

scan_top_level(TypeMod, {def, _, Name, Args, Cs}) when is_list(Cs) ->
    F = fun(Env, Stack, ArgDomains) ->
                NewStack = [{Name, ArgDomains} | Stack],
                ClauseArgs = zip(Args, ArgDomains),
                {_, DefDomain} = scan_clauses(Env, NewStack, TypeMod, ClauseArgs, Cs),
                DefDomain
        end,
    generate_env_fun(Name, length(Args), F, TypeMod);

scan_top_level(TypeMod, {def, _, Name, Args, Expr}) ->
    F = fun(Env, Stack, ArgDomains) -> 
                NewStack = [{Name, ArgDomains} | Stack],
                ArgEnv = maps:from_list([{symbol:tag(A), D} || {A, D} <- zip(Args, ArgDomains)]),
                NewEnv = intersection(Env, ArgEnv),
                {_, DefDomain} = scan(NewEnv, NewStack, TypeMod, Expr),
                DefDomain
        end,
    generate_env_fun(Name, length(Args), F, TypeMod).

scan(Env, Stack, TypeMod, {application, _, Expr, Args} = Parsnip) ->
    {ArgEnvs, ArgDomains} = unzip([scan(Env, Stack, TypeMod, A) || A <- Args]),
    {ExprEnv, ExprDomain} = scan(Env, Stack, TypeMod, Expr),
    io:format("Args: ~p~n", [Args]),
    io:format("ArgDomains: ~p~n", [ArgDomains]),
    Domain = apply_domain(ExprDomain, Stack, ArgDomains, Parsnip),
    {intersection([ExprEnv | ArgEnvs]), Domain};

scan(Env, Stack, TypeMod, {lookup, _, Var, Elems} = Parsnip) ->
    {_, ElemDomains} = unzip([scan(Env, Stack, TypeMod, E) || E <- Elems]),
    Entries = maps:from_list([{symbol:name(E), D} || {E, D} <- zip(Elems, ElemDomains)]),
    io:format("Elem Entires: ~p~n", [Entries]),
    {VarEnv, Domain} = case scan(Env, Stack, TypeMod, Var) of
                           {E, {product,_} = D} -> {E, domain:lookup(D, Entries)};
                           {E, {tagged,_,_} = D} -> {E, domain:lookup(D, Entries)};
                           {E, {error, Err}} -> {E, {error, Err}};
                           {E, D} -> {E, {error, [{Parsnip, {expected_product_domain, D}, Stack}]}}
                       end,
    {intersection(VarEnv, Env), Domain};

scan(Env, Stack, TypeMod, {pair, _, Key, Value} = Parsnip) ->
    {ValueEnv, ValueDomain} = scan(Env, Stack, TypeMod, Value),
    {KeyEnv, KeyDomain} = scan(Env, Stack, TypeMod, Key),
    Domain = intersection(ValueDomain, KeyDomain),
    NewEnv = intersection(KeyEnv, ValueEnv),
    case domain:subset(KeyDomain, ValueDomain) of
        false      -> {NewEnv, {error, [{Parsnip, {not_a_subset, KeyDomain, ValueDomain}, Stack}]}};
        true       -> {NewEnv, Domain}
    end;

scan(Env, _, TypeMod, {lambda, Line, [{clause, _, Ps, _} | _] = Cs} = Parsnip) -> 
    Name = symbol:id(['lambda', list_to_atom(integer_to_list(Line))]),
    F = fun(Stack, ArgDomains) -> 
                NewStack = [{Name, ArgDomains} | Stack],
                Tags = [symbol:id(Name) || _ <- ArgDomains],
                Args = [{{variable, 0, '', Tag}, ArgD} || {Tag, ArgD} <- zip(Tags, ArgDomains)],
                {LEnv, LDomain} = scan_clauses(Env, NewStack, TypeMod, Args, Cs),
                IsSubset = fun({D1, D2}) -> domain:subset(D1, D2) end,
                ActualDomains = [maps:get(Tag, LEnv) || Tag <- Tags],
                case lists:all(IsSubset, zip(ArgDomains, ActualDomains)) of
                    false      -> {error, [{Parsnip, {arguments_not_subsets, ArgDomains, ActualDomains}, NewStack}]};
                    true       -> LDomain
                end
        end,
    {Env, two_step_pass_stack(F, Name, length(Ps))};

scan(Env, Stack, TypeMod, {tuple, _, Elems}) -> fold(Env, Stack, TypeMod, Elems);

scan(Env, Stack, TypeMod, {match, _, Arg, Clauses}) ->
    {_, ArgDomain} = scan(Env, Stack, TypeMod, Arg),
    scan_clauses(Env, Stack, TypeMod, [{symbol:id(''), ArgDomain}], Clauses);

scan(Env, _, _, {variable, _, _, Tag}) -> 
    D = maps:get(Tag, Env, any),
    {intersection(Env, #{Tag => D}), D};

scan(Env, _, TypeMod, {type, _, _} = T) -> 
    Domain = case TypeMod:domain(symbol:tag(T)) of
                 {f, Name, F}   -> two_step_ignore_stack(F, Name);
                 D              -> D end,
    {Env, Domain};

scan(Env, _, _, {key, _, Key}) -> {Env, Key};

scan(Env, _, _, {qualified_symbol, _, S}) ->
    D = maps:get(S, Env, any),
    {intersection(Env, #{S => D}), D}.

scan_clauses(Env, Stack, TypeMod, Args, Clauses) ->
    Scan = fun S([]) -> [];
               S([Clause | Cs]) ->
                   case scan_clause(Env, Stack, TypeMod, Args, Clause) of
                       skip_clause -> S(Cs);
                       {E, D, PsDomains} ->

                           % Only scan patterns until (and including) the first pattern which domain
                           % is a subset of the pattern domain. There's no point in keeping on
                           % scanning patterns if we can infer from the argument domains that they
                           % will match the domain for sure.
                           IsSubset = lists:all(fun({Darg, Dpat}) -> domain:subset(Darg, Dpat) end,
                                                [{Darg, Dpat} || {{_, Darg}, Dpat} <- zip(Args, PsDomains)]),
                           case IsSubset of
                               true -> [{E, D}];
                               false -> [{E, D} | S(Cs)]
                           end
                   end
           end,

    {EnvCs, DomainCs} = unzip(Scan(Clauses)),
    io:format("EnvCs: ~p~nDomainCs: ~p~n", [union(EnvCs), DomainCs]),
    {union(EnvCs), union(DomainCs)}.

scan_clause(Env, Stack, TypeMod, Args, {clause, _, Patterns, Expr}) ->
    Scan = fun(Arg, Domain, Pattern) ->
                   {E, D} = scan_pattern(Domain, TypeMod, Pattern),
                   {intersection(E, #{symbol:tag(Arg) => D}), D}
           end,
    {PsEnvs, PsDomains} = unzip([Scan(Arg, Domain, Pattern) || 
                                 {{Arg, Domain}, Pattern} <- zip(Args, Patterns)]),

    ClauseEnv = intersection([Env | PsEnvs]),

    % If any pattern has a domain of `none` there's no point in scanning the expression
    % since a Domain of `none` entails that the domain can take no values
    HasNone = lists:any(fun(D) -> D =:= none end, PsDomains),
    case HasNone of
        true    -> skip_clause;
        _       -> {ExprEnv, ExprDomain} = scan(ClauseEnv, Stack, TypeMod, Expr),
                   {ExprEnv, ExprDomain, PsDomains}
    end.


% Pattern of: `T(a, b)`
scan_pattern(Domain, TypeMod, {application, _, {type, _, T}, Args}) ->
    {ArgEnvs, ArgDomains} = unzip([scan_pattern(any, TypeMod, A) || A <- Args]),
    {f, _, TDomainFun} = TypeMod:domain(T), 
    {intersection(ArgEnvs), intersection(Domain, TDomainFun(ArgDomains))};

% Pattern of: `T { k: v, ... }`
% The dictionary either contains pairs or variables named after product keywords
scan_pattern(Domain, TypeMod, {lookup, Line, Var, Elems}) ->
    {VarEnv, VarDomain} = scan_pattern(Domain, TypeMod, Var),
    {ElemsEnv, ElemsDomain} = scan_pattern(VarDomain, TypeMod, {dict, Line, Elems}),
    {intersection(ElemsEnv, VarEnv), intersection(VarDomain, ElemsDomain)};

% Pattern of: `{ k: v, ... }`
scan_pattern(Domain, TypeMod, {dict, _, Elems}) ->
    GetKey = fun({pair, _, {variable, _, Key, _}, _}) -> Key;
                ({variable, _, Key, _}) -> Key;
                ({key, _, Key}) -> Key
             end,
    GetDomain = fun F(Elem, D) -> case D of
                                    {product, M} -> maps:get(GetKey(Elem), M, any);
                                    {sum, S} -> union([F(Elem, E) || E <- ordsets:from_list(S)]);
                                    {tagged, _, T} -> F(Elem, T);
                                    any -> any;
                                    _ -> none
                               end end,
    {ArgEnvs, ArgDomainList} = unzip([scan_pattern(GetDomain(E, Domain), TypeMod, E) || E <- Elems]),
    ArgDomain = maps:from_list([{GetKey(E), D} || {E, D} <- zip(Elems, ArgDomainList)]),
    {intersection(ArgEnvs), {product, ArgDomain}};

% Pattern of: `( ... )`
scan_pattern(Domain, TypeMod, {tuple, _, [Elem]}) -> scan_pattern(Domain, TypeMod, Elem);

% Pattern of: `T` for some defined type `T`
scan_pattern(Domain, TypeMod, {type, _, _} = T) -> 
    {#{}, intersection(Domain, TypeMod:domain(symbol:tag(T)))};

% Pattern of `v` for any non-type
scan_pattern(Domain, _, {variable, _, _, _} = Var) -> {#{symbol:tag(Var) => Domain}, Domain};

% Pattern of `k: T`
scan_pattern(Domain, TypeMod, {pair, _, Key, Val}) ->
    {ValEnv, ValDomain} = scan_pattern(Domain, TypeMod, Val),
    KeyDomain = intersection(ValDomain, Domain),
    io:format("KeyDomain: ~p~nDomain: ~p, ValDomain: ~p~n", [KeyDomain, Domain, ValDomain]),
    {intersection(ValEnv, #{symbol:tag(Key) => KeyDomain}), KeyDomain}.

fold(Env, Stack, TypeMod, Elements) when is_list(Elements) ->
    F = fun(Elem, {EnvAcc, _}) -> 
                {ElemEnv, Domain} = scan(EnvAcc, Stack, TypeMod, Elem),
                {intersection([EnvAcc, ElemEnv]), Domain} 
        end,
    lists:foldl(F, {Env, any}, Elements).


% This is a bit of a funky pattern that is going to be hard to understand when
% reading the code. In short, what's going on here is that we want the domain
% function in `{f, Name, DomainFun}` to be purely a function that accepts a
% domain as a parameter and returns a domain. However, when the function is
% executed, it executes the scan of the AST and needs an environment of other
% top level functions available. This isn't known at the time we generate the
% domain function, so we can't store it in the clojure. Instead I'm using this
% kludge to make sure it's passed in after we've created a domain function for
% all top-level functions.
%
% To understand what's going on in the code a bit better, have a look at the
% commit commit message of `ac6dcd61822ecfde1aeaa75482f059a9fe1f6d24`.
generate_env_fun(Name, Length, DomainFromEnv, TypeMod) -> 
    fun(EnvFunctions) ->
            F = fun(Stack, Inputs) ->
                        ArgDomains = [get_domain(TypeMod, I) || I <- Inputs],
                        {Envs, _, _} = unzip3([EnvF(EnvFunctions) || EnvF <- EnvFunctions]),
                        Env = maps:from_list(Envs),
                        DomainFromEnv(Env, Stack, ArgDomains)
                end,
            {f, Name, DomainFun} = two_step_pass_stack(F, Name, Length),
            Domain = DomainFun([]), % Pass empty stack for top-level domain
            {{{Name, Length}, {f, Name, DomainFun}}, {Name, Length}, Domain}
    end.

get_domain(_, any)                              -> any;
get_domain(_, none)                             -> none;
get_domain(TypeMod, Atom) when is_atom(Atom)    -> element(2, scan([], [], TypeMod, get_type(Atom)));
get_domain(_, Tuple) when is_tuple(Tuple)       -> Tuple;
get_domain(_, D)                                -> D.

get_type(Atom) when is_atom(Atom) -> {type, 0, get_type(atom_to_list(Atom))};
get_type([]) -> [];
get_type([$/ | Tail]) -> get_type(Tail);
get_type(List) when is_list(List) ->
    {Element, Rest} = lists:splitwith(fun(C) -> not(C =:= $/) end, List),
    [list_to_atom(Element) | get_type(Rest)].

apply_domain({f, Name, StackFun}, Stack, Args, _) ->
    case check_stack_recursion(Stack, Name, Args) of
        error -> none;
        ok -> {f, Name, DomainFun} = StackFun(Stack),
              io:format("Args for ~p: ~p~n", [Name, Args]),
              erlang:apply(DomainFun, Args)
    end;
apply_domain({error, Err}, _, _, _) -> {error, Err};
apply_domain(D, Stack, _, Parsnip) -> {error, [{Parsnip, {expected_function_domain, D}, Stack}]}.

check_stack_recursion([], _, _) -> ok;
check_stack_recursion([{Name, Domains} | Stack], Name, ArgDomains) ->
    case lists:all(fun({D, ArgD}) -> domain:subset(D, ArgD) end, zip(Domains, ArgDomains)) of
        true -> error;
        false -> check_stack_recursion(Stack, Name, ArgDomains)
    end;
check_stack_recursion([_ | Stack], Name, ArgDomains) -> 
    check_stack_recursion(Stack, Name, ArgDomains).


% This function is responsible for two things:
% - It wraps the function domain inside another function domain in order to
%   pass the Stack
% - It generates a function which takes arguments as individual parameters
%   instead of an argument list
% We need to pass the stack to avoid recursions. Check the corresponding git
% commit message for more details.
two_step_pass_stack(F, Name, N) ->
    StepOne = fun(Stack) -> 
                      StepTwo = spread(fun(Args) -> F(Stack, Args) end, N),
                      {f, Name, StepTwo}
              end,
    {f, Name, StepOne}.

% In cases where we don't need to pass the stack, we still want to wrap the
% function domains in another domain to keep things consistent
two_step_ignore_stack(F, Name) -> {f, Name, fun(_) -> {f, Name, F} end}.

spread(F, N) ->
    case N of
        0  -> F;
        1  -> fun(A1) -> F([A1]) end;
        2  -> fun(A1, A2) -> F([A1, A2]) end;
        3  -> fun(A1, A2, A3) -> F([A1, A2, A3]) end;
        4  -> fun(A1, A2, A3, A4) -> F([A1, A2, A3, A4]) end;
        5  -> fun(A1, A2, A3, A4, A5) -> F([A1, A2, A3, A4, A5]) end;
        6  -> fun(A1, A2, A3, A4, A5, A6) -> F([A1, A2, A3, A4, A5, A6]) end;
        7  -> fun(A1, A2, A3, A4, A5, A6, A7) -> F([A1, A2, A3, A4, A5, A6, A7]) end;
        8  -> fun(A1, A2, A3, A4, A5, A6, A7, A8) -> F([A1, A2, A3, A4, A5, A6, A7, A8]) end;
        9  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9]) end;
        10  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]) end;
        11  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]) end;
        12  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]) end;
        13  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]) end;
        14  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]) end;
        15  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]) end;
        16  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]) end;
        17  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]) end;
        18  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]) end;
        19  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]) end;
        20  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]) end;
        21  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]) end;
        22  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]) end;
        23  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]) end;
        24  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24]) end;
        25  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25) -> F([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25]) end
    end.

-ifdef(TEST).

run(Code, RunAsserts) ->
    {ok, _, {TypeAST, AST}} = kind:get_AST(Code),
    io:format("DefAST: ~p~n", [AST]),
    io:format("TypeAST: ~p~n", [TypeAST]),
    {ok, TypeMod} = typer:load("test", TypeAST),
    Env = scan(TypeMod, AST),
    RunAsserts(Env),
    true = code:soft_purge(TypeMod),
    true = code:delete(TypeMod).


env_gen_test() ->
    AST = [{def, 1, function1, [], []}, {def, 2, function2, [], []}],
    Output = scan({}, AST),
    ExpectedDomains = [none, none],
    ActualDomains = [DomainFun([]) || {_, {f, _, DomainFun}} <- maps:to_list(Output)],
    ExpectedKeys = [{function1, 0}, {function2, 0}],
    ActualKeys = [Key || {Key, _} <- maps:to_list(Output)],
    ?assertEqual(ExpectedDomains, ActualDomains),
    ?assertEqual(ExpectedKeys, ActualKeys).

run_xor_test() ->
    Code = "type Boolean -> True | False\n"
           "def xor a b\n"
           " | True, False -> True\n"
           " | False, True -> b\n"
           " | _, _ -> False",
    RunAsserts = fun(Env) ->
                         {f, 'xor', DomainFun} = maps:get({'xor', 2}, Env),

                         Actual1 = DomainFun(any, any),
                         Expected1 = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('Boolean/True', 'Boolean/False'),
                         Expected2 = 'Boolean/True',
                         ?assertEqual(none, domain:diff(Expected2, Actual2)),

                         Boolean = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         Actual3 = DomainFun(Boolean, Boolean),
                         Expected3 = Boolean,
                         ?assertEqual(none, domain:diff(Expected3, Actual3)),

                         Actual4 = DomainFun('Boolean/True', 'Boolean/True'),
                         Expected4 = 'Boolean/False',
                         ?assertEqual(none, domain:diff(Expected4, Actual4))
                 end,
    run(Code, RunAsserts).

direct_recursion_test() ->
    Code = "type State -> Start | Continue | Stop\n"
           "def g state\n"
           " | Start -> g(Continue)\n"
           " | Continue -> g(Stop)\n"
           " | Stop -> state",

    RunAsserts = fun(Env) ->
                         {f, g, DomainFun} = maps:get({g, 1}, Env),

                         Actual1 = DomainFun('State/Stop'),
                         Expected1 = 'State/Stop',
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('State/Start'),
                         Expected2 = 'State/Stop',
                         ?assertEqual(none, domain:diff(Expected2, Actual2))
                 end,
    run(Code, RunAsserts).

infinite_recursion_test() ->
    Code = "def recurse a -> recurse(a)",
    RunAsserts = fun(Env) ->
                         {f, recurse, DomainFun} = maps:get({recurse, 1}, Env),
                         Expected = none,
                         Actual = DomainFun(['_']),
                         ?assertEqual(Expected, Actual)
                 end,
    run(Code, RunAsserts).
                    
infinite_co_recursion_test() ->
    Code = "def f a -> g(a)\n"
           "def g a -> h(a)\n"
           "def h a -> f(a)",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Expected = none,
                         Actual = DomainFun(['_']),
                         ?assertEqual(Expected, Actual)
                 end,
    run(Code, RunAsserts).

application_error_test() ->
    Code = "def f a -> a(f)",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         {error, Error} = DomainFun(any),
                         ExpectedError = expected_function_domain,
                         ExpectedDomain = any,
                         [{_, {ActualError, ActualDomain}, _}] = Error,
                         ?assertEqual(ExpectedError, ActualError),
                         ?assertEqual(ExpectedDomain, ActualDomain)
                 end,
    run(Code, RunAsserts).

lookup_expr_product_test() ->
    Code ="def f t -> t { a, b }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Expected1 = {product, #{a => 'A', b => 'B'}},
                         Actual1 = DomainFun({product, #{a => 'A', b => 'B', c => 'C'}}),
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Expected2 = {product, #{a => 'A', b => 'B'}},
                         Actual2 = DomainFun({tagged, tag, {product, #{a => 'A', b => 'B'}}}),
                         ?assertEqual(none, domain:diff(Expected2, Actual2)),

                         Expected3 = none,
                         Actual3 = DomainFun({product, #{c => 'C', d => 'D'}}),
                         ?assertEqual(none, domain:diff(Expected3, Actual3))
                 end,
    run(Code, RunAsserts).

lookup_expr_sum_test() ->
    Code = "type T -> T: {blip: (Blip | Blop)\n"
           "              blup: (Blup | Blap)}\n"
           "def f a\n"
           " | (t: T) -> t { blup }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Expected = {product, #{blup => {sum, ordsets:from_list(['T/Blup', 'T/Blap'])}}},
                         Actual = DomainFun(any),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).
           
lookup_error_propagation_test() ->
    Code = "def f t -> t { a }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         StackError = {error, [some_error]},
                         Actual1 = DomainFun(StackError),
                         Expected1 = StackError,
                         ?assertEqual(none, domain:diff(Expected1, Actual1))
                 end,
    run(Code, RunAsserts).

lookup_non_product_or_tagged_domain_test() ->
    Code = "def f t -> t { a }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Expected = {expected_product_domain, any},
                         {error, [{_, Error, _}]} = DomainFun(any),
                         ?assertEqual(none, domain:diff(Expected, Error))
                 end,
    run(Code, RunAsserts).

pair_values_refinement_test() ->
    Code = "type Boolean -> True | False\n"
           "def f t -> t: Boolean",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Actual1 = DomainFun('Boolean/True'),
                         Expected1 = 'Boolean/True',
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('any'),
                         Constraint = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?assertMatch({error, [{_, {not_a_subset, any, Constraint}, _}]}, Actual2)
                 end,
    run(Code, RunAsserts).

pair_expressions_refinement_test() ->
    Code = "type Boolean -> True | False\n"
           "type Option a -> a | None\n"
           "def id a -> a\n"
           "def f t -> id(t): Option(Boolean)",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Actual1 = DomainFun('Boolean'),
                         Expected1 = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('any'),
                         Constraint = {sum, ordsets:from_list(['Boolean/True',
                                                               'Boolean/False',
                                                               'Option/None'])},
                         ?assertMatch({error, [{_, {not_a_subset, any, Constraint}, _}]}, Actual2)
                 end,
    run(Code, RunAsserts).

lambda_clause_test() ->
    Code = "type Boolean -> True | False\n"
           "def ap f a -> f(a)\n"
           "def test a -> ap(True -> False\n"
           "                 False -> False,\n"
           "                 a)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual = DomainFun('Boolean'),
                         Expected = 'Boolean/False',
                         ?assertEqual(none, domain:diff(Expected, Actual)),

                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual2 = DomainFun(any),
                         Constraint = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?assertMatch({error, 
                                       [{_, 
                                         {arguments_not_subsets, [any], [Constraint]}, 
                                         [_, {ap, _}, {test, _}]}]}, Actual2)
                 end,
    run(Code, RunAsserts).

-endif.



