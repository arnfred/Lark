-module(scanner).
-import(lists, [zip/2, unzip/1, unzip3/1, seq/2, nth/2]).
-import(domain, [diff/2, union/1, union/2, intersection/1, intersection/2]).
-export([scan/2]).

-include_lib("eunit/include/eunit.hrl").

scan(TypeMod, AST) when is_list(AST) ->
    Scanned = [scan_top_level(TypeMod, Def) || Def <- AST],
    {Envs, _} = unzip([EnvFun(Scanned) || EnvFun <- Scanned]),
    maps:from_list(Envs).

scan_top_level(TypeMod, {def, _, Name, Args, Cs}) when is_list(Cs) ->
    F = fun(Env, ArgDomains, Stack) ->
                io:format("Scanning ~p with Domains: ~p and Stack ~p~n", [Name, ArgDomains, Stack]),
                case check_stack_recursion(Stack, Name, ArgDomains) of
                    error -> none;
                    ok -> 
                        NewStack = [{Name, ArgDomains} | Stack],
                        ClauseArgs = zip(Args, ArgDomains),
                        {_, DefDomain} = scan_clauses(Env, NewStack, TypeMod, ClauseArgs, Cs),
                        DefDomain
                end
        end,
    generate_env_fun(Name, length(Args), F);

scan_top_level(TypeMod, {def, _, Name, Args, Expr}) ->
    F = fun(Env, ArgDomains, Stack) -> 
                case check_stack_recursion(Stack, Name, ArgDomains) of
                    error -> none;
                    ok -> 
                        NewStack = [{Name, ArgDomains} | Stack],
                        ArgEnv = intersection(Env, maps:from_list([{symbol:tag(A), D} || {A, D} <- zip(Args, ArgDomains)])),
                        {_, DefDomain} = scan(ArgEnv, NewStack, TypeMod, Expr),
                        DefDomain
                end
        end,
    generate_env_fun(Name, length(Args), F).

scan(Env, Stack, TypeMod, {application, _, Expr, Args} = Current) ->
    {ArgEnvs, ArgDomains} = unzip([scan(Env, Stack, TypeMod, A) || A <- Args]),
    {ExprEnv, ExprDomain} = scan(Env, Stack, TypeMod, Expr),
    Domain = case ExprDomain of
                 {f, DomainFun} -> DomainFun(ArgDomains, Stack);
                 {error, E}     -> {error, E};
                 D              -> {error, [{Current, {expected_domain_fun, D}, Stack}]}
             end,
    {intersection([ExprEnv | ArgEnvs]), Domain};

scan(Env, Stack, TypeMod, {lookup, _, Var, Elems}) ->
    io:format("Lookup Env: ~p~n", [Env]),
    {ElemEnvs, ElemDomains} = unzip([scan(Env, Stack, TypeMod, E) || E <- Elems]),
    {VarEnv, Domain} = case scan(Env, Stack, TypeMod, Var) of
                                         {E, {f, DomainFun}} -> {E, DomainFun(ElemDomains)};
                                         {E, D} -> {E, TypeMod:lookup(D, ElemDomains)}
                                     end,
    {intersection([VarEnv | ElemEnvs]), Domain};

scan(Env, Stack, TypeMod, {pair, _, Key, Value}) ->
    {ValueEnv, ValueDomain} = scan(Env, Stack, TypeMod, Value),
    {KeyEnv, KeyDomain} = scan(Env, Stack, TypeMod, Key),
    {intersection(KeyEnv, ValueEnv), intersection(ValueDomain, KeyDomain)};

scan(Env, Stack, TypeMod, {match, _, Arg, Clauses}) ->
    {_, ArgDomain} = scan(Env, Stack, TypeMod, Arg),
    scan_clauses(Env, Stack, TypeMod, [{symbol:id(''), ArgDomain}], Clauses);

scan(Env, _, TypeMod, {lambda, _, Cs}) -> 
    Name = symbol:id('lambda'),
    F = fun(ArgDomains, Stack) -> 
                case check_stack_recursion(Stack, Name, ArgDomains) of
                    error -> none;
                    ok -> 
                        NewStack = [{Name, ArgDomains}],
                        Args = [{symbol:id(''), ArgD} || ArgD <- ArgDomains],
                        {_, LDomain} = scan_clauses(Env, NewStack, TypeMod, Args, Cs),
                        LDomain
                end
        end,
    {Env, {f, F}};

scan(Env, Stack, TypeMod, {tuple, _, Elems}) -> fold(Env, Stack, TypeMod, Elems);

scan(Env, _, _, {variable, _, _, Tag}) -> 
    D = maps:get(Tag, Env, any),
    {#{Tag => D}, D};

scan(_, _, TypeMod, {type, _, _} = T) -> {#{}, TypeMod:domain(symbol:tag(T))};

scan(Env, _, _, {qualified_symbol, _, S}) ->
    D = maps:get(S, Env, any),
    {#{S => D}, D}.

scan_clauses(Env, Stack, TypeMod, Args, Clauses) ->
    Scan = fun S([]) -> [];
               S([Clause | Cs]) ->
                   case scan_clause(Env, Stack, TypeMod, Args, Clause) of
                       skip -> S(Cs);
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

    % If any pattern has a domain of `none` there's no point in scanning the expression
    % since a Domain of `none` entails that the domain can take no values
    HasNone = lists:any(fun(D) -> D =:= none end, PsDomains),
    case HasNone of
        true    -> skip;
        _       -> {ExprEnv, ExprDomain} = scan(intersection([Env | PsEnvs]), Stack, TypeMod, Expr),
                   {ExprEnv, ExprDomain, PsDomains}
    end.


% Pattern of: `T(a, b)`
scan_pattern(Domain, TypeMod, {application, _, {type, _, T}, Args}) ->
    {ArgEnvs, ArgDomains} = unzip([scan_pattern(any, TypeMod, A) || A <- Args]),
    {f, TDomainFun} = TypeMod:domain(T), 
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
                ({variable, _, Key, _}) -> Key
             end,
    GetDomain = fun F(Elem, D) -> case D of
                                    {product, M} -> maps:get(GetKey(Elem), M, any);
                                    {sum, S} -> union([F(Elem, E) || E <- sets:from_list(S)]);
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
% function in `{f, DomainFun}` to be purely a function that accepts a domain as
% a parameter and returns a domain. However, when the function is executed, it
% executes the scan of the AST and needs an environment of other top level
% functions available. This isn't known at the time we generate the domain
% function, so we can't store it in the clojure. Instead I'm using this kludge
% to make sure it's passed in after we've created a domain function for all
% top-level functions.
%
% To understand what's going on in the code a bit better, have a look at the
% commit commit message of `ac6dcd61822ecfde1aeaa75482f059a9fe1f6d24`.
generate_env_fun(Name, Length, DomainFromEnv) -> 
    fun(EnvFunctions) ->
            DomainFun = fun(ArgDomains, Stack) ->
                                {Envs, _} = unzip([EnvF(EnvFunctions) || EnvF <- EnvFunctions]),
                                Env = maps:from_list(Envs),
                                DomainFromEnv(Env, ArgDomains, Stack)
                        end,
            {{{Name, Length}, {f, DomainFun}}, {f, DomainFun}}
    end.



check_stack_recursion([], _, _) -> ok;
check_stack_recursion([{Name, Domains} | Stack], Name, ArgDomains) ->
    case lists:all(fun({D, ArgD}) -> domain:subset(D, ArgD) end, zip(Domains, ArgDomains)) of
        true -> error;
        false -> check_stack_recursion(Stack, Name, ArgDomains)
    end;
check_stack_recursion([_ | Stack], Name, ArgDomains) -> 
    check_stack_recursion(Stack, Name, ArgDomains).

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
    ActualDomains = [DomainFun([], []) || {_, {f, DomainFun}} <- maps:to_list(Output)],
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
                         {f, DomainFun} = maps:get({'xor', 2}, Env),

                         Actual1 = DomainFun([any, any], []),
                         Expected1 = {sum, sets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun(['Boolean/True', 'Boolean/False'], []),
                         Expected2 = 'Boolean/True',
                         ?assertEqual(none, domain:diff(Expected2, Actual2)),

                         Boolean = {sum, sets:from_list(['Boolean/True', 'Boolean/False'])},
                         Actual3 = DomainFun([Boolean, Boolean], []),
                         Expected3 = Boolean,
                         ?assertEqual(none, domain:diff(Expected3, Actual3)),

                         Actual4 = DomainFun(['Boolean/True', 'Boolean/True'], []),
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
                         {f, DomainFun} = maps:get({g, 1}, Env),

                         Actual1 = DomainFun(['State/Stop'], []),
                         Expected1 = 'State/Stop',
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun(['State/Start'], []),
                         Expected2 = 'State/Stop',
                         ?assertEqual(none, domain:diff(Expected2, Actual2))
                 end,
    run(Code, RunAsserts).

infinite_recursion_test() ->
    Code = "def recurse a -> recurse(a)",
    RunAsserts = fun(Env) ->
                         {f, DomainFun} = maps:get({recurse, 1}, Env),
                         Expected = none,
                         Actual = DomainFun(['_'], []),
                         ?assertEqual(Expected, Actual)
                 end,
    run(Code, RunAsserts).
                    
infinite_co_recursion_test() ->
    Code = "def f a -> g(a)\n"
           "def g a -> h(a)\n"
           "def h a -> f(a)",
    RunAsserts = fun(Env) ->
                         {f, DomainFun} = maps:get({f, 1}, Env),
                         Expected = none,
                         Actual = DomainFun(['_'], []),
                         ?assertEqual(Expected, Actual)
                 end,
    run(Code, RunAsserts).

application_error_test() ->
    Code = "def f a -> a(f)",
    RunAsserts = fun(Env) ->
                         {f, DomainFun} = maps:get({f, 1}, Env),
                         {error, Error} = DomainFun([any], []),
                         ExpectedError = expected_domain_fun,
                         ExpectedDomain = any,
                         ExpectedStack = [{f, [any]}],
                         [{_, {ActualError, ActualDomain}, ActualStack}] = Error,
                         ?assertEqual(ExpectedError, ActualError),
                         ?assertEqual(ExpectedDomain, ActualDomain),
                         ?assertEqual(ExpectedStack, ActualStack)
                 end,
    run(Code, RunAsserts).
           


-endif.



