-module(scanner).
-import(lists, [zip/2, unzip/1, unzip3/1, seq/2, nth/2]).
-import(domain, [diff/2, union/1, union/2, intersection/1, intersection/2]).
-export([scan/2]).

-include_lib("eunit/include/eunit.hrl").

scan(Types, AST) when is_list(AST) ->
    Scanned = [scan(#{}, Types, Def) || Def <- AST],
    {Envs, _} = unzip([EnvFun(Scanned) || EnvFun <- Scanned]),
    maps:from_list(Envs).

scan(_, Types, {def, _, Name, Args, Cs}) when is_list(Cs) ->
    DomainFun = fun(Env, ArgDomains) ->
                        ClauseArgs = zip(Args, ArgDomains),
                        {_, Domain} = scan_clauses(Env, Types, ClauseArgs, Cs),
                        Domain
                end,
    generate_env_fun(Name, DomainFun);

scan(_, Types, {def, _, Name, Args, Expr}) ->
    DomainFun = fun(Env, ArgDomains) -> 
                        ArgEnv = maps:from_dict([{A, D} || {A, D} <- zip(Args, ArgDomains)]),
                        scan(intersection(ArgEnv, Env), Types, Expr)
                end,
    generate_env_fun(Name, DomainFun);

scan(Env, Types, {application, _, F, Args}) ->
    {ArgEnvs, ArgDomains} = [scan(Env, Types, A) || A <- Args],
    {FEnv, FDomain} = case F of
                          {type, _, T}  -> {#{}, Types:T(ArgDomains)};
                          Expr          -> {E, {f, DomainFun}} = scan(Env, Types, Expr),
                                           {E, DomainFun(ArgDomains)}
                      end,
    {intersection([FEnv | ArgEnvs]), FDomain};

scan(Env, Types, {lookup, _, Var, Elems}) ->
    {ElemEnvs, ElemDomains} = unzip([scan(Env, Types, E) || E <- Elems]),
    {VarEnv, Domain} = case scan(Env, Types, Var) of
                                         {E, {f, DomainFun}} -> {E, DomainFun(ElemDomains)};
                                         {E, D} -> {E, D}
                                     end,
    {intersection([VarEnv | ElemEnvs]), Domain};

scan(Env, Types, {pair, _, Key, Value}) ->
    {ValueEnv, ValueDomain} = scan(Env, Types, Value),
    {KeyEnv, KeyDomain} = scan(Env, Types, Key),
    {intersection(KeyEnv, ValueEnv), intersection(ValueDomain, KeyDomain)};

scan(Env, Types, {match, _, Arg, Clauses}) ->
    {_, ArgDomain} = scan(Env, Types, Arg),
    scan_clauses(Env, Types, [{symbol:id(''), ArgDomain}], Clauses);

scan(Env, Types, {lambda, _, Cs}) -> 
    LambdaDomain = fun(_, ArgDomains) -> 
                           scan_clauses(Env, Types, {symbol:id(''), ArgDomains}, Cs)
                   end,
    {Env, LambdaDomain};

scan(Env, Types, {tuple, _, Elems}) -> fold(Env, Types, Elems);

scan(Env, _, {variable, _, _, Tag}) -> 
    D = maps:get(Tag, Env, any),
    {#{Tag => D}, D};

scan(_, Types, {type, _, T}) -> {#{}, Types:T()};

scan(Env, _, {qualified_symbol, _, S}) ->
    D = maps:get(S, Env, any),
    {#{S => D}, D}.

scan_clauses(Env, Types, Args, Clauses) ->
    {EnvCs, DomainCs} = unzip([scan_clause(Env, Types, Args, C) || C <- Clauses]),
    {union(EnvCs), union(DomainCs)}.

scan_clause(Env, Types, Args, {clause, _, Patterns, Expr}) ->
    Scan = fun(Arg, D, T, P) ->
                   {E, Domain} = scan_pattern(D, T, P),
                   {intersection(E, #{Arg => Domain}), Domain}
           end,
    {PsEnvs, _} = unzip([Scan(A, D, Types, P) || {{A, D}, P} <- zip(Args, Patterns)]),
    scan(intersection([PsEnvs, Env]), Types, Expr).


% Pattern of: `T(a, b)`
scan_pattern(Domain, Types, {application, _, {type, _, T}, Args}) ->
    TArgDomains = Types:args(T),
    {f, TDomainFun} = Types:T(), 
    {ArgEnvs, ArgDomains} = unzip([scan_pattern(D, Types, A) || {D, A} <- zip(TArgDomains, Args)]),
    {intersection(ArgEnvs), intersection(Domain, TDomainFun(ArgDomains))};

% Pattern of: `T { k: v, ... }`
% The dictionary either contains pairs or variables named after product keywords
scan_pattern(Domain, Types, {lookup, Line, Var, Elems}) ->
    {VarEnv, VarDomain} = scan_pattern(Domain, Types, Var),
    {ElemsEnv, ElemsDomain} = scan_pattern(VarDomain, Types, {dict, Line, Elems}),
    {intersection(ElemsEnv, VarEnv), intersection(VarDomain, ElemsDomain)};

% Pattern of: `{ k: v, ... }`
scan_pattern(Domain, Types, {dict, _, Elems}) ->
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
    {ArgEnvs, ArgDomainList} = unzip([scan_pattern(GetDomain(E, Domain), Types, E) || E <- Elems]),
    ArgDomain = maps:from_list([{GetKey(E), D} || {E, D} <- zip(Elems, ArgDomainList)]),
    {intersection(ArgEnvs), {product, ArgDomain}};

% Pattern of: `T` for some defined type `T`
scan_pattern(Domain, Types, {type, _, T}) -> {#{}, intersection(Domain, Types:T())};

% Pattern of `v` for any non-type
scan_pattern(Domain, _, {variable, _, _, Tag}) -> {#{Tag => Domain}, Domain};

% Pattern of `k: v`
scan_pattern(Domain, Types, {pair, _, {variable, _, Key, _}, Val}) ->
    {ValEnv, ValDomain} = scan_pattern(Domain, Types, Val),
    {intersection([ValEnv, #{Key => intersection(ValDomain, Domain)}]), ValDomain}.

fold(Env, Types, Elements) when is_list(Elements) ->
    F = fun(Elem, {EnvAcc, _}) -> 
                {ElemEnv, Domain} = scan(EnvAcc, Types, Elem),
                {union([EnvAcc, ElemEnv]), Domain} 
        end,
    {NewEnv, NewDomain} = lists:foldl(F, {Env, any}, Elements),
    {NewEnv, NewDomain}.

generate_env_fun(Name, DomainFromEnv) -> 
    fun(EnvFunctions) ->
            DomainFun = fun(ArgDomains) ->
                                {Envs, _} = unzip([EnvF(EnvFunctions) || EnvF <- EnvFunctions]),
                                Env = maps:from_list(Envs),
                                DomainFromEnv(Env, ArgDomains)
                        end,
            {{Name, {f, DomainFun}}, {f, DomainFun}}
    end.


-ifdef(TEST).

env_gen_test() ->
    AST = [{def, 1, function1, [], []}, {def, 2, function2, [], []}],
    Output = scan({}, AST),
    ExpectedDomains = [none, none],
    ActualDomains = [DomainFun([]) || {_, {f, DomainFun}} <- maps:to_list(Output)],
    ExpectedKeys = [function1, function2],
    ActualKeys = [Key || {Key, _} <- maps:to_list(Output)],
    ?assertEqual(ExpectedDomains, ActualDomains),
    ?assertEqual(ExpectedKeys, ActualKeys).

-endif.

