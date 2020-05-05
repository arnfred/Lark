-module(scanner).
-import(lists, [zip/2, unzip/1, unzip3/1, seq/2, nth/2]).
-import(domain, [diff/2, union/1, union/2, intersection/1, intersection/2]).
-export([scan/3]).

scan(Env, Types, {def, _, Name, Args, {clauses, _, _, Cs}}) ->
    DefDomain = fun(E, ArgDomains) -> 
                        ClauseArgs = zip(Args, ArgDomains),
                        scan_clauses(E, Types, ClauseArgs, Cs) end,
    NewEnv = domain:union(Env, #{Name => {f, DefDomain}}),
    DefDomain(NewEnv, [maps:get(A, Env, any) || A <- Args]);

scan(Env, Types, {def, _, Name, Args, Expr}) ->
    DefDomain = fun(E, ArgDomains) -> 
                        ArgEnv = maps:from_dict([{A, D} || {A, D} <- zip(Args, ArgDomains)]),
                        scan(union(ArgEnv, E), Types, Expr)
                end,
    NewEnv = domain:union(Env, #{Name => {f, DefDomain}}),
    DefDomain(NewEnv, [maps:get(A, Env, any) || A <- Args]);

scan(Env, Types, {application, _, F, Args}) ->
    {ArgEnvs, ArgDomains} = [scan(Env, Types, A) || A <- Args],
    {FEnv, FDomain} = case F of
                          {type, _, T} -> {#{}, Types:T(ArgDomains)};
                          Expr -> {ExprEnv, {f, DomainFun}} = scan(Env, Types, Expr),
                                  DomainFun(ExprEnv, ArgDomains)
                      end,
    {intersection([FEnv | ArgEnvs]), FDomain};

scan(Env, Types, {lookup, _, Var, Elems}) ->
    {ElemEnvs, ElemDomains} = unzip([scan(Env, Types, E) || E <- Elems]),
    {VarEnv, Domain} = case scan(Env, Types, Var) of
                                         {E, {f, DomainFun}} -> DomainFun(E, ElemDomains);
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
    LambdaDomain = fun(E, ArgDomains) -> scan_clauses(union(E, Env), Types, {symbol:id(''), ArgDomains}, Cs) end,
    {Env, LambdaDomain};

scan(Env, Types, {tuple, _, Elems}) -> fold(Env, Types, Elems);

scan(Env, _, {variable, _, _, Tag}) -> 
    {maps:update_with(Tag, fun(a) -> a end, #{Tag => any}, Env), maps:get(Tag, Env, any)};

scan(_, Types, {type, _, T}) -> {#{}, Types:T()};

scan(_, _, {qualified_symbol, _, _}) -> {#{}, any}.

scan_clauses(Env, Types, Args, Clauses) ->
    {EnvCs, DomainCs} = unzip([scan_clause(Env, Types, Args, C) || C <- Clauses]),
    {union(EnvCs), union(DomainCs)}.

scan_clause(Env, Types, Args, {clause, _, Patterns, Expr}) ->
    Scan = fun(Arg, D, T, P) ->
                   {E, Domain} = scan_pattern(D, T, P),
                   {E, domain:union(#{Arg => Domain}, Domain)}
           end,
    {PsEnvs, _} = unzip([Scan(A, D, Types, P) || {{A, D}, P} <- zip(Args, Patterns)]),
    scan(intersection([PsEnvs, Env]), Types, Expr).


% Pattern of: `T(a, b)`
scan_pattern(_, Types, {application, _, {type, _, T}, Args}) ->
    TDomain = Types:args(T),
    {ArgEnvs, ArgDomains} = unzip([scan_pattern(D, Types, A) || {D, A} <- zip(TDomain, Args)]),
    {intersection(ArgEnvs), Types:T(ArgDomains)};

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
    % This is currently wrong. The domain needs to be specific to the Element in the dict
    {ArgEnvs, ArgDomainList} = unzip([scan_pattern(Domain, Types, E) || E <- Elems]),
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

%pivot([]) -> [];
%pivot(ListOfLists) -> 
%    [[nth(N, List) || List <- ListOfLists] || N <- seq(1,length(nth(1, ListOfLists)))].

fold(Env, Types, Elements) when is_list(Elements) ->
    F = fun(Elem, {EnvAcc, _}) -> 
                {ElemEnv, Domain} = scan(EnvAcc, Types, Elem),
                {union([EnvAcc, ElemEnv]), Domain} 
        end,
    {NewEnv, NewDomain} = lists:foldl(F, {Env, any}, Elements),
    {NewEnv, NewDomain}.
