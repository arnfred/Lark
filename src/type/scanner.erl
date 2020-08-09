-module(scanner).
-import(lists, [zip/2, unzip/1, unzip3/1, seq/2, nth/2]).
-import(domain, [intersection/1, intersection/2, union/1, union/2]).
-export([scan/2]).

scan(TypeMod, {ast, _, _, _, Defs}) ->
    Scanned = [scan_top_level(TypeMod, Def) || {def, _, _, _, _} = Def <- maps:values(Defs)],
    {_, Names, Domains} = unzip3([EnvFun(Scanned) || EnvFun <- Scanned]),
    maps:from_list(zip(Names, Domains)).

scan_top_level(TypeMod, {def, _, Name, Args, Cs} = Def) when is_list(Cs) ->
    F = fun(Env, Stack, ArgDomains) ->
                NewStack = [{Name, ArgDomains} | Stack],
                ClauseArgs = zip(Args, ArgDomains),
                {DefEnv, DefDomain} = scan_clauses(Env, NewStack, TypeMod, ClauseArgs, Cs),
                ActualDomains = [maps:get(Tag, DefEnv) || {_, _, _, Tag} <- Args],
                Error = error:format({arguments_not_subsets, ArgDomains, ActualDomains},
                                     {scanner, Def, NewStack}), 
                IsSubset = fun({D1, D2}) -> domain:subset(D1, D2) end,
                case lists:all(IsSubset, zip(ArgDomains, ActualDomains)) of
                    false      -> error:leftbias(DefDomain, Error);
                    true       -> DefDomain
                end
        end,
    generate_env_fun(Name, length(Args), F, TypeMod);

scan_top_level(TypeMod, {def, _, Name, Args, Expr} = Def) ->
    F = fun(Env, Stack, ArgDomains) -> 
                NewStack = [{Name, ArgDomains} | Stack],
                ArgEnv = maps:from_list([{symbol:tag(A), D} || {A, D} <- zip(Args, ArgDomains)]),
                NewEnv = intersection(Env, ArgEnv),
                {DefEnv, DefDomain} = scan(NewEnv, NewStack, TypeMod, Expr),
                ActualDomains = [maps:get(Tag, DefEnv) || {_, _, _, Tag} <- Args],
                Error = error:format({arguments_not_subsets, ArgDomains, ActualDomains},
                                     {scanner, Def, NewStack}),
                IsSubset = fun({D1, D2}) -> domain:subset(D1, D2) end,
                case lists:all(IsSubset, zip(ArgDomains, ActualDomains)) of
                    false      -> error:leftbias(DefDomain, Error);
                    true       -> DefDomain
                end
        end,
    generate_env_fun(Name, length(Args), F, TypeMod).

scan(Env, Stack, TypeMod, {application, _, Expr, Args} = Parsnip) ->
    {ArgEnvs, ArgDomains} = unzip([scan(Env, Stack, TypeMod, A) || A <- Args]),
    {ExprEnv, ExprDomain} = scan(Env, Stack, TypeMod, Expr),
    Domain = apply_domain(ExprDomain, Stack, ArgDomains, Parsnip),
    {intersection([ExprEnv | ArgEnvs]), Domain};

scan(Env, Stack, TypeMod, {pair, _, Key, Value} = Pair) ->
    {ValueEnv, ValueDomain} = scan(Env, Stack, TypeMod, Value),
    {KeyEnv, KeyDomain} = scan(Env, Stack, TypeMod, Key),
    Domain = intersection(ValueDomain, KeyDomain),
    NewEnv = intersection(KeyEnv, ValueEnv),
    Error = error:format({pair_not_subset, KeyDomain, ValueDomain}, {scanner, Pair, Stack}),
    case domain:subset(KeyDomain, ValueDomain) of
        false      -> {NewEnv, error:leftbias(Domain, Error)};
        true       -> {NewEnv, Domain}
    end;

scan(Env, _, TypeMod, {lambda, Ctx, [{clause, _, Ps, _} | _] = Cs} = Lambda) -> 
    Name = symbol:id(['lambda', list_to_atom(integer_to_list(maps:get(line, Ctx)))]),
    F = fun(Stack, ArgDomains) -> 
                NewStack = [{Name, ArgDomains} | Stack],
                Tags = [symbol:id(Name) || _ <- ArgDomains],
                Args = [{{variable, 0, '', Tag}, ArgD} || {Tag, ArgD} <- zip(Tags, ArgDomains)],
                {LEnv, LDomain} = scan_clauses(Env, NewStack, TypeMod, Args, Cs),
                IsSubset = fun({D1, D2}) -> domain:subset(D1, D2) end,
                ActualDomains = [maps:get(Tag, LEnv) || Tag <- Tags],
                Error = error:format({arguments_not_subsets, ArgDomains, ActualDomains}, 
                                     {scanner, Lambda, NewStack}),
                case lists:all(IsSubset, zip(ArgDomains, ActualDomains)) of
                    false      -> error:leftbias(LDomain, Error);
                    true       -> LDomain
                end
        end,
    {Env, two_step_pass_stack(F, Name, length(Ps))};

scan(Env, Stack, TypeMod, {val, _, Pattern, Expr}) ->
    {_, ExprDomain} = scan(Env, Stack, TypeMod, Expr),
    {PatternEnv, PatternDomain} = scan_pattern(ExprDomain, TypeMod, Stack, Pattern),
    NewEnv = intersection(Env, PatternEnv),
    NewDomain = intersection(PatternDomain, ExprDomain),
    {NewEnv, NewDomain};

scan(Env, Stack, TypeMod, {'let', _, Pattern, Expr, Term}) ->
    {_, ExprDomain} = scan(Env, Stack, TypeMod, Expr),
    {PatternEnv, PatternDomain} = scan_pattern(ExprDomain, TypeMod, Stack, Pattern),
    case PatternDomain of
        {error, Errs} -> {Env, {error, Errs}};
        _             -> NewEnv = intersection(Env, PatternEnv),
                         {TermEnv, TermDomain} = scan(NewEnv, Stack, TypeMod, Term),
                         {intersection(NewEnv, TermEnv), TermDomain}
    end;

scan(Env, Stack, TypeMod, {seq, _, Expr1, Expr2}) ->
    {Expr1Env, _} = scan(Env, Stack, TypeMod, Expr1),
    NewEnv = intersection(Env, Expr1Env),
    {Expr2Env, Expr2Domain} = scan(NewEnv, Stack, TypeMod, Expr2),
    {intersection(Expr1Env, Expr2Env), Expr2Domain};

scan(Env, Stack, TypeMod, {tuple, _, Elems}) -> fold(Env, Stack, TypeMod, Elems);

scan(Env, _, _, {variable, _, _, Tag}) -> 
    io:format("variable Env for ~p: ~p~n", [Tag, Env]),
    D = maps:get(Tag, Env, any),
    {intersection(Env, #{Tag => D}), D};

scan(Env, _, _, {key, _, _}) -> {Env, any};

scan(Env, _, TypeMod, {type, _, _, _} = T) -> 
    Domain = case TypeMod:domain(symbol:tag(T)) of
                 {f, Name, F}   -> two_step_ignore_stack(F, Name);
                 D              -> D end,
    {Env, Domain};

scan(Env, _, _, {qualified_type, _, S}) ->
    D = maps:get(S, Env, any),
    {intersection(Env, #{S => D}), D};

scan(Env, _, _, {qualified_symbol, _, S}) ->
    D = maps:get(S, Env, any),
    {intersection(Env, #{S => D}), D}.

scan_clauses(Env, Stack, TypeMod, Args, Clauses) ->
    IsSubset = fun({Darg, Dpat}) -> domain:subset(Darg, Dpat) end,
    ArgDomains = [ArgD || {_, ArgD} <- Args],
    Scan = fun S([]) -> [];
               S([Clause | Cs]) -> 
	               {E, D, PsDomains} = scan_clause(Env, Stack, TypeMod, Args, Clause),
                   % Only scan patterns until (and including) the first pattern which domain
                   % is a subset of the pattern domain. There's no point in keeping on
                   % scanning patterns if we can infer from the argument domains that they
                   % will not be reached
                   case lists:all(IsSubset, zip(ArgDomains, PsDomains)) of
                       true -> [{E, D, PsDomains}];
                       false -> [{E, D, PsDomains} | S(Cs)]
                   end
           end,

    {EnvCs, DomainCs, DomainPs} = unzip3(Scan(Clauses)),

    % Check that arguments are a subset of the union of clause domains
    ActualDomains = [union(Ds) || Ds <- pivot(DomainPs)],
    Error = error:format({arguments_not_subsets, ArgDomains, ActualDomains},
                         {scanner, Clauses, Stack}),
    case lists:all(IsSubset, zip(ArgDomains, ActualDomains)) of
        false -> {union(EnvCs), error:leftbias(union(DomainCs), Error)};
        true -> {union(EnvCs), union(DomainCs)}
    end.

scan_clause(Env, Stack, TypeMod, Args, {clause, _, Patterns, Expr}) ->
    Scan = fun(Arg, Domain, Pattern) ->
                   {E, D} = scan_pattern(Domain, TypeMod, Stack, Pattern),
                   {intersection(E, #{symbol:tag(Arg) => D}), D}
           end,
    {PsEnvs, PsDomains} = unzip([Scan(Arg, ArgDomain, Pattern) || 
                                 {{Arg, ArgDomain}, Pattern} <- zip(Args, Patterns)]),

    % If any pattern returns an error, there's no point in scanning the
    % expression since it can't be satisfied under the current constraints
    ClauseEnv = intersection([Env | PsEnvs]),
    case error:collect(PsDomains) of
        {error, E}  -> {ClauseEnv, {error, E}, PsDomains};
        _           -> {ExprEnv, ExprDomain} = scan(ClauseEnv, Stack, TypeMod, Expr),
                       {ExprEnv, ExprDomain, PsDomains}
    end.

% Pattern of: `T(a, b)`
scan_pattern(Domain, TypeMod, Stack, {application, _, {type, _, _, T}, Args}) ->
    {ArgEnvs, ArgDomains} = unzip([scan_pattern(any, TypeMod, Stack, A) || A <- Args]),
    {f, _, TDomainFun} = TypeMod:domain(T), 
    {intersection(ArgEnvs), intersection(Domain, TDomainFun(ArgDomains))};

% Pattern of: `{ k: v, ... }`
scan_pattern(Domain, TypeMod, Stack, {dict, _, Keys} = Dict) ->

    ErrContext = {scanner, Dict, Stack},
    case get_domain_map(Domain, Keys, ErrContext) of
        none                 -> 
            Err = error:format({non_dict_lookup, Domain}, ErrContext),
            {KeyEnvs, KeyDomains} = unzip([scan_pattern(Err, TypeMod, Stack, K) || K <- Keys]),
            {intersection(KeyEnvs), intersection(KeyDomains)};

        {error, Errs}        -> 
            {KeyEnvs, KeyDomains} = unzip([scan_pattern({error, Errs}, TypeMod, Stack, K) || K <- Keys]),
            {intersection(KeyEnvs), intersection(KeyDomains)};

        Map when is_map(Map) ->
            Scan = fun(Key) -> case maps:get(symbol:name(Key), Map) of
                                   KeyDomain -> scan_pattern(KeyDomain, TypeMod, Stack, Key)
                               end end,
            {KeyEnvs, KeyDomainList} = unzip([Scan(K) || K <- Keys]),
            KeyDomain = maps:from_list([{symbol:name(K), D} || {K, D} <- zip(Keys, KeyDomainList)]),

            % If any of the dictionary elements cause a `nonexistent_key`
            % error, only those errors should be propagated
            case error:collect(KeyDomainList) of
                {error, Errs} -> {intersection(KeyEnvs), {error, Errs}};
                _             -> 
                    RetDomain = intersection({product, Map}, {product, KeyDomain}),
                    {intersection(KeyEnvs), RetDomain}
            end
    end;

% Pattern of: `(Pattern)`
scan_pattern(Domain, TypeMod, Stack, {tuple, _, [Elem]}) ->
    scan_pattern(Domain, TypeMod, Stack, Elem);

% Pattern of: `(Pattern | Pattern)`, e.g. a sum of types
scan_pattern(Domain, TypeMod, Stack, {tuple, _, Elements}) ->
    {TupleEnvs, TupleDomains} = unzip([scan_pattern(Domain, TypeMod, Stack, E) || E <- Elements]),
    {union(TupleEnvs), union(TupleDomains)};

% Pattern of: `T` for some defined type `T`
scan_pattern(Domain, TypeMod, Stack, {type, _, _, _} = T) -> 
    TDomain = TypeMod:domain(symbol:tag(T)),

    % Why checking for intersection and not if `T` is a subset of `Domain`?
    % ---------------------------------------------------------------------
    % Because at compile time, we don't know what value a variable can have,
    % and for that reason which branch of the pattern match it might take. If a
    % branch is marked as an error because it isn't a subset at compile time,
    % we lose the domain information of the variable, should it end up having a
    % value that would lead it down this branch.
    case intersection(Domain, TDomain) of
        none -> {#{}, error:format({no_intersection, Domain, TDomain}, {scanner, T, Stack})};
        D -> {#{}, D}
    end;

% Pattern of `v` for any non-type
scan_pattern(Domain, _, _, {variable, _, _, _} = Var) -> {#{symbol:tag(Var) => Domain}, Domain};

% Pattern of `T: {k: S}`
scan_pattern(Domain, TypeMod, Stack, {pair, _, {type, _, _, _} = T, Val} = P) ->
    {_, TDomain} = scan_pattern(Domain, TypeMod, Stack, T),
    case TDomain of
        {tagged, Tag, D}    -> {ValEnv, ValDomain} = scan_pattern(D, TypeMod, Stack, Val),
                               {ValEnv, {tagged, Tag, intersection(D, ValDomain)}};
        {error, Errs}       -> {#{}, {error, Errs}};
        D                   -> {#{}, error:format({expected_tagged_domain, TDomain, D, symbol:tag(T)}, {scanner, P, Stack})}
    end;

% Pattern of `k: T`
scan_pattern(Domain, TypeMod, Stack, {pair, _, Key, Val}) ->
    {ValEnv, ValDomain} = scan_pattern(Domain, TypeMod, Stack, Val),
    {intersection(ValEnv, #{symbol:tag(Key) => ValDomain}), ValDomain}.


get_domain_map({product, M}, [], _) -> M;
get_domain_map({product, M} = P, [K | Keys], ErrContext) -> 
    case maps:is_key(symbol:name(K), M) of
        true -> get_domain_map(P, Keys, ErrContext);
        false -> intersection(error:format({nonexistent_key, symbol:name(K), P}, ErrContext), 
                              get_domain_map(P, Keys, ErrContext))
    end;
get_domain_map({tagged, _, D}, Keys, ErrContext) -> get_domain_map(D, Keys, ErrContext);
get_domain_map({recur, F}, Keys, ErrContext) -> get_domain_map(F(), Keys, ErrContext);
get_domain_map({sum, Ds}, Keys, ErrContext) -> union([get_domain_map(D, Keys, ErrContext) || D <- Ds]);
get_domain_map({error, _} = E, _, _) -> E;
get_domain_map(_, _, _) -> none.


fold(Env, Stack, TypeMod, Elements) when is_list(Elements) ->
    F = fun(Elem, {EnvAcc, DomainAcc}) -> 
                {ElemEnv, Domain} = scan(EnvAcc, Stack, TypeMod, Elem),
                {intersection([EnvAcc, ElemEnv]), [Domain | DomainAcc]} 
        end,
    %{NewEnv, [Domain | _]} = lists:foldl(F, {Env, any}, Elements),
    {NewEnv, Domains} = lists:foldl(F, {Env, []}, Elements),
    case error:collect(Domains) of
        {ok, [Domain | _]} -> {NewEnv, Domain};
        {error, Errs} -> {NewEnv, {error, Errs}}
    end.


pivot([]) -> [];
pivot([H | _] = ListOfLists) ->
    Rev = lists:foldl(fun(Elems, Accs) -> [[E | Acc] || {E, Acc} <- zip(Elems, Accs)] end,
                      [[] || _ <- H],
                      ListOfLists),
    [lists:reverse(L) || L <- Rev].

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

get_type(Atom) when is_atom(Atom) -> {type, 0, '_', get_type(atom_to_list(Atom))};
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
apply_domain(D, Stack, _, Parsnip) -> 
    error:leftbias(D, {error, [{Parsnip, {expected_function_domain, D}, Stack}]}).

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



