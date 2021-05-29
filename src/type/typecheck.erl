-module(typecheck).
-import(lists, [zip/2, unzip/1, unzip3/1, seq/2, nth/2]).
-import(domain, [intersection/1, intersection/2, intersect_envs/2, union/1, union/2]).
-export([domain/6, qualified_apply/7]).

-include_lib("eunit/include/eunit.hrl").

domain(Mode, Arities, Stack, Args, DomainModule, Term) ->

    ArgDomains = [case maps:get(A, Arities, undefined) of
                      undefined -> A;
                      0         -> erlang:apply(DomainModule, A, []);
                      Arity     -> EnvF = env_fun(Mode, DomainModule, A, Arity),
                                   EnvF(Stack, element(2, Term))
                   end || A <- Args],
    Env = maps:map(fun(Tag, Arity) -> env_fun(Mode, DomainModule, Tag, Arity) end, Arities),
    def(Mode, Env, Stack, ArgDomains, Term).

% The environment consists of curried functions, taking first a stack and then
% (if the arity is more than 1) whatever parameters is needed
env_fun(Mode, DomainModule, Tag, Arity) ->
    fun(Stack, Ctx) ->
            case Arity of
                0   -> apply_domain(Mode, Stack, Ctx, DomainModule, Tag, []);
                _N  -> F = fun(Params) -> apply_domain(Mode, Stack, Ctx, DomainModule, Tag, Params) end,
                       utils:function(Arity, F)
            end
    end.

def(Mode, Env, Stack, ArgDomains, {'fun', Ctx, Clauses}) ->
    Tags = [symbol:id('') || _ <- ArgDomains],
    case clauses(Mode, Env, Stack, zip(Tags, ArgDomains), Clauses) of
        {error, Errs}           -> {error, Errs};
        {ok, {LEnv, LDomain}}   -> 
            ActualDomains = [maps:get(Tag, LEnv) || Tag <- Tags],
            case check(Mode, Stack, ArgDomains, ActualDomains, Ctx, LDomain) of
                {error, Errs}   -> {error, Errs};
                {ok, Domain}    -> Domain
            end
    end;

def(Mode, Env, Stack, [], Term) -> 
    case expr(Mode, Env, Stack, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {_, Domain}}   -> Domain
    end.

clauses(Mode, Env, Stack, Args, [{clause, Ctx, _, _} | _] = Clauses) ->
    ArgDomains = [ArgD || {_, ArgD} <- Args],
    Scan = fun S([]) -> [];
               S([Clause | Cs]) -> 
                       {E, D, PsDomains} = clause(Mode, Env, Stack, Args, Clause),
                       % Only scan patterns until (and including) the first pattern which domain
                       % is a subset of the pattern domain. There's no point in keeping on
                       % scanning patterns if we can infer from the argument domains that they
                       % will not be reached
                       case domain:subset(ArgDomains, PsDomains) of
                           true -> [{E, D, PsDomains}];
                           false -> [{E, D, PsDomains} | S(Cs)]
                       end
           end,

    {EnvCs, DomainCs, DomainPs} = unzip3(Scan(Clauses)),
    ActualDomains = [union(Ds) || Ds <- pivot(DomainPs)],
    check(Mode, Stack, ArgDomains, ActualDomains, Ctx, {env_union(EnvCs), union(DomainCs)}).

clause(Mode, Env, Stack, Args, {clause, _, Patterns, Expr}) ->
    Scan = fun(ArgTag, Domain, Pattern) -> 
                   % The strictness of a pattern is either `normal` or
                   % `lenient`. A pattern is evaluated at compile time in to a
                   % literal domain.
                   %
                   % When we typecheck using a strictness mode of `strict`, we
                   % check that the set of values that an expression can take
                   % is a subset of possible values which it is allowed to take
                   % given the constraints in place (e.g. a pattern).  When
                   % using a strictness mode of `normal`, we check instead that
                   % the value domain intersects with the constraint domain,
                   % i.e. that some values satisfy the constraints.                   %
                   %
                   % When evaluating code at runtime, having made sure that a
                   % value domain is a subset of the constraints, means that
                   % we're sure that any function called with the value is
                   % defined for that value.
                   %
                   % Patterns on the other hand are evaluated to domain
                   % literals (e.g. a list of possible values) at compile time
                   % and aren't 'executed' at run-time. When we evaluate a
                   % pattern at run-time, we're interested in the fact that a
                   % pattern may match. Even though we might type-check
                   % with the strictness mode set to 'strict', we aren't
                   % interested in whether an expression is strictly a subset
                   % of a patternb.
                   %
                   % There's one complication in that application arguments in
                   % a pattern are treated as expressions. This means that an
                   % argument to an application could itself be an expression
                   % calling a function. Since this evaluation would take time
                   % at compile time, we should only be evaluating the domain
                   % level of any function, which removes the risk of calling a
                   % function for values it isn't defined for. It might be a
                   % faff to make sure that we don't end up passing the
                   % application a bunch of terms that haven't been evaluated
                   % to their domains though.
                   PatternMode = case Mode of lenient -> lenient; _ -> normal end,
                   case pattern(PatternMode, Env, Stack, Domain, Pattern) of
                       {error, _} = Errs    -> {#{}, Errs};
                       {ok, {E, D}}         -> {merge(E, #{ArgTag => D}), D}
                   end end,

    {PsEnvs, PsDomains} = unzip([Scan(ArgTag, ArgDomain, Pattern) || 
                                 {{ArgTag, ArgDomain}, Pattern} <- zip(Args, Patterns)]),
    ClauseEnv = merge([Env | PsEnvs]),
    % If any pattern returns an error, there's no point in scanning the
    % expression since it can't be satisfied under the current constraints
    case lists:member(none, PsDomains) orelse lists:any(fun error:match/1, PsDomains) of
        true    -> {ClauseEnv, none, PsDomains};
        false   ->
            case expr(Mode, ClauseEnv, Stack, Expr) of
                {error, _} = Errs           -> {ClauseEnv, Errs, PsDomains};
                {ok, {ExprEnv, ExprDomain}} -> {merge(ClauseEnv, ExprEnv), ExprDomain, PsDomains}
            end
    end.

pattern(Mode, Env, Stack, {sum, Ds}, Term) ->
    {SumEnvs, SumDomains} = comb([pattern(Mode, Env, Stack, D, Term) || D <- Ds]),
    case union(SumDomains) of
        {error, Errs}   -> {error, Errs};
        D               -> {ok, {env_union(SumEnvs), D}}
    end;

pattern(Mode, Env, Stack, {recur, _F} = D, Term) ->
    pattern(Mode, Env, Stack, domain:unroll(D, element(2, Term)), Term);

pattern(Mode, Env, Stack, any, {list, _, Elems} = Term) ->
    pattern(Mode, Env, Stack, [any || _ <- Elems], Term);
pattern(Mode, Env, Stack, any, {tagged, _, _, _} = Term) ->
    pattern(Mode, Env, Stack, {tagged, symbol:tag(Term), any}, Term);
pattern(Mode, Env, Stack, any, {dict, Ctx, Elems}) ->
    case map_pattern(Mode, Env, Stack, any, Elems) of
        {error, Errs}               -> {error, Errs};
        {ok, {KeyEnvs, KeyDomains}} ->
            case check(Mode, Stack, any, intersection(KeyDomains), Ctx) of
                {error, Errs}   -> {error, Errs};
                {ok, ResDomain} -> {ok, {merge(KeyEnvs), ResDomain}}
            end
    end;

% Pattern of `[T, S, R]`
pattern(Mode, Env, Stack, Domain, {list, _, Elems}) when is_list(Domain) andalso (length(Domain) == length(Elems)) ->
    case map_pattern(Mode, Env, Stack, Domain, Elems) of
        {error, Errs}           -> {error, Errs};
        {ok, {TEnvs, TDomains}} -> {ok, {env_union(TEnvs), TDomains}}
    end;
pattern(Mode, Env, Stack, Domain, {list, Ctx, Elems}) ->
    case map_pattern(lenient, Env, Stack, any, Elems) of
        {error, Errs}           -> {error, Errs};
        {ok, {TEnvs, TDomains}} ->
            case check(Mode, Stack, Domain, TDomains, Ctx) of
                {error, Errs}   -> {error, Errs};
                {ok, Res}       -> {ok, {env_union(TEnvs), Res}}
            end
    end;

% Pattern of `(T | S | R)`
pattern(Mode, Env, Stack, Domain, {sum, _, Elems}) ->
    {SumEnvs, SumDomains} = comb([pattern(Mode, Env, Stack, Domain, E) || E <- Elems]),
    case union(SumDomains) of
        {error, Errs}   -> {error, Errs};
        D               -> {ok, {env_union(SumEnvs), D}}
    end;

% Pattern of `{k: v, ...}`
pattern(Mode, Env, Stack, Domain, {dict, Ctx, Elems}) when is_map(Domain) ->
    Scan = fun(Elem) -> case maps:get(symbol:name(Elem), Domain, Mode) of
                            lenient     -> pattern(Mode, Env, Stack, none, Elem);
                            normal      -> error:format({nonexistent_key, symbol:name(Elem), Domain},
                                                        {typecheck, Ctx, Stack});
                            strict      -> error:format({nonexistent_key, symbol:name(Elem), Domain},
                                                        {typecheck, Ctx, Stack});
                            ElemDomain  -> pattern(Mode, Env, Stack, ElemDomain, Elem)
                        end end,
    case error:collect([Scan(E) || E <- Elems]) of
        {error, Errs}   -> {error, Errs};
        {ok, Res}       ->
            {KeyEnvs, KeyDomains} = unzip(Res),
            {ok, {merge(KeyEnvs), intersection([Domain | KeyDomains])}}
    end;
pattern(Mode, Env, Stack, Domain, {dict, Ctx, Elems}) ->
    case map_pattern(lenient, Env, Stack, any, Elems) of
        {error, Errs}           -> {error, Errs};
        {ok, {TEnvs, TDomains}} ->
            case check(Mode, Stack, Domain, intersection(TDomains), Ctx) of
                {error, Errs}   -> {error, Errs};
                {ok, Res}       -> {ok, {env_union(TEnvs), Res}}
            end
    end;

% Pattern of: `T: S` (tagged type)
pattern(Mode, Env, Stack, Domain, {tagged, Ctx, _, Val} = T) ->
    Tag = symbol:tag(T),
    case Domain of
        {tagged, Tag, D} ->
            case pattern(Mode, Env, Stack, D, Val) of
                {error, Errs}            -> {error, Errs};
                {ok, {TEnv, TDomain}}    -> {ok, {TEnv, {tagged, Tag, TDomain}}}
            end;
        _  ->
            case pattern(lenient, Env, Stack, any, Val) of
                {error, Errs}           -> {error, Errs};
                {ok, {TEnv, TDomain}}   ->
                    case check(Mode, Stack, Domain, {tagged, Tag, TDomain}, Ctx) of
                        {error, Errs}   -> {error, Errs};
                        {ok, Res}       -> {ok, {TEnv, Res}}
                    end
            end
    end;

% Pattern of `v: T`, e.g. an alias for a variable `v` to value of pattern `T`
pattern(Mode, Env, Stack, Domain, {pair, _, Key, Val}) ->
    case pattern(Mode, Env, Stack, Domain, Val) of
        {error, Errs}               -> {error, Errs};
        {ok, {ValEnv, ValDomain}}   -> 
            case pattern(Mode, Env, Stack, ValDomain, Key) of
                {error, Errs}               -> {error, Errs};
                {ok, {KeyEnv, KeyDomain}}   -> 
                    {ok, {merge(ValEnv, KeyEnv), intersection(KeyDomain, ValDomain)}}
            end
    end;

% Pattern of `{ k: T, ... }`
pattern(Mode, Env, Stack, Domain, {dict_pair, _, Key, Val}) ->
    case pattern(Mode, Env, Stack, Domain, Val) of
        {error, Errs}               -> {error, Errs};
        {ok, {ValEnv, ValDomain}}   -> {ok, {ValEnv, #{symbol:name(Key) => ValDomain}}}
    end;

% Pattern: `T(a)` or `f(b)`
pattern(Mode, Env, Stack, Domain, {application, Ctx, _, _} = Term) ->
    % The arguments of a function application are interpreted as an expression
    % and not as a pattern. This allows us to pass other variables in scope as
    % arguments.
    case expr(Mode, Env, Stack, Term) of
        {error, Errs}           -> {error, Errs};
        {ok, {_, AppDomain}}    ->
        case check(Mode, Stack, AppDomain, Domain, Ctx) of
            {error, Errs}       -> {error, Errs};
            {ok, ResDomain}     -> {ok, {#{}, ResDomain}}
        end
    end;

% Pattern: `kind/prelude/Boolean`
pattern(Mode, Env, Stack, Domain, {qualified_symbol, Ctx, _, _} = Term) ->
    case expr(Mode, Env, Stack, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {_, QDomain}}   ->
            case check(Mode, Stack, QDomain, Domain, Ctx) of
                {error, Errs}       -> {error, Errs};
                {ok, Res}           -> {ok, {#{}, Res}}
            end
    end;

% Pattern: `kind/prelude/Option(T)`
pattern(Mode, Env, Stack, Domain, {qualified_application, Ctx, _, _, _} = Term) ->
    case expr(Mode, Env, Stack, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {_, QDomain}}   ->
            case check(Mode, Stack, QDomain, Domain, Ctx) of
                {error, Errs}       -> {error, Errs};
                {ok, Res}           -> {ok, {#{}, Res}}
            end
    end;


% Pattern: `1` or `'atom'` or `"string"`
pattern(Mode, _, Stack, Domain, {value, Ctx, _, Val}) -> 
    case check(Mode, Stack, Domain, Val, Ctx) of
        {error, Errs}   -> {error, Errs};
        {ok, Res}       -> {ok, {#{}, Res}}
    end;

% Pattern: `T`
pattern(Mode, Env, Stack, Domain, {keyword, Ctx, _, _} = T) ->
    EnvDomain = symbol:tag(T),
    case check(Mode, Stack, EnvDomain, Domain, Ctx) of
        {error, Errs}    -> {error, Errs};
        {ok, Res}        -> {ok, {#{}, Res}}
    end;

% Pattern: `a`
pattern(Mode, Env, Stack, Domain, {variable, Ctx, Name, Tag}) ->
    % The tagger tags a top-level variable with it's fname (e.g. {f, 1})
    % For consistency, the env stores both types and defs without arity
    case maps:get(Tag, Env, maps:get(Name, Env, undefined)) of
        undefined   -> {ok, {#{Tag => fun(_Stack, _Ctx) -> Domain end}, Domain}};
        EnvF        ->
            case EnvF(Stack, Ctx) of
                {error, Errs}   -> {error, Errs};
                EnvDomain       ->
                    case check(Mode, Stack, EnvDomain, Domain, Ctx) of
                        {error, Errs}    -> {error, Errs};
                        {ok, ResDomain}  -> {ok, {#{Tag => fun(_Stack, _Ctx) -> ResDomain end}, ResDomain}}
                    end
            end
    end.



% Expr: `m/T(a)` or `m/f(b)`
expr(Mode, Env, Stack, {qualified_application, Ctx, ModulePath, Name, Args}) ->
    case map_expr(Mode, Env, Stack, Args) of
        {error, Errs}         -> {error, Errs};
        {ok, {_, ArgDomains}} -> qualified_apply(Mode, Env, Stack, Ctx, ModulePath, Name, ArgDomains)
    end;

% Expr: `m/T(a)` or `m/f(b)`
expr(Mode, Env, Stack, {beam_application, Ctx, ModulePath, Name, Args}) ->
    case map_expr(Mode, Env, Stack, Args) of
        {error, Errs}         -> {error, Errs};
        {ok, {_, ArgDomains}} -> beam_apply(Env, ModulePath, Name, ArgDomains)
    end;

% Expr: `kind/prelude/Boolean`
expr(Mode, Env, Stack, {qualified_symbol, Ctx, ModulePath, Name}) ->
    ModuleName = module:beam_name(ModulePath),
    Arity = utils:get_min_arity(ModuleName, Name),
    case Arity =:= 0 of
        true    -> qualified_apply(Mode, Env, Stack, Ctx, ModulePath, Name, []);
        false   -> error:format({wrong_arity, module:kind_name(ModulePath), Name, 0, Arity},
                                {Stack, Ctx, typecheck})
    end;

expr(Mode, Env, Stack, {seq, _Ctx, Expr1, Expr2}) ->
    case expr(Mode, Env, Stack, Expr1) of
        {error, Errs}           -> {error, Errs};
        {ok, _}                 -> expr(Mode, Env, Stack, Expr2)
    end;

% Expr: `val x = { ... }
%        x + 4`
expr(Mode, Env, Stack, {'let', Ctx, Pattern, Expr, Term}) ->
    case expr(Mode, Env, Stack, Expr) of
        {error, Errs}           -> {error, Errs};
        {ok, {_, ExprDomain}}   ->
            case pattern(Mode, Env, Stack, ExprDomain, Pattern) of
                {error, Errs}                       -> {error, Errs};
                {ok, {PatternEnv, PatternDomain}}  ->
                    case check(Mode, Stack, ExprDomain, PatternDomain, Ctx) of
                        {error, Errs}   -> {error, Errs};
                        {ok, _}         ->
                            NewEnv = merge(Env, PatternEnv),
                            expr(Mode, NewEnv, Stack, Term)
                    end
            end
    end;

% Expr: `| T -> { ... }
%        | X -> { ....}`
expr(Mode, Env, Stack, {'fun', Ctx, [{clause, _, Ps, _} | _]} = Fun) ->
    Arity = length(Ps),
    F = utils:function(Arity, fun(ArgDomains) ->
                                      % I like this pattern and find it
                                      % delightfully cheeky.
                                      % As part of the stack I need a name for
                                      % the anonymous function. To do so, I
                                      % generate a function that takes a stack
                                      % as an argument and use the erlang name
                                      % of this function to name the name
                                      % function before calling it with the
                                      % newly constructed stack that I've made
                                      F = fun(_LocalStack) -> def(Mode, Env, Stack, ArgDomains, Fun) end,
                                      NewStack = [{utils:gen_tag(F), Ctx, ArgDomains} | Stack],
                                      F(NewStack)
                              end),
    {ok, {#{}, F}};

% Expr: `T(a)` or `f(b)`
expr(Mode, Env, Stack, {application, Ctx, Expr, Args}) ->

    case map_expr(Mode, Env, Stack, Args) of
        {error, Errs}           -> {error, Errs};
        {ok, {_, ArgDomains}}   ->
            case expr(Mode, Env, Stack, Expr) of
                {error, Errs}           -> {error, Errs};
                {ok, {_, ExprDomain}}   -> 
                    FunctionMode = case Mode of lenient -> lenient; _ -> normal end,
                    case check(FunctionMode, Stack, ExprDomain, domain:function(length(Args)), Ctx) of
                        {error, _}       ->
                            case is_function(ExprDomain) of
                                false -> error:format({expected_function_domain, ExprDomain},
                                                      {typecheck, Ctx, Stack});
                                true  -> Arity = utils:get_arity(ExprDomain),
                                         error:format({wrong_arity, symbol:tag(Expr), length(Args), Arity},
                                                      {typecheck, Ctx, Stack})
                            end;
                        {ok, none}       -> {ok, {#{}, none}}; % In case Mode is lenient
                        {ok, _ResDomain} ->
                            case ExprDomain of
                                any -> {ok, {#{}, any}};
                                _   -> 
                                    case erlang:apply(ExprDomain, ArgDomains) of
                                        {error, Errs}    -> {error, Errs};
                                        Domain           -> {ok, {#{}, Domain}}
                                    end
                            end
                    end
            end
    end;

% Expr of form: `T(a)` where `T` is a recursive type (and so we want to not
% call the type constructor in an infinite loop
expr(Mode, Env, Stack, {recursive_type_application, Ctx, Tag, Args}) ->
    F = fun() ->
                T = {type, Ctx, Tag, [Tag]},
                case expr(Mode, Env, Stack, {application, Ctx, T, Args}) of
                    {error, Errs}       -> {error, Errs};
                    {ok, {_, Domain}}   -> Domain
                end
        end,
    {ok, {#{}, {recur, F}}};

% expr of form `T` where T is recursive like `type T -> {a: Boolean, b: T}`
expr(Mode, Env, Stack, {recursive_type, Ctx, Name, Path}) ->
    F = fun() ->
                case expr(Mode, Env, Stack, {type, Ctx, Name, Path}) of
                    {error, Errs}       -> {error, Errs};
                    {ok, {_, Domain}}   -> Domain
                end
        end,
    {ok, {#{}, {recur, F}}};

% Expr: `T : S` (tagged type)
expr(Mode, Env, Stack, {tagged, _, _, Value} = T) ->
    case expr(Mode, Env, Stack, Value) of
        {error, Errs}           -> {error, Errs};
        {ok, {VEnv, VDomain}}   -> {ok, {VEnv, {tagged, symbol:tag(T), VDomain}}}
    end;

% Expr: `s : T`
expr(Mode, Env, Stack, {pair, Ctx, Key, Value}) ->
    error:flatmap2(expr(Mode, Env, Stack, Value),
                   expr(Mode, Env, Stack, Key),
                   fun({_, ValueDomain}, {_, KeyDomain}) ->
                           case check(Mode, Stack, KeyDomain, ValueDomain, Ctx) of
                               {error, _}   -> error:format({pair_not_subset, KeyDomain, ValueDomain},
                                                            {typecheck, Ctx, Stack});
                               {ok, Domain} -> {ok, {#{}, Domain}}
                           end
                   end);

% Pair in Expr: `{s : T, ...}`
expr(Mode, Env, Stack, {dict_pair, _, Key, Value}) ->
    case expr(Mode, Env, Stack, Value) of
        {error, Errs}           -> {error, Errs};
        {ok, {_, ValDomain}}    -> {ok, {#{}, #{symbol:name(Key) => ValDomain}}}
    end;

% Expr: `{k: R}`
expr(Mode, Env, Stack, {dict, Ctx, Elems}) ->
    case utils:duplicates(Elems, fun symbol:name/1) of
        []          ->
            case map_expr(Mode, Env, Stack, Elems) of
                {error, Errs}           -> {error, Errs};
                {ok, {_, ElemsDomain}}  -> {ok, {#{}, intersection(ElemsDomain)}}
            end;
        Duplicates  -> 
            Keys = lists:map(fun({T, _}) -> symbol:name(T) end, Duplicates),
            error:format({duplicate_keys, Keys},
                                    {typecheck, Ctx, Stack})
    end;

% Expr: `[T, S, ...]`
expr(Mode, Env, Stack, {list, _, Elems}) ->
    case map_expr(Mode, Env, Stack, Elems) of
        {error, Errs}           -> {error, Errs};
        {ok, {_, ElemsDomain}}  -> {ok, {#{}, ElemsDomain}}
    end;

% Expr: `T | S`
expr(Mode, Env, Stack, {sum, _, Elems}) ->
    case map_expr(Mode, Env, Stack, Elems) of
        {error, Errs}           -> {error, Errs};
        {ok, {_, ElemsDomain}}  -> {ok, {#{}, union(ElemsDomain)}}
    end;

% Expr: `1` or `'atom'` or `"string"`
expr(_, _, _, {value, _, _, Val}) -> {ok, {#{}, Val}};

% Expr: `T`
expr(_, Env, Stack, {type, Ctx, _, _} = T) ->
    Tag = symbol:tag(T),
    EnvRes = case maps:get(Tag, Env, undefined) of
                     undefined   -> Tag;
                     EnvF        -> EnvF(Stack, Ctx)
                 end,
    case EnvRes of
        {error, Errs}   -> {error, Errs};
        EnvDomain       -> {ok, {#{}, EnvDomain}}
    end;

% Expr: `a`
expr(_, Env, Stack, {variable, Ctx, Name, Tag}) -> 
    % The tagger tags a top-level variable with it's fname (e.g. {f, 1})
    % For consistency, the env stores both types and defs without arity
    case maps:get(Tag, Env, maps:get(Name, Env, undefined)) of
        undefined   -> error:format({undefined_variable, Name}, {typecheck, Ctx, Stack});
        EnvF        ->
            case EnvF(Stack, Ctx) of
                {error, Errs}   -> {error, Errs};
                EnvDomain       -> {ok, {#{Tag => fun(_Stack, _Ctx) -> EnvDomain end}, EnvDomain}}
            end
    end.

map_expr(Mode, Env, Stack, Terms) ->
    case error:collect([expr(Mode, Env, Stack, T) || T <- Terms]) of
        {error, Errs}   -> {error, Errs};
        {ok, Res}       -> {ok, unzip(Res)}
    end.

map_pattern(Mode, Env, Stack, Domain, Terms) when not(is_list(Domain)) ->
    map_pattern(Mode, Env, Stack, [Domain || _ <- Terms], Terms);
map_pattern(Mode, Env, Stack, Domains, Terms) when length(Domains) == length(Terms) ->
    case error:collect([pattern(Mode, Env, Stack, D, T) || {D, T} <- zip(Domains, Terms)]) of
        {error, Errs}   -> {error, Errs};
        {ok, Res}       -> {TEnvs, TDomains} = unzip(Res),
                           {ok, {TEnvs, TDomains}}
    end.

check(Strictness, Stack, D1, D2, Ctx) -> check(Strictness, Stack, D1, D2, Ctx, intersection(D1, D2)).

check(Strictness, Stack, L1, L2, Ctx, Ret) when is_list(L1) andalso is_list(L2) andalso length(L1) == length(L2) ->
    case error:collect([check(Strictness, Stack, D1, D2, Ctx, {}) || {D1, D2} <- zip(L1, L2)]) of
        {error, Errs}   -> {error, Errs};
        {ok, _}         -> {ok, Ret}
    end;

check(strict, Stack, D1, D2, Ctx, Ret) ->
    F = fun(T1, T2) ->
                case domain:subset(T1, T2) of
                    true    -> {ok, Ret};
                    false   -> error:format({domain_not_subset, T1, T2}, 
                                            {typecheck, Ctx, Stack})
                end
        end,
    error:flatmap2(D1, D2, F);

check(normal, Stack, D1, D2, Ctx, Ret) ->
    F = fun(T1, T2) ->
                case intersection(T1, T2) =:= none of
                    false   -> {ok, Ret};
                    true    -> error:format({domains_do_not_intersect, T1, D2}, 
                                            {typecheck, Ctx, Stack})
                end
        end,
    error:flatmap2(D1, D2, F);

check(lenient, _, D1, D2, _, Ret) -> error:map2(D1, D2, fun(_, _) -> Ret end).

pivot([]) -> [];
pivot([H | _] = ListOfLists) ->
    Rev = lists:foldl(fun(Elems, Accs) -> [[E | Acc] || {E, Acc} <- zip(Elems, Accs)] end,
                      [[] || _ <- H],
                      ListOfLists),
    [lists:reverse(L) || L <- Rev].

apply_domain(Mode, Stack, Ctx, Module, Tag, Params) ->
    NewStack = [{module:beam_name([Module, Tag]), Ctx, Params} | Stack],
    case check_stack_recursion(NewStack) of
        error   -> {recur, fun () -> erlang:apply(Module, Tag, [NewStack, Mode] ++ Params) end};
        ok      -> erlang:apply(Module, Tag, [NewStack, Mode] ++ Params)
    end.



% Qualified apply is tricky because the appropriate domain function can take
% a few different shapes:
%
% - In the normal case a domain function for a kind function or type will live
%   in the corresponding domain module
% - For a sub-type defined in a type module, the domain function is the same as
%   the type function
% - When all arguments are literal domains and the function called is an erlang
%   function, we can call the erlang function, but only if it doesn't have
%   side-effects
qualified_apply(Mode, Env, Stack, Ctx, ModulePath, Name, ArgDomains) ->
    ModuleName = module:beam_name(ModulePath),
    NewStack = [{module:beam_name(ModulePath ++ [Name]), Ctx, ArgDomains} | Stack],
    DomainModuleName = module:beam_name(ModulePath ++ [domain]),
    % Check if domain function exists
    case erlang:function_exported(DomainModuleName, Name, length(ArgDomains)) of
        false   -> error:format({undefined_qualified_symbol, module:kind_name(ModulePath ++ [domain]),
                                 {Name, length(ArgDomains)}},
                                {typecheck, Ctx, Stack});
        true    -> case erlang:apply(DomainModuleName, Name, [NewStack, Mode] ++ ArgDomains) of
                       {error, Errs}       -> {error, Errs};
                       Res                 -> {ok, {#{}, Res}}
                   end
    end.

% If the domain is literal and the function is whitelisted, call it directly
% otherwise, return the `any` domain
beam_apply(Env, ModulePath, Name, ArgDomains) ->
    ModuleName = module:beam_name(ModulePath),
    case domain:is_literal(Env, ArgDomains) of
        true    -> case import:is_whitelisted(ModuleName, Name) of
                       true     -> {ok, {#{}, erlang:apply(ModuleName, Name, ArgDomains)}};
                       false    -> {ok, {#{}, any}}
                   end;
        false   -> {ok, {#{}, any}}
    end.

check_stack_recursion(Stack) -> check_stack_recursion(Stack, #{}).
check_stack_recursion([], _) -> ok;
check_stack_recursion([{Name, _, Domains} | Tail], Seen) ->
    case maps:is_key({Name, Domains}, Seen) of
        true    -> error;
        false   -> check_stack_recursion(Tail, maps:put({Name, Domains}, true, Seen))
    end.

merge(E1, E2) -> 
    F = fun(K, _) -> case {maps:is_key(K, E1), maps:is_key(K, E2)} of
                         {true, true} -> intersection(maps:get(K, E1), maps:get(K, E2));
                         {false, true} -> maps:get(K, E2);
                         {true, false} -> maps:get(K, E1)
                     end
        end,
    maps:map(F, maps:merge(E1, E2)).


merge(Envs) when is_list(Envs) -> lists:foldl(fun(E1,E2) -> merge(E1, E2) end, #{}, Envs).

env_union(Envs) when is_list(Envs) -> lists:foldl(fun(E1, E2) -> env_union(E1, E2) end, #{}, Envs).
env_union(E1, E2) ->
    F = fun(K, _) -> case {maps:is_key(K, E1), maps:is_key(K, E2)} of
                         {true, true} -> union(maps:get(K, E1), maps:get(K, E2));
                         {false, true} -> maps:get(K, E2);
                         {true, false} -> maps:get(K, E1)
                     end
        end,
    maps:map(F, maps:merge(E1, E2)).

comb(L) when is_list(L) -> comb(L, []).
comb([], Acc) -> unzip(lists:reverse(Acc));
comb([{error, _} = Err | Tail], Acc) -> comb(Tail, [{#{}, Err} | Acc]);
comb([{ok, {E, D}} | Tail], Acc) -> comb(Tail, [{E, D} | Acc]).
