-module(linearize).
-import(lists, [zip/2, zip3/3, unzip/1, unzip3/1, seq/2, nth/2]).
-import(domain, [subset/2, intersection/1, intersection/2, intersect_envs/2, union/1, union/2]).
-export([term/2, term/3]).

-include_lib("eunit/include/eunit.hrl").

term(Term, ModuleMap) ->
    term(expr, Term, ModuleMap).
term(expr, Term, ModuleMap) -> 
    GlobalScope = scope(ModuleMap),
    History = [],
    expr(GlobalScope, #{}, History, Term);
term(pattern, Term, ModuleMap) -> 
    GlobalScope = scope(ModuleMap),
    History = [],
    PatternDomain = any,
    pattern(GlobalScope, #{}, History, PatternDomain, Term).



scope(ModuleMap) ->

    % There's a gotcha when a scope is computed that in order to compute the
    % scope, the scope itself is needed.  to circumvent this catch-22, we relax
    % the requirement and compute an index of index functions first. Each index
    % function will compute a scope function when given the full map of
    % indices. Thus we can neatly compute the index of index functions, and
    % then with the index in hand, build the scope by evaluating each index
    % function.
    I = fun(Def) ->
                fun(Index) ->
                        fun(ArgDomains, History) ->
                                Global = global_scope(Index),
                                case expr(Global, #{}, [], Def) of
                                   {error, Errs}  -> {error, Errs};
                                   {ok, {_, F}}   -> F(ArgDomains, History)
                                end end end end,

    % The index contains a mapping of def paths to index functions. An index
    % function will return a scope function when given an index as an argument.
    % A scope function will return the evaluated tree of the def that the def
    % path is pointing to, when given arg domains and the current evaluation
    % history
    Index = maps:from_list([{P ++ [N], I(Term)} || {module, _, P, _, _, Defs} <- maps:values(ModuleMap),
                                                   {N, Term} <- maps:to_list(Defs)]),


    global_scope(Index).



% The global scope is a map from symbol paths ('[a, b, f]') to a function `F`
% which takes arg domains + history and computes the evaluated tree of the
% function `f` in module `a/b`
global_scope(Index) -> maps:from_list([{Path, I(Index)} || {Path, I} <- maps:to_list(Index)]).



%  How do I compute an expression and what do I learn from an expression:
%   Inputs: Expression AST
%           A local scope of: Symbol -> Domain
%           A global scope of: Path -> (Arg Domains -> Tree)
%           A call history of applications (to check for recursion)
%   Output: The linearized AST
%           Map of linearized definitions
%   Errors: Strict domain not subset errors
expr(GlobalScope, LocalScope, History, Term) ->
    Pre = fun(_, LclScp, T) -> expr_pre({GlobalScope, LclScp}, History, T) end,
    Post = fun(Type, LclScp, T) -> expr_post(Type, {GlobalScope, LclScp}, History, T) end,
    case ast:traverse_term(expr, Pre, Post, LocalScope, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {Env, Tree}}   ->
            {Local, _} = lists:partition(fun({{KeyPath, _}, _V}) -> length(KeyPath) =:= 1 end, maps:to_list(Env)),
            case linearize_local_defs(Local, History, Tree) of
                {error, Errs}               -> {error, Errs};
                {ok, {DefsEnv, DefsTree}}   -> {ok, {maps:merge(Env, DefsEnv), DefsTree}}
            end
    end.



%  How do I compute a pattern and what do I learn from a pattern:
%   Inputs: Pattern AST
%           A local scope of: Symbol -> Domain
%           A global scope of: Path -> (Arg Domains -> Tree)
%           A history of applications (to check for recursion)
%   Output: An environment of: Symbol -> Domain
%           The pattern domain literal (containing vars)
%   Errors: Only if there's no intersection between domains in for example
%           pattern applications
pattern(GlobalScope, LocalScope, History, Domain, Term) -> 
    Pre = fun(_, _, T) -> pattern_pre(History, domain(T), T) end,
    Post = fun(Type, LclScp, T) -> pattern_post(Type, {GlobalScope, LclScp}, History, T) end,
    case ast:traverse_term(pattern, Pre, Post, LocalScope, set_domain(Term, Domain)) of
        {error, Errs}       -> {error, Errs};
        {ok, {_, Trees}}    -> {ok, Trees}
    end.


% When scanning clauses, we jump out of the tree traversal because a few extra checks are needed:
% - Stop scanning clauses if arg domains are subset of current pattern domains
% - Skip clauses for which there's no intersection between arg domains and pattern domains
% - Check if the argdomains are a subset of the union of clauses
clauses(Scopes, History, ArgDomains, Clauses) -> 
    ErrCtx = symbol:ctx(hd(Clauses)),
    case error:collect(clauses(Scopes, History, ArgDomains, Clauses, [])) of
        {error, Errs}       -> {error, Errs};
        {ok, []}            -> error:format({no_intersection_between_clauses_and_argdomains, ArgDomains},
                                            {linearize, ErrCtx, History});
        {ok, ClausesRes}    -> {EnvCs, TreeCs} = unzip(ClausesRes),
                               DomainPs = [lists:map(fun domain/1, PsTs) || {clause, _, PsTs, _} <- TreeCs],
                               ActualDomains = [union(Ds) || Ds <- pivot(DomainPs)],
                               case subset(ArgDomains, ActualDomains) of
                                   false    -> error:format({arguments_not_subset_of_clauses, ArgDomains, ActualDomains},
                                                            {linearize, ErrCtx, History});
                                   true     -> {ok, {utils:merge(EnvCs), TreeCs}}
                               end
    end.
clauses(_, _, _, [], Res) -> lists:reverse(Res);
clauses(Scopes, History, ArgDomains, [Clause | Cs], Res) ->
    case clause(Scopes, History, ArgDomains, Clause) of
        {error, Errs}           -> [{error, Errs} | clauses(Scopes, History, ArgDomains, Cs, Res)];
        {ok, {Env, Clauses}}    ->                        

            % Only scan patterns until (and including) the first clause for
            % which all the pattern domains are subsets of the argument
            % domains. There's no point in keeping on scanning patterns if we
            % can infer from the argument domains that they will not be reached
            ClauseRes = lists:reverse([{ok, {Env, C}} || C <- Clauses]),
            IsClauseSubset = fun({clause, _, PsTs, _}) -> domain:subset(ArgDomains, [domain(T) || T <- PsTs]) end,
            case lists:any(IsClauseSubset, Clauses) of
                true    -> clauses(Scopes, History, ArgDomains, [], ClauseRes ++ Res);
                false   -> clauses(Scopes, History, ArgDomains, Cs, ClauseRes ++ Res)
            end
    end.



clause({GlobalScope, LocalScope} = Scopes, History, ArgDomains, {clause, Ctx, Patterns, Expr}) ->
    case error:collect([pattern(GlobalScope, LocalScope, History, D, P) || {D, P} <- zip(ArgDomains, Patterns)]) of
        {error, Errs}   -> {error, Errs};
        {ok, PsTrees}   ->
            case error:collect([expand_clause(Scopes, History, PsTs, Expr, Ctx)
                                || PsTs <- combinations(PsTrees),
                                   not(lists:member(none, lists:map(fun domain/1, PsTs)))]) of
                {error, Errs}       -> {error, Errs};
                {ok, ClausesRes}    -> {Envs, Clauses} = unzip(ClausesRes),
                                       {ok, {utils:merge(Envs), Clauses}}
            end
    end.



expand_clause({GlobalScope, LocalScope}, History, Patterns, Expr, Ctx) -> 
    ClauseEnv = utils:merge([env(P) || P <- Patterns]),
    ClauseScope = maps:merge(LocalScope, ClauseEnv),
    case expr(GlobalScope, ClauseScope, History, Expr) of
        {error, Errs}           -> {error, Errs};
        {ok, {Env, ExprTree}}   -> ClauseCtx = maps:put(domain, domain(ExprTree), Ctx),
                                   {ok, {Env, {clause, ClauseCtx, Patterns, ExprTree}}}
    end.



pattern_pre(_, Domain, {tagged, Ctx, Path, Expr} = Term) ->
    case domain:intersection(Domain, {tagged, Path, any}) of
        {sum, Ds}                   -> ExprDomain = domain:union([D || {tagged, _, D} <- Ds]),
                                       {ok, {tagged, Ctx, Path, set_domain(Expr, ExprDomain)}};
        {tagged, Path, ExprDomain}  -> {ok, {tagged, Ctx, Path, set_domain(Expr, ExprDomain)}};
        _                           -> {skip, [set_domain(Term, none)]}
    end;

pattern_pre(_, Domain, {dict, Ctx, Elems} = Term) ->
    Keys = [symbol:name(E) || E <- Elems],
    case domain:intersection(Domain, maps:from_list(zip(Keys, [any || _ <- Elems]))) of
        {sum, MapDs}        -> Ds = [domain:union([maps:get(K, D) || D <- MapDs]) || K <- Keys],
                               {ok, {dict, Ctx, [set_domain(E, D) || {E, D} <- zip(Elems, Ds)]}};
        M when is_map(M)    -> Ds = [maps:get(K, M) || K <- Keys],
                               {ok, {dict, Ctx, [set_domain(E, D) || {E, D} <- zip(Elems, Ds)]}};
        _                   -> {skip, [set_domain(Term, none)]}
    end;

pattern_pre(_, Domain, {list, Ctx, Elems} = Term) ->
    case domain:intersection(Domain, [any || _ <- Elems]) of
        {sum, ListDs}       -> Ds = [domain:union(L) || L <- pivot(ListDs)],
                               {ok, {list, Ctx, [set_domain(E, D) || {E, D} <- zip(Elems, Ds)]}};
        Ds when is_list(Ds) -> {ok, {list, Ctx, [set_domain(E, D) || {E, D} <- zip(Elems, Ds)]}};
        _                   -> {skip, [set_domain(Term, none)]}
    end;

pattern_pre(_, Domain, {sum, Ctx, Elems}) -> {ok, {sum, Ctx, [set_domain(E, Domain) || E <- Elems]}};

pattern_pre(_, Domain, {keyword, _, _, _} = Term) ->
    D = domain:intersection(symbol:tag(Term), Domain),
    {ok, set_domain(Term, D)};

pattern_pre(_, Domain, {keyword, _, _} = Term) ->
    {ok, set_domain(Term, Domain)};

pattern_pre(_, Domain, {value, _, _, Val} = Term) ->
    D = domain:intersection(Val, Domain),
    {ok, set_domain(Term, D)};

pattern_pre(_, Domain, {variable, _, _, _} = Term) ->
    {ok, set_domain(Term, Domain)};

pattern_pre(_, Domain, {application, Ctx, Expr, Args}) ->
    {ok, set_domain({application, Ctx, set_domain(Expr, any), [set_domain(A, any) || A <- Args]}, Domain)};

pattern_pre(_, Domain, {qualified_application, Ctx, ModulePath, Name, Args}) ->
    {ok, set_domain({qualified_application, Ctx, ModulePath, Name, [set_domain(A, any) || A <- Args]}, Domain)};

pattern_pre(_, Domain, {beam_application, Ctx, ModulePath, Name, Args}) ->
    {ok, set_domain({beam_application, Ctx, ModulePath, Name, [set_domain(A, any) || A <- Args]}, Domain)};

pattern_pre(History, Domain, {qualified_symbol, Ctx, _, _} = Term) ->
    error:format({unapplied_function_in_pattern, symbol:tag(Term)}, {linearize, Ctx, History});

pattern_pre(_, Domain, {pair, Ctx, Key, Val}) ->
    {ok, {pair, Ctx, set_domain(Key, Domain), set_domain(Val, Domain)}}.



expr_pre(_, _, {'fun', _, _}) -> leave_intact;
expr_pre(_, _, {'let', _, _, _, _}) -> leave_intact;
expr_pre(_, _, _) -> ok.


post(expr, _, _, {def, _, _, {'fun', _, F}}) -> {ok, {#{}, F}};
post(expr, _, _, {def, Ctx, _, Expr}) ->
    F = fun(ArgDomains, History) ->
            case length(ArgDomains) =:= 0 of
                false   -> error:format({wrong_function_arity, 0, length(ArgDomains)},
                                        {linearize, Ctx, History});
                true    -> {ok, {#{}, Expr}}
            end
        end,
    % A def with no clauses is linearized to a function with zero arguments.
    {ok, {#{}, F}};

% Expr of type `a b -> a + b` (e.g. a function)
% When we linearize a def, we can compile several versions of the def in the
% module. Then we can just call the appropriate version at the calling point.
% This approach has the added benefit in that we can call the def recursively.
%
% What do we do when we assign a local variable to a function and call it at
% two separate points with different domains?
% I think my preferred approach would always inline the function at its calling
% sites. Since an anonymous function can't be recursive, we don't need to worry
% about it calling itself.
post(expr, Scopes, _, {'fun', Ctx, Clauses} = Term) ->
    F = fun(ArgDomains, History) ->
        Arity = fun_term_arity(Term),
        case Arity =:= length(ArgDomains) of
            false   -> error:format({wrong_function_arity, Arity, length(ArgDomains)},
                                    {linearize, Ctx, History});
            true    -> case clauses(Scopes, History, ArgDomains, Clauses) of
                           {error, Errs}           -> {error, Errs};
                           {ok, {EnvCs, TreeCs}}   -> Domain = union([domain(T) || T <- TreeCs]),
                                                      {ok, {EnvCs, set_domain({'fun', Ctx, TreeCs}, Domain)}}
                       end
        end
    end,
    {ok, {#{}, set_domain({'fun', Ctx, F}, F)}};



% Expr of type `val p = e` where `p` is a pattern and `e` is an expression
post(expr, {GlobalScope, LocalScope}, History, {'let', Ctx, Pattern, Expr, NextExpr}) ->
    case expr(GlobalScope, LocalScope, History, Expr) of
        {error, Errs}       -> {error, Errs};
        {ok, {EEnv, ETree}}    ->
            case pattern(GlobalScope, LocalScope, History, domain(ETree), Pattern) of
                {error, Errs}   -> {error, Errs};
                {ok, [PTree]}   -> case expr(GlobalScope, maps:merge(LocalScope, env(PTree)), History, NextExpr) of
                                       {error, Errs}        -> {error, Errs};
                                       {ok, {NEnv, NTree}}  -> Domain = domain(NTree),
                                                               Term = {'let', Ctx, PTree, ETree, NTree},
                                                               {ok, {maps:merge(NEnv, EEnv), set_domain(Term, Domain)}}
                                   end;
                {ok, _}         -> error:format({sum_type_in_let_pattern, Pattern, domain(ETree)},
                                                {linearize, Ctx, History})
            end
    end;



post(expr, _, _, {seq, _, _, Then} = Term) -> {ok, {#{}, set_domain(Term, domain(Then))}};



% Patterns of type `x.t(a, b, c)` where `x` is a function defined in global scope
post(pattern, {GlobalScope, _}, History, {qualified_application, Ctx, Path, Name, Args} = Term) ->
    F = maps:get(Path ++ [Name], GlobalScope),
    pattern_apply(F, Args, Path ++ [Name], History, Ctx, domain(Term));



% Expr of type `x.t(a, b, c)` where `x` is a function defined in global scope
post(expr, {GlobalScope, _}, History, {qualified_application, Ctx, Path, Name, Args} = Term) ->
    F = maps:get(Path ++ [Name], GlobalScope),
    expr_apply(F, Args, Path ++ [Name], History, Ctx, Term);



% Patterns of type `x.t(a, b, c)` where `x` is a beam function
post(pattern, _, _History, {beam_application, Ctx, Path, Name, Args}) ->
    ModuleName = module:beam_name(Path),
    ArgDomains = [domain(A) || A <- Args],
    Domain = case domain:is_literal(ArgDomains) of
                 true    -> case import:is_whitelisted(ModuleName, Name) of
                                true     -> erlang:apply(ModuleName, Name, ArgDomains);
                                false    -> any
                            end;
                 false   -> any % TODO: Get type information for beam function
             end,
    {ok, {#{}, set_domain({value, Ctx, unknown, Domain}, Domain)}};

% Expr of type `x.t(a, b, c)` where `x` is a beam function
post(expr, Scopes, History, {beam_application, _Ctx, _Path, _Name, _Args} = Term) ->
    case post(pattern, Scopes, History, Term) of
        {error, Errs}               -> {error, Errs};
        {ok, {value, _, _, Domain}} -> {ok, {#{}, set_domain(Term, Domain)}}
    end;



% Patterns of type `x.f(a, b, c)` where `f` refers to/is a function
post(pattern, _, History, {application, Ctx, Expr, Args} = Term) ->
    case domain(Expr) of
        F when is_function(F)   ->
            pattern_apply(domain(Expr), Args, [utils:gen_tag(F)], History, Ctx, domain(Term));
        Other                   ->
            error:format({function_domain_expected, Other}, {linearize, Ctx, History})
    end;



% Expr of type `x.f(a, b, c)` where `f` refers to/is a function
%
% Case 1: (n -> n + 1)(2)
% Case 2: f = (n -> n + 1)
%         f(2)
% case 3: val t = erlang/random/randint(1,2).match(| 1 -> (| _ -> boolean/True)
%                                                  | 2 -> (| _ -> boolean/False))
%         t('whatever')
%
% We want to coopt erlang core functions for *local* functions (not 'defs' even
% if they aren't exported) to avoid rolling our own closures etc.  This means
% that whatever function we end up compiling in erlang core can't be linearized
% at the point of application like we would do with a qualified application
% call.
%
% What does argument linearization at the point of application do?
%  - It allows us to compute pattern applications with correct/narrow input
%    arguments
%  - It will allow us to overload function definitions and at compile time make
%    sure we dispatch to the proper function based on the argument domains.
%  - It allows us to compile only clause statements and expressions specific to
%    the argument domains
%  - It *does not* mean that we can't narrow down domain errors (e.g. where a
%    function argument is not in the subset of domains accepted as arguments by
%    the functions) specifically to their call site. We still do domain checks
%    at the call sites. We just don't return the AST with the function
%    linearized specifically to this call site.
%
% When we linearize at function definition, we can't linearize with respect to
% the individual argument domains like we do when we linearize at application
% points. Naiively this would mean that we would assume the `any` domain at
% linearization for all function arguments. However, if we scan through the
% code we can do much better and find the union of domains that this particular
% function is applied to and linearize with the union of all the input
% arguments this particular function is applied to.
%
% To do so, at the application site we record the function tag and the argument
% domains in the env at every application of a local function. We can do that
% in the same format as we store the linearized global function in the env, but
% omit the resulting linearized function AST. Then once the linearized AST has
% been computed, we can traverse it and linearize all local function
% definitions.
post(expr, _, History, {application, Ctx, Expr, Args} = Term) ->
    Apply = fun(F) -> expr_apply(F, Args, [utils:gen_tag(F)], History, Ctx, Term) end,
    case domain(Expr) of
        {sum, Ds}   ->
            case error:collect([Apply(F) || F <- Ds]) of
                {error, Errs}       -> {error, Errs};
                {ok, Res}           -> {Envs, Trees} = unzip(Res),
                                       Domain = union([domain(T) || T <- Trees]),
                                       {ok, {utils:union(Envs), set_domain(Term, Domain)}}
            end;
        F           -> Apply(F)
    end;



% Expr/Patterns of type `T: S`
post(_, _, _, {tagged, _, Path, Expr} = Term) ->
    {ok, {#{}, set_domain(Term, {tagged, Path, domain(Expr)})}};



% Expr/Patterns of type `[a, b, c]`
post(_, _, _, {list, _, Elems} = Term) -> 
    {ok, {#{}, set_domain(Term, [domain(E) || E <- Elems])}};



% Expr/Patterns of type `A | B | C`
post(_, _, _, {sum, _, Elems} = Term) ->
    {ok, {#{}, set_domain(Term, {sum, ordsets:from_list([domain(E) || E <- Elems])})}};



% Expr/Patterns of type `{k1: v1, k2: v2}`
post(_, _, _, {dict, _, Elems} = Term) ->
    Domain = maps:from_list([{symbol:name(E), domain(E)} || E <- Elems]),
    {ok, {#{}, set_domain(Term, Domain)}};



% Dict key/val pattern of type `k: v`
post(pattern, _, _, {pair, _, {keyword, _, _}, Val} = Term) ->
    {ok, {#{}, set_domain(Term, domain(Val))}};



% Patterns of type `k: v` or more complicated like `[a, b]: x.t(q)`
post(pattern, {GlobalScope, LocalScope}, History, {pair, Ctx, Key, Val}) ->
    case pattern(GlobalScope, LocalScope, History, domain(Val), Key) of
        {error, Errs}           -> {error, Errs};
        {ok, KTrees} -> F = fun(Tree) -> Term = {pair, Ctx, Tree, Val},
                                         Env = env(Tree),
                                         case domain:is_literal(domain(Tree)) of
                                             false   -> {ok, {Env, set_domain(Term, domain(Val))}};
                                             true    -> {ok, {Env, domain:to_term(domain(Tree), symbol:ctx(Term))}}
                                         end end,
                        sum([F(Tree) || Tree <- KTrees])
    end;

% Expr of type `k: v`
post(expr, _, _, {pair, _, _, Val} = Term) ->
    {ok, {#{}, set_domain(Term, domain(Val))}};



% Pattern like `a`
post(pattern, {_, LocalScope}, _, {variable, _, _, _} = Term) -> 
    ScopeDomain = maps:get(symbol:tag(Term), LocalScope, any),
    Domain = intersection(ScopeDomain, domain(Term)),
    case domain:is_literal(Domain) of
        false    -> {ok, {#{symbol:tag(Term) => Domain}, set_domain(Term, Domain)}};
        true     -> {ok, {#{symbol:tag(Term) => Domain}, domain:to_term(Domain, symbol:ctx(Term))}}
    end;

% Expr of type `x`
post(expr, {_, LocalScope}, _, {variable, _, _, _} = Term) ->
    Domain = maps:get(symbol:tag(Term), LocalScope),
    case domain:is_literal(Domain) of
        false   -> {ok, {#{}, set_domain(Term, Domain)}};
        true    -> {ok, {#{}, domain:to_term(Domain, symbol:ctx(Term))}}
    end;



% Pattern like `T` -- Domain has already been set in pre_pattern
post(pattern, _, _, {keyword, _, _, _} = Term) -> {ok, {#{}, Term}};
% Pattern like `key: ...` in dictionary -- Domain has already been set in pre_pattern
post(pattern, _, _, {keyword, _, _} = Term) -> {ok, {#{}, Term}};

% Expr of type `T`
post(expr, _, _, {keyword, _, _, _} = Term) ->
    {ok, {#{}, set_domain({value, symbol:ctx(Term), atom, symbol:tag(Term)}, symbol:tag(Term))}};
post(expr, _, _, {keyword, _, _} = Term) ->
    {ok, {#{}, set_domain(Term, none)}};



% Pattern like `5` -- Domain has already been set in pre_pattern
post(pattern, _, _, {value, _, _, _} = Term) -> {ok, {#{}, Term}};

% Expr/Patterns of type `1` or `"sdfgsf"` or `'atom'`
post(expr, _, _, {value, _, _, Val} = Term) -> {ok, {#{}, set_domain(Term, Val)}}.


% Local functions (i.e. functions that aren't defined by `def`) need to be
% linearized at their point of definition to compile to a local function in
% erlang core. To linearize a local function we collect the argument domains
% from all calling sites for the particular function and linearize it with the
% union of arguments.
linearize_local_defs(LocalDefs, History, Tree) -> 
    Local = [{Tag, DomainArgs} || {{[Tag], DomainArgs}, _} <- LocalDefs],
    GroupedDefs = utils:group_by(fun({K, _V}) -> K end, fun({_K, V}) -> V end, Local),
    linearize_local_defs(GroupedDefs, History, Tree, #{}, []).
linearize_local_defs([], _, Tree, Env, []) -> {ok, {Env, Tree}};
linearize_local_defs([], _, _, _, Errors) -> error:collect(lists:reverse(Errors));
linearize_local_defs([{Tag, DomainArgsList} | Defs], History, Tree, Env, Errors) ->
    DomainArgs = [union(Args) || Args <- pivot(DomainArgsList)],
    Pre = fun(expr, _, {'fun', _, _})      -> leave_intact;
             (_, _, _)                     -> ok end,
    Post = fun(expr, _, {'fun', Ctx, F})   ->
                   case utils:gen_tag(F) =:= Tag of
                       true     -> case fun_apply(F, DomainArgs, Tag, History, Ctx) of
                                       {error, Errs}    -> {error, Errs};
                                       {ok, {Env, T}}   -> {ok, Env, T}
                                   end;
                       false    -> ok
                   end;
              (_, _, _)                    -> ok end,
    case ast:traverse_term(expr, Pre, Post, #{}, Tree) of
        {error, Errs}   -> linearize_local_defs(Defs, History, Tree, Env, [Errs | Errors]);
        {ok, {E, T}}    -> linearize_local_defs(Defs, History, T, maps:merge(Env, E), Errors)
    end.

fun_apply(F, ArgDomains, Tag, History, Ctx) when is_function(F) ->
    Step = {Tag, Ctx, ArgDomains},
    case is_recursive(Step, History) of
        true            -> Domain = {recur, F},
                           {ok, set_domain({recursion, Ctx, Tag, ArgDomains}, Domain)};
        false           -> F(ArgDomains, [Step | History])
    end;
fun_apply(F, _, _, History, Ctx) -> error:format({function_domain_expected, F}, {linearize, Ctx, History}).

pattern_apply(F, Args, Path, History, Ctx, AppDomain) ->
    ArgDomains = [domain(A) || A <- Args],
    case fun_apply(F, ArgDomains, Path, History, Ctx) of
        {error, Errs}               -> {error, Errs};
        {ok, {_, Tree}}             -> Domain = intersection(domain(Tree), AppDomain),
                                       {ok, {#{}, domain:to_term(Domain, Ctx)}}
    end.

expr_apply(F, Args, Path, History, Ctx, Term) ->
    ArgDomains = [domain(A) || A <- Args],
    case fun_apply(F, ArgDomains, Path, History, Ctx) of
        {error, Errs}       -> {error, Errs};
        {ok, {Env, Tree}}   -> NewEnv = maps:put({Path, ArgDomains}, Tree, Env),
                               {ok, {NewEnv, set_domain(Term, domain(Tree))}}
    end.

is_recursive({Path, _, Args}, History) ->
    lists:any(fun({HPath, _, HArgs}) -> (HPath == Path) andalso (HArgs == Args) end, History).

fun_term_arity({'fun', _, []}) -> 0;
fun_term_arity({'fun', _, [{clause, _, Ps, _} | _]}) -> length(Ps).

domain(Term) -> maps:get(domain, symbol:ctx(Term)).
set_domain(Term, Domain) ->
    NewCtx = maps:put(domain, Domain, symbol:ctx(Term)),
    setelement(2, Term, NewCtx).

    
pivot([]) -> [];
pivot([H | _] = ListOfLists) ->
    Rev = lists:foldl(fun(Elems, Accs) -> [[E | Acc] || {E, Acc} <- zip(Elems, Accs)] end,
                      [[] || _ <- H],
                      ListOfLists),
    [lists:reverse(L) || L <- Rev].

% Flatten sum domains: When use for example `boolean` in a pattern, it will
% return the sum of `True` and `False`. We cannot match against both of these
% in one clause in erlang core, so instead we flatten sums so each domain in
% the sum gets its own clause.
%
% It might be obvious from the domain of the pattern argument that, say, only
% `boolean/True` is ever a possibility, in which case we only want to return
% the pattern clause for this case
combinations([Elements | Rest]) -> [[E | Tail] || E <- Elements,
                                                  Tail <- combinations(Rest)];
combinations([])                -> [[]].



% Patterns in kind can include sum types, e.g.  multiple domain values. However
% in erlang core, each pattern can only be for a single value. For this reason
% we'll have to expand each pattern of domains to possibly multiple literal
% patterns.
expand({dict, Ctx, ElemList}) -> [{dict, Ctx, Elems} || Elems <- combinations(ElemList)];
expand({list, Ctx, ElemList}) -> [{list, Ctx, Elems} || Elems <- combinations(ElemList)];
expand({sum, Ctx, ElemList}) -> [{sum, Ctx, Elems} || Elems <- combinations(ElemList)];
expand({tagged, Ctx, Path, ExprList}) -> [{tagged, Ctx, Path, Expr} || Expr <- ExprList];
expand({pair, Ctx, KeyList, ValList}) -> [{pair, Ctx, Key, Val} || [Key, Val] <- combinations([KeyList, ValList])];
expand({application, Ctx, ExprList, ArgsList}) ->
    [{application, Ctx, Expr, Args} || [Expr | Args] <- combinations([ExprList | ArgsList])];
expand({qualified_application, Ctx, ModulePath, Name, ArgsList}) ->
    [{qualified_application, Ctx, ModulePath, Name, Args} || Args <- combinations(ArgsList)];
expand({beam_application, Ctx, ModulePath, Name, ArgsList}) ->
    [{beam_application, Ctx, ModulePath, Name, Args} || Args <- combinations(ArgsList)];
expand(Term) -> [Term].

expand_sum({sum, _, Elems}) -> Elems;
expand_sum(Term) -> [Term].

sum([Elem]) -> Elem;
sum(Elems) -> set_domain({sum, symbol:ctx(hd(Elems)), Elems}, domain:union([domain(E) || E <- Elems])).

env(Term) -> maps:get(env, symbol:ctx(Term), #{}).
child_env({dict, _, Elems}) -> utils:merge([env(E) || E <- Elems]);
child_env({list, _, Elems}) -> utils:merge([env(E) || E <- Elems]);
child_env({tagged, _, _, Expr}) -> env(Expr);
child_env({pair, _, Key, Val}) -> maps:merge(env(Key), env(Val));
child_env(_) -> #{}.

set_env(Term, Env) ->
    Ctx = symbol:ctx(Term),
    NewCtx = maps:put(env, utils:merge([child_env(Term), Env, env(Term)]), Ctx),
    setelement(2, Term, NewCtx).

pattern_post(Type, Scopes, History, Term) ->
    case error:collect([post(Type, Scopes, History, T) || T <- expand(Term)]) of
        {error, Errs}   -> {error, Errs};
        {ok, ResList}   -> Trees = [set_env(T, Env) || {Env, Tree} <- ResList, T <- expand_sum(Tree)],
                           {ok, Trees}
    end.

expr_post(Type, Scopes, History, Term) ->
    case post(Type, Scopes, History, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {Env, Tree}}   -> {ok, Env, Tree}
    end.

