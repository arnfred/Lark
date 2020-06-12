-module(ast).
-export([traverse/4, traverse/5, traverse/6]).
-import(maps, [merge/2]).
-include_lib("eunit/include/eunit.hrl").

traverse(Key, Pre, Post, Type, Env, Term) -> 
    LiftedPre = tag(Key, Pre),
    term({LiftedPre, Post}, Type, Env, Term).
traverse(Key, Pre, Post, Env, AST) when is_list(AST) -> 
    [def(Key, Pre, Post, Env, Term) || Term <- AST];
traverse(Key, Pre, Post, Env, AST) -> 
    def(Key, Pre, Post, Env, AST).
traverse(Key, Pre, Post, AST) when is_list(AST) -> 
    [def(Key, Pre, Post, #{}, Term) || Term <- AST];
traverse(Key, Pre, Post, AST) -> 
    def(Key, Pre, Post, #{}, AST).

def(Key, Pre, Post, Env, Term) -> 
    LiftedPre = tag(Key, Pre),
    term({LiftedPre, Post}, expr, Env, Term).

term({LiftedPre, Post}, Type, Env, Term) ->
    case LiftedPre(Term) of
        {error, Errs}       -> {error, Errs};
        {ok, TaggedTerm}    -> 
            F = fun() -> iter({LiftedPre, Post}, Type, Env, TaggedTerm) end,
            case Post(Type, term_type(Term), F) of
                {error, Errs}           -> {error, Errs};
                {ok, {NewEnv, NewTerm}} -> {ok, {NewEnv, NewTerm}};
                {NewEnv, NewTerm}       -> {ok, {NewEnv, NewTerm}}
            end
    end.

iter(Meta, expr, Env, {type_def, Context, Name, Args, Expr}) when is_list(Args) ->
    case iter(Meta, expr, Env, {def, Context, Name, Args, Expr}) of
        {error, Errs} -> {error, Errs};
        {ok, {DefEnv, {def, NewCtx, Name, TArgs, TExpr}}} ->
            {ok, {DefEnv, {type_def, NewCtx, Name, TArgs, TExpr}}}
    end;

iter(Meta, expr, Env, {def, Context, Name, Args, Expr}) when is_list(Args) ->
    case map(Meta, pattern, Env, Args) of
        {error, Errs}           -> {error, Errs};
        {ok, {ArgsEnv, TArgs}}  ->
            RawExpr = case Expr of
                          _ when is_list(Expr)  -> map(Meta, expr, merge(Env, ArgsEnv), Expr);
                          _                     -> term(Meta, expr, merge(Env, ArgsEnv), Expr)
                      end,
            case RawExpr of
                {error, Errs}           -> {error, Errs};
                {ok, {ExprEnv, TExpr}}  -> 
                    {ok, {merge(ArgsEnv, ExprEnv), {def, Context, Name, TArgs, TExpr}}}
            end
    end;

iter(Meta, expr, Env, {clauses, Context, Clauses}) when is_list(Clauses) ->
    case map(Meta, expr, Env, Clauses) of
        {error, Errs}                   -> {error, Errs};
        {ok, {ClausesEnv, TClauses}}    -> {ok, {ClausesEnv, {clauses, Context, TClauses}}}
    end;

iter(Meta, expr, Env, {clause, Context, Patterns, Expr}) when is_list(Patterns) ->
    case map(Meta, pattern, Env, Patterns) of
        {error, Errs}                   -> {error, Errs};
        {ok, {PatternEnv, TPatterns}}   -> 
            case term(Meta, expr, merge(Env, PatternEnv), Expr) of
                {error, Errs}           -> {error, Errs};
                {ok, {ExprEnv, TExpr}}  -> 
                    {ok, {merge(PatternEnv, ExprEnv), {clause, Context, TPatterns, TExpr}}}
            end
    end;

iter(Meta, expr, Env, {lambda, Context, Clauses}) when is_list(Clauses) ->
    case map(Meta, expr, Env, Clauses) of
        {error, Errs}                -> {error, Errs};
        {ok, {ClausesEnv, TClauses}} -> {ok, {ClausesEnv, {lambda, Context, TClauses}}}
    end;

iter(Meta, expr, Env, {val, Context, Pattern, Expr}) ->
    error:map2(term(Meta, pattern, Env, Pattern),
               term(Meta, expr, Env, Expr),
               fun({PatternEnv, TPattern}, {ExprEnv, TExpr}) ->
                       {merge(PatternEnv, ExprEnv), {val, Context, TPattern, TExpr}} end);

iter(Meta, expr, Env, {match, Context, Expr, Clauses}) when is_list(Clauses) ->
    error:map2(term(Meta, expr, Env, Expr),
               map(Meta, expr, Env, Clauses),
               fun({ExprEnv, TExpr}, {ClauseEnv, TClauses}) ->
                       {merge(ExprEnv, ClauseEnv), {match, Context, TExpr, TClauses}} end);

iter(Meta, Type, Env, {application, Context, Expr, Args}) when is_list(Args) ->
    error:map2(map(Meta, Type, Env, Args),
               term(Meta, Type, Env, Expr),
               fun({ArgsEnv, TArgs}, {ExprEnv, TExpr}) -> 
                       {merge(ArgsEnv, ExprEnv), {application, Context, TExpr, TArgs}} end);

iter(Meta, Type, Env, {lookup, Context, Expr, Elems}) when is_list(Elems) ->
    error:map2(term(Meta, Type, Env, Expr),
               iter(Meta, pattern, Env, {dict, Context, Elems}),
               fun({ExprEnv, TExpr}, {ElemsEnv, TElems}) ->
                       {dict, _, DictElems} = TElems,
                       {merge(ExprEnv, ElemsEnv), {lookup, Context, TExpr, DictElems}} end);

iter(Meta, Type, Env, {tuple, Context, Expressions}) when is_list(Expressions) ->
    case fold(Meta, Type, Env, Expressions) of
        {error, Errs}              -> {error, Errs};
        {ok, {ExprsEnv, TExprs}}   -> {ok, {ExprsEnv, {tuple, Context, TExprs}}}
    end;

iter(Meta, Type, Env, {dict, Context, Expressions}) when is_list(Expressions) ->
    case map(Meta, {dict, Type}, Env, Expressions) of
        {error, Errs}              -> {error, Errs};
        {ok, {ExprsEnv, TExprs}}   -> 
            {ok, {ExprsEnv, {dict, Context, TExprs}}}
    end;

% Element in dictionary
iter(Meta, {dict, Type}, Env, Term) ->
    case {Type, term_type(Term), symbol:is(Term)} of
        {pattern, pair, _}    -> iter_pair(Meta, pattern, Env, Term, fun maps:merge/2);
        {expr, pair, _}       -> iter_pair(Meta, expr, Env, Term, fun(_, ValEnv) -> ValEnv end);
        {_, _, false}         -> error:format({illegal_dict_element, term_type(Term), Type}, {ast, Term});
        {pattern, _Symbol, _} -> {ok, {#{}, Term}}; % Also true for lookup expr
        {expr, _Symbol, _}    -> error:format({illegal_dict_element, symbol:name(Term), Type}, {ast, Term})
    end;

iter(Meta, Type, Env, {pair, _, _, _} = Term) -> iter_pair(Meta, Type, Env, Term, fun maps:merge/2);

iter(_, Type, Env, Term) ->
    IsSymbol = symbol:is(Term),
    InEnv    = IsSymbol andalso maps:is_key(symbol:name(Term), Env),
    case {Type, IsSymbol, InEnv} of
        {_, false, _}           ->
            error:format({unrecognized_term, term_type(Term), Type}, {ast, Term});
        {expr, true, false}     ->
            error:format({undefined_symbol_in_expression, symbol:name(Term)}, {ast, Term});
        {expr, true, true}      -> 
            Name = symbol:name(Term),
            {ok, {#{Name => maps:get(Name, Env)}, Term}};
        {pattern, true, false}  -> {ok, {#{}, Term}};
        {pattern, true, true}   ->
            error:format({symbol_in_pattern_already_defined, symbol:name(Term)}, {ast, Term})
    end.

iter_pair(Meta, Type, Env, {pair, Context, Key, Val}, Merge) ->
    error:map2(term(Meta, Type, Env, Key),
               term(Meta, Type, Env, Val),
               fun({KeyEnv, TKey}, {ValEnv, TVal}) -> 
                       {Merge(KeyEnv, ValEnv), {pair, Context, TKey, TVal}} end).


map(Meta, Type, Env, Elements) ->
    Mapped = [term(Meta, Type, Env, Elem) || Elem <- Elements],
    case error:collect(Mapped) of
        {error, Errs} -> {error, Errs};
        {ok, Zipped} ->
            {EnvList, Ts} = lists:unzip(Zipped),
            NewEnv = lists:foldl(fun maps:merge/2, #{}, EnvList),
            {ok, {NewEnv, Ts}}
    end.

fold(Meta, Type, Env, Elements) ->
    F = fun(Expr, {EnvAcc, ExprAcc}) -> 
                case term(Meta, Type, EnvAcc, Expr) of
                    {error, Errs}           -> {EnvAcc, [{error, Errs} | ExprAcc]};
                    {ok, {NewEnv, NewExpr}} -> {maps:merge(EnvAcc, NewEnv), [NewExpr | ExprAcc]} 
                end
        end,
    {NewEnv, NewElements} = lists:foldl(F, {Env, []}, Elements),
    case error:collect(NewElements) of
        {error, Errs}       -> {error, lists:reverse(Errs)};
        {ok, Elems}         -> {ok, {NewEnv, lists:reverse(Elems)}}
    end.

tag(Key, Pre) ->
    fun(Term) -> case Pre(Term) of
                     {error, Errs}  -> {error, Errs};
                     {ok, Tag}      -> {ok, with_tag(Key, Tag, Term)};
                     Tag            -> {ok, with_tag(Key, Tag, Term)}
                 end
    end.

with_tag(Key, T, {A, B, C}) ->
    {A, add_tag(Key, T, B), add_tag(Key, T, C)};
with_tag(Key, T, {A, B, C, D}) ->
    {A, add_tag(Key, T, B), add_tag(Key, T, C), add_tag(Key, T, D)};
with_tag(Key, T, {A, B, C, D, E}) ->
    {A, add_tag(Key, T, B), add_tag(Key, T, C), add_tag(Key, T, D), add_tag(Key, T, E)}.

add_tag(Key, T, Elements) when is_list(Elements) -> 
    [add_tag(Key, T, Elem) || Elem <- Elements];
add_tag(Key, T, Elem) when is_tuple(Elem) -> 
    setelement(2, Elem, maps:put(Key, T, element(2, Elem)));
add_tag(Key, T, M) when is_map(M) ->
    maps:put(Key, T, M);
add_tag(_, _, Other) -> Other.

term_type(Elem) when is_tuple(Elem) -> element(1, Elem).
