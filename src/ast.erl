-module(ast).
-export([traverse/3, traverse/4, term_type/1, context/1, tag/2, tag/3]).
-import(maps, [merge/2]).
-include_lib("eunit/include/eunit.hrl").

traverse(Pre, Post, Scope, AST) -> term({Pre, Post}, expr, Scope, AST).
traverse(Pre, Post, AST)        -> term({Pre, Post}, expr, #{}, AST).

term({Pre, Post}, Type, Scope, Term) ->
    case Pre(Type, Scope, Term) of
        {error, Errs}   -> {error, Errs};
        skip            -> {ok, {Scope, Term}};
        {ok, NewTerm}   -> 
            case iter({Pre, Post}, Type, Scope, NewTerm) of
                {error, Errs}               -> {error, Errs};
                {ok, {Env, TraversedTerm}}  -> 
                    case Post(Type, Env, TraversedTerm) of
                        {error, Errs}       -> {error, Errs};
                        skip                -> {ok, {Env, TraversedTerm}};
                        {ok, PostTerm}      -> {ok, {maps:put(Term, PostTerm, Env), PostTerm}};
                        {ok, Key, PostTerm} -> {ok, {maps:put(Key, PostTerm, Env), PostTerm}}
                    end
            end
    end.

iter(Meta, expr, Scope, {def, Context, Name, Args, Expr}) when is_list(Args) ->
    case map(Meta, pattern, Scope, Args) of
        {error, Errs}            -> {error, Errs};
        {ok, {ArgsEnvs, TArgs}}  ->
            ExprScope = merge([Scope | ArgsEnvs]),
            RawExpr = case Expr of
                          _ when is_list(Expr)  -> map(Meta, expr, ExprScope, Expr);
                          _                     -> term(Meta, expr, ExprScope, Expr)
                      end,
            case RawExpr of
                {error, Errs}                           -> {error, Errs};
                {ok, {Envs, TExprs}} when is_list(Envs) -> 
                    {ok, {merge(ArgsEnvs ++ Envs), {def, Context, Name, TArgs, TExprs}}};
                {ok, {Env, TExpr}}                      -> 
                    {ok, {merge([Env | ArgsEnvs]), {def, Context, Name, TArgs, TExpr}}}
            end
    end;

iter(Meta, expr, Scope, {type_def, Context, Name, Args, Expr}) when is_list(Args) ->
    case iter(Meta, expr, Scope, {def, Context, Name, Args, Expr}) of
        {error, Errs}       -> {error, Errs};
        {ok, {Env, Term}}   -> {ok, {Env, setelement(1, Term, type_def)}}
    end;

iter(Meta, expr, Scope, {clause, Context, Patterns, Expr}) when is_list(Patterns) ->
    case map(Meta, pattern, Scope, Patterns) of
        {error, Errs}                       -> {error, Errs};
        {ok, {PatternEnvs, TPatterns}}   -> 
            NewScope = merge([Scope | PatternEnvs]),
            case term(Meta, expr, NewScope, Expr) of
                {error, Errs}       -> {error, Errs};
                {ok, {Env, TExpr}}  -> 
                    {ok, {merge([Env | PatternEnvs]), {clause, Context, TPatterns, TExpr}}}
            end
    end;

iter(Meta, expr, Scope, {lambda, Context, Clauses}) when is_list(Clauses) ->
    case map(Meta, expr, Scope, Clauses) of
        {error, Errs}               -> {error, Errs};
        {ok, {Envs, TClauses}}   -> {ok, {merge(Envs), {lambda, Context, TClauses}}}
    end;

iter(Meta, expr, Scope, {val, Context, Pattern, Expr}) ->
    error:map2(term(Meta, pattern, Scope, Pattern),
               term(Meta, expr, Scope, Expr),
               fun({PatternEnv, TPattern}, {Env, TExpr}) ->
                       {merge(PatternEnv, Env), {val, Context, TPattern, TExpr}} end);

iter(Meta, expr, Scope, {match, Context, Expr, Clauses}) when is_list(Clauses) ->
    error:map2(term(Meta, expr, Scope, Expr),
               map(Meta, expr, Scope, Clauses),
               fun({ExprEnv, TExpr}, {ClauseEnvs, TClauses}) ->
                       {merge([ExprEnv | ClauseEnvs]), {match, Context, TExpr, TClauses}} end);

iter(Meta, Type, Scope, {application, Context, Expr, Args}) when is_list(Args) ->
    error:map2(map(Meta, Type, Scope, Args),
               term(Meta, Type, Scope, Expr),
               fun({ArgsEnvs, TArgs}, {ExprEnv, TExpr}) -> 
                       {merge([ExprEnv | ArgsEnvs]), {application, Context, TExpr, TArgs}} end);

iter(Meta, Type, Scope, {lookup, Context, Expr, Elems}) when is_list(Elems) ->
    error:map2(term(Meta, Type, Scope, Expr),
               map(Meta, Type, Scope, Elems),
               fun({ExprEnv, TExpr}, {ElemsEnvs, TElems}) ->
                       {merge([ExprEnv | ElemsEnvs]), {lookup, Context, TExpr, TElems}} end);

iter(Meta, Type, Scope, {tuple, Context, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}          -> {error, Errs};
        {ok, {Envs, TExprs}}   -> {ok, {merge(Envs), {tuple, Context, TExprs}}}
    end;


iter(Meta, expr, Scope, {'let', Context, Pattern, Expr, Term}) ->
    error:flatmap2(term(Meta, pattern, Scope, Pattern),
                   term(Meta, expr, Scope, Expr),
                   fun({PatternEnv, TPattern}, {ExprEnv, TExpr}) ->
                           NewScope = merge(Scope, PatternEnv),
                           case term(Meta, expr, NewScope, Term) of
                               {error, Errs}            -> {error, Errs};
                               {ok, {Env, TTerm}}    -> 
                                   {ok, {merge([PatternEnv, ExprEnv, Env]), 
                                         {'let', Context, TPattern, TExpr, TTerm}}}
                           end
                   end);

iter(Meta, expr, Scope, {seq, Context, First, Then}) ->
    case term(Meta, expr, Scope, First) of
        {error, Errs}               -> {error, Errs};
        {ok, {FirstEnv, TFirst}} -> 
            case term(Meta, expr, Scope, Then) of
                {error, Errs}           -> {error, Errs};
                {ok, {ThenEnv, TThen}}   -> 
                    {ok, {merge(FirstEnv, ThenEnv), {seq, Context, TFirst, TThen}}}
            end
    end;


iter(Meta, Type, Scope, {dict, Context, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}              -> {error, Errs};
        {ok, {ExprsEnvs, TExprs}}   -> 
            {ok, {merge(ExprsEnvs), {dict, Context, TExprs}}}
    end;

%% Element in dictionary
%iter(Meta, {dict, Type}, Scope, Term) ->
%    case {Type, term_type(Term), symbol:is(Term)} of
%        {pattern, pair, _}    -> iter(Meta, pattern, Scope, Term);
%        {expr, pair, _}       -> iter(Meta, {pair, expr}, Scope, Term);
%        {_, _, false}         -> error:format({illegal_dict_element, term_type(Term), Type}, {ast, Term});
%        {pattern, Symbol, _}  -> term(Meta, pattern, #{}, Symbol); % Also true for lookup expr
%        {expr, _Symbol, _}    -> error:format({illegal_dict_element, symbol:name(Term), Type}, {ast, Term})
%    end;
%
%iter(Meta, {pair, expr}, Scope, {pair, Ctx, Key, Val}) ->
%    error:map2(term(Meta, expr, #{}, Key),
%               term(Meta, expr, Scope, Val),
%               fun({_, TKey}, {ValEnv, TVal}) -> 
%                       {ValEnv, {pair, Ctx, TKey, TVal}} end);

iter(Meta, Type, Scope, {pair, Ctx, Key, Val}) ->
    error:map2(term(Meta, Type, Scope, Key),
               term(Meta, Type, Scope, Val),
               fun({KeyEnv, TKey}, {ValEnv, TVal}) -> 
                       {merge(KeyEnv, ValEnv), {pair, Ctx, TKey, TVal}} end);


iter(_, _, _, {symbol, _, _, _} = Term)           -> {ok, {#{}, Term}};
iter(_, _, _, {variable, _, _, _} = Term)         -> {ok, {#{}, Term}};
iter(_, _, _, {type, _, _, _} = Term)             -> {ok, {#{}, Term}};
iter(_, _, _, {qualified_type, _, _} = Term)      -> {ok, {#{}, Term}};
iter(_, _, _, {qualified_variable, _, _} = Term)  -> {ok, {#{}, Term}};
iter(_, _, _, {qualified_symbol, _, _} = Term)    -> {ok, {#{}, Term}};
iter(_, _, _, {key, _, _} = Term)                 -> {ok, {#{}, Term}};

iter(_, Type, _, Term) ->
    error:format({unrecognized_term, term_type(Term), Type}, {ast, Term}).

map(Meta, Type, Scope, Elements) ->
    Mapped = [term(Meta, Type, Scope, Elem) || Elem <- Elements],
    case error:collect(Mapped) of
        {error, Errs} -> {error, Errs};
        {ok, Zipped} -> {ok, lists:unzip(Zipped)}
    end.

merge(Maps) when is_list(Maps) ->
    lists:foldl(fun maps:merge/2, #{}, Maps).

term_type(Elem) when is_tuple(Elem) -> element(1, Elem).
context(Elem) when is_tuple(Elem) -> element(1, Elem).

get_tag(Key, Term)  -> maps:get(Key, element(2, Term)).

tag(Key, Term)      -> tag(Key, Term, fun(Tag) -> Tag end).
tag(Key, Term, F)   ->
    OldTag = get_tag(Key, Term),
    NewTag = F(OldTag),
    with_tag(Key, NewTag, Term).

with_tag(Key, Tag, {A, B, C}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C)};
with_tag(Key, Tag, {A, B, C, D}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C), add_tag(Key, Tag, D)};
with_tag(Key, Tag, {A, B, C, D, E}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C), add_tag(Key, Tag, D), add_tag(Key, Tag, E)}.

add_tag(Key, Tag, Elements) when is_list(Elements) ->
    [add_tag(Key, Tag, Elem) || Elem <- Elements];
add_tag(Key, Tag, Elem) when is_tuple(Elem) ->
    setelement(2, Elem, maps:put(Key, Tag, element(2, Elem)));
add_tag(Key, Tag, M) when is_map(M) ->
    maps:put(Key, Tag, M);
add_tag(_, _, Other) -> Other.
