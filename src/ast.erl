-module(ast).
-export([traverse/2, traverse/3, traverse/4, traverse/5, 
         term_type/1, context/1, tag/2, tag/3, tag/4, get_tag/2]).
-import(maps, [merge/2]).
-include_lib("eunit/include/eunit.hrl").

traverse(Type, Pre, Post, Scope, AST)               -> climb({Pre, Post}, Type, Scope, AST).
traverse(Pre, Post, Scope, ASTs) when is_list(ASTs) -> map_asts(Pre, Post, Scope, ASTs);
traverse(Pre, Post, Scope, AST)                     -> climb({Pre, Post}, top_level, Scope, AST).
traverse(Pre, Post, ASTs) when is_list(ASTs)        -> map_asts(Pre, Post, #{}, ASTs);
traverse(Pre, Post, AST)                            -> climb({Pre, Post}, top_level, #{}, AST).
traverse(Post, ASTs) when is_list(ASTs)             -> map_asts(fun(_,_,T) -> T end, Post, #{}, ASTs);
traverse(Post, AST)                                 -> climb({fun(_, _, T) -> T end, Post}, top_level, #{}, AST).

climb({Pre, Post}, Type, Scope, Term) ->
    case Pre(Type, Scope, Term) of
        {error, Errs}                   -> {error, Errs};
        skip                            -> {ok, {Scope, Term}};
        ok                              -> chew(Pre, Post, Type, Scope, Term);
        {ok, NewTerm}                   -> chew(Pre, Post, Type, Scope, NewTerm);
        {change, NewPost, NewTerm}      -> chew(Pre, NewPost, Type, Scope, NewTerm)
    end.

chew(Pre, Post, Type, Scope, Term) ->
    case step({Pre, Post}, Type, Scope, Term) of
        {error, Errs}               -> {error, Errs};
        {ok, {Env, TraversedTerm}}  -> 
            case Post(Type, Scope, TraversedTerm) of
                {error, Errs}               -> {error, Errs};
                skip                        -> {ok, {#{}, Term}};
                ok                          -> {ok, {Env, TraversedTerm}};
                {ok, PostTerm}              -> {ok, {Env, PostTerm}};
                {ok, Key, PostTerm}         -> {ok, {maps:put(Key, PostTerm, Env), PostTerm}};
                {ok, Key, Value, PostTerm}  -> {ok, {maps:put(Key, Value, Env), PostTerm}}
            end
    end.

step(Meta, _, Scope, {def, Context, Name, Args, Expr}) when is_list(Args) ->
    case map(Meta, pattern, Scope, Args) of
        {error, Errs}            -> {error, Errs};
        {ok, {ArgsEnvs, TArgs}}  ->
            ExprScope = merge([Scope | ArgsEnvs]),
            RawExpr = case Expr of
                          _ when is_list(Expr)  -> map(Meta, expr, ExprScope, Expr);
                          _                     -> climb(Meta, expr, ExprScope, Expr)
                      end,
            case RawExpr of
                {error, Errs}                           -> {error, Errs};
                {ok, {Envs, TExprs}} when is_list(Envs) -> 
                    {ok, {merge(ArgsEnvs ++ Envs), {def, Context, Name, TArgs, TExprs}}};
                {ok, {Env, TExpr}}                      -> 
                    {ok, {merge([Env | ArgsEnvs]), {def, Context, Name, TArgs, TExpr}}}
            end
    end;

step(Meta, Type, Scope, {type_def, Context, Name, Args, Expr}) when is_list(Args) ->
    case step(Meta, Type, Scope, {def, Context, Name, Args, Expr}) of
        {error, Errs}       -> {error, Errs};
        {ok, {Env, Term}}   -> {ok, {Env, setelement(1, Term, type_def)}}
    end;

step(Meta, expr, Scope, {clause, Context, Patterns, Expr}) when is_list(Patterns) ->
    case map(Meta, pattern, Scope, Patterns) of
        {error, Errs}                       -> {error, Errs};
        {ok, {PatternEnvs, TPatterns}}   -> 
            NewScope = merge([Scope | PatternEnvs]),
            case climb(Meta, expr, NewScope, Expr) of
                {error, Errs}       -> {error, Errs};
                {ok, {Env, TExpr}}  -> 
                    {ok, {merge([Env | PatternEnvs]), {clause, Context, TPatterns, TExpr}}}
            end
    end;

step(Meta, expr, Scope, {lambda, Context, Clauses}) when is_list(Clauses) ->
    case map(Meta, expr, Scope, Clauses) of
        {error, Errs}               -> {error, Errs};
        {ok, {Envs, TClauses}}   -> {ok, {merge(Envs), {lambda, Context, TClauses}}}
    end;

step(Meta, expr, Scope, {val, Context, Pattern, Expr}) ->
    error:map2(climb(Meta, pattern, Scope, Pattern),
               climb(Meta, expr, Scope, Expr),
               fun({PatternEnv, TPattern}, {Env, TExpr}) ->
                       {merge(PatternEnv, Env), {val, Context, TPattern, TExpr}} end);

step(Meta, expr, Scope, {match, Context, Expr, Clauses}) when is_list(Clauses) ->
    error:map2(climb(Meta, expr, Scope, Expr),
               map(Meta, expr, Scope, Clauses),
               fun({ExprEnv, TExpr}, {ClauseEnvs, TClauses}) ->
                       {merge([ExprEnv | ClauseEnvs]), {match, Context, TExpr, TClauses}} end);

step(Meta, Type, Scope, {application, Context, Expr, Args}) when is_list(Args) ->
    error:map2(map(Meta, Type, Scope, Args),
               climb(Meta, Type, Scope, Expr),
               fun({ArgsEnvs, TArgs}, {ExprEnv, TExpr}) -> 
                       {merge([ExprEnv | ArgsEnvs]), {application, Context, TExpr, TArgs}} end);

step(Meta, Type, Scope, {lookup, Context, Expr, Elems}) when is_list(Elems) ->
    error:map2(climb(Meta, Type, Scope, Expr),
               map(Meta, Type, Scope, Elems),
               fun({ExprEnv, TExpr}, {ElemsEnvs, TElems}) ->
                       {merge([ExprEnv | ElemsEnvs]), {lookup, Context, TExpr, TElems}} end);

step(Meta, Type, Scope, {tuple, Context, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}          -> {error, Errs};
        {ok, {Envs, TExprs}}   -> {ok, {merge(Envs), {tuple, Context, TExprs}}}
    end;


step(Meta, expr, Scope, {'let', Context, Pattern, Expr, Term}) ->
    error:flatmap2(climb(Meta, pattern, Scope, Pattern),
                   climb(Meta, expr, Scope, Expr),
                   fun({PatternEnv, TPattern}, {ExprEnv, TExpr}) ->
                           NewScope = merge(Scope, PatternEnv),
                           case climb(Meta, expr, NewScope, Term) of
                               {error, Errs}            -> {error, Errs};
                               {ok, {Env, TTerm}}    -> 
                                   {ok, {merge([PatternEnv, ExprEnv, Env]), 
                                         {'let', Context, TPattern, TExpr, TTerm}}}
                           end
                   end);

step(Meta, expr, Scope, {let_type, Context, Type, Term}) ->
    error:flatmap(climb(Meta, expr, Scope, Type),
                  fun({TypeEnv, TType}) ->
                          NewScope = merge(Scope, TypeEnv),
                          case climb(Meta, expr, NewScope, Term) of
                              {error, Errs}            -> {error, Errs};
                              {ok, {TermEnv, TTerm}}    -> 
                                  {ok, {merge(TypeEnv, TermEnv), 
                                        {let_type, Context, TType, TTerm}}}
                          end
                  end);

step(Meta, expr, Scope, {seq, Context, First, Then}) ->
    case climb(Meta, expr, Scope, First) of
        {error, Errs}               -> {error, Errs};
        {ok, {FirstEnv, TFirst}} -> 
            case climb(Meta, expr, Scope, Then) of
                {error, Errs}           -> {error, Errs};
                {ok, {ThenEnv, TThen}}   -> 
                    {ok, {merge(FirstEnv, ThenEnv), {seq, Context, TFirst, TThen}}}
            end
    end;


step(Meta, Type, Scope, {dict, Context, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}              -> {error, Errs};
        {ok, {ExprsEnvs, TExprs}}   -> 
            {ok, {merge(ExprsEnvs), {dict, Context, TExprs}}}
    end;

%% Element in dictionary
%step(Meta, {dict, Type}, Scope, Term) ->
%    case {Type, climb_type(Term), symbol:is(Term)} of
%        {pattern, pair, _}    -> step(Meta, pattern, Scope, Term);
%        {expr, pair, _}       -> step(Meta, {pair, expr}, Scope, Term);
%        {_, _, false}         -> error:format({illegal_dict_element, climb_type(Term), Type}, {ast, Term});
%        {pattern, Symbol, _}  -> climb(Meta, pattern, #{}, Symbol); % Also true for lookup expr
%        {expr, _Symbol, _}    -> error:format({illegal_dict_element, symbol:name(Term), Type}, {ast, Term})
%    end;
%
%step(Meta, {pair, expr}, Scope, {pair, Ctx, Key, Val}) ->
%    error:map2(climb(Meta, expr, #{}, Key),
%               climb(Meta, expr, Scope, Val),
%               fun({_, TKey}, {ValEnv, TVal}) -> 
%                       {ValEnv, {pair, Ctx, TKey, TVal}} end);

step(Meta, Type, Scope, {pair, Ctx, Key, Val}) ->
    error:map2(climb(Meta, Type, Scope, Key),
               climb(Meta, Type, Scope, Val),
               fun({KeyEnv, TKey}, {ValEnv, TVal}) -> 
                       {merge(KeyEnv, ValEnv), {pair, Ctx, TKey, TVal}} end);


step(_, _, _, {symbol, _, _, _} = Term)           -> {ok, {#{}, Term}};
step(_, _, _, {variable, _, _, _} = Term)         -> {ok, {#{}, Term}};
step(_, _, _, {type, _, _, _} = Term)             -> {ok, {#{}, Term}};
step(_, _, _, {qualified_type, _, _} = Term)      -> {ok, {#{}, Term}};
step(_, _, _, {qualified_variable, _, _} = Term)  -> {ok, {#{}, Term}};
step(_, _, _, {qualified_symbol, _, _} = Term)    -> {ok, {#{}, Term}};
step(_, _, _, {key, _, _} = Term)                 -> {ok, {#{}, Term}};

step(_, Type, _, Term) ->
    error:format({unrecognized_term, term_type(Term), Type}, {ast, Term}).

map(Meta, Type, Scope, Elements) ->
    Mapped = [climb(Meta, Type, Scope, Elem) || Elem <- Elements],
    case error:collect(Mapped) of
        {error, Errs} -> {error, Errs};
        {ok, Zipped} -> {ok, lists:unzip(Zipped)}
    end.

map_asts(Pre, Post, Scope, ASTs) ->
    case error:collect([climb({Pre, Post}, top_level, Scope, AST) || AST <- ASTs]) of
        {error, Errs}   -> {error, Errs};
        {ok, Res}       ->
            {Envs, Outs} = lists:unzip(Res),
            {ok, {merge(Envs), Outs}}
    end.

merge(Maps) when is_list(Maps) ->
    lists:foldl(fun maps:merge/2, #{}, Maps).

term_type(Elem) when is_tuple(Elem) -> element(1, Elem).
context(Elem) when is_tuple(Elem) -> element(2, Elem).

get_tag(Key, Term)      -> maps:get(Key, element(2, Term)).
get_tag(Key, Term, Def) -> maps:get(Key, element(2, Term), Def).

tag(Key, Term)                              -> tag(Key, Term, fun(Tag) -> Tag end).
tag(Key, Term, F) when is_function(F)       -> 
    case get_tag(Key, Term, undefined) of
        undefined   -> error:format({undefined_tag, Key},{ast, Term});
        Tag         -> {ok, with_tag(Key, F(Tag), Term)}
    end;
tag(Key, Term, Tag)                         -> {ok, with_tag(Key, Tag, Term)}.
tag(Key, Term, F, Def) when is_function(F)  ->
    OldTag = get_tag(Key, Term, Def),
    NewTag = F(OldTag),
    NewTerm = with_tag(Key, NewTag, Term),
    {ok, NewTerm}.

with_tag(Key, Tag, {A, B, C}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C)};
with_tag(Key, Tag, {A, B, C, D}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C), add_tag(Key, Tag, D)};
with_tag(Key, Tag, {A, B, C, D, E}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C), add_tag(Key, Tag, D), add_tag(Key, Tag, E)}.

add_tag(Key, Tag, Elements) when is_list(Elements) ->
    [add_tag(Key, Tag, Elem) || Elem <- Elements];
add_tag(Key, Tag, Elem) when is_tuple(Elem) andalso size(Elem) > 2 ->
    setelement(2, Elem, maps:put(Key, Tag, element(2, Elem)));
add_tag(Key, Tag, M) when is_map(M) ->
    maps:put(Key, Tag, M);
add_tag(_, _, Other) -> Other.
