-module(ast).
-export([traverse/2, traverse/3, traverse/4, traverse_term/5, 
         term_type/1, context/1, tag/2, tag/3, tag/4, get_tag/2, get_tag/3]).
-import(maps, [merge/2]).
-import(utils, [merge/1]).
-include_lib("eunit/include/eunit.hrl").

traverse(Pre, Post, Scope, ASTs) when is_list(ASTs) -> map_asts(Pre, Post, Scope, ASTs);
traverse(Pre, Post, Scope, AST)                     -> step({Pre, Post}, top_level, Scope, AST).
traverse(Pre, Post, ASTs) when is_list(ASTs)        -> map_asts(Pre, Post, #{}, ASTs);
traverse(Pre, Post, AST)                            -> step({Pre, Post}, top_level, #{}, AST).
traverse(Post, ASTs) when is_list(ASTs)             -> map_asts(fun(_,_,_) -> ok end, Post, #{}, ASTs);
traverse(Post, AST)                                 -> step({fun(_, _, _) -> ok end, Post}, top_level, #{}, AST).

traverse_term(Type, Pre, Post, Scope, Term)         -> in({Pre, Post}, Type, Scope, Term).

in({Pre, Post}, Type, Scope, Term) ->
    case Pre(Type, Scope, Term) of
        {error, Errs}                   -> {error, Errs};
        skip                            -> {ok, {#{}, Term}};
        {skip, NewTerm}                 -> {ok, {#{}, NewTerm}};
        leave_intact                    -> run_post(Post, #{}, Type, Scope, Term);
        ok                              -> out(Pre, Post, Type, Scope, Term);
        {ok, NewTerm}                   -> out(Pre, Post, Type, Scope, NewTerm);
        {ok, Key, NewTerm}              -> out(Pre, Post, Type, maps:put(Key, NewTerm, Scope), NewTerm);
        {ok, Key, Val, NewTerm}         -> out(Pre, Post, Type, maps:put(Key, Val, Scope), NewTerm);
        {change, NewPost, NewTerm}      -> out(Pre, NewPost, Type, Scope, NewTerm);
        Other                           -> error:format({unrecognized_pre_response, Other}, {ast, Term})

    end.

out(Pre, Post, Type, Scope, Term) ->
    case Term of
        {error, Errs}  -> {error, Errs};
        _              ->
            case step({Pre, Post}, Type, Scope, Term) of
                {error, Errs}               -> {error, Errs};
                {ok, {Env, TraversedTerm}}  -> run_post(Post, Env, Type, Scope, TraversedTerm)
            end
    end.

run_post(Post, Env, Type, Scope, Term) ->
    case Post(Type, Scope, Term) of
        {error, Errs}                           -> {error, Errs};
        skip                                    -> {ok, {#{}, Term}};
        ok                                      -> {ok, {Env, Term}};
        {ok, PostTerm}                          -> {ok, {Env, PostTerm}};
        {ok, PostE, PostT} when is_map(PostE)   -> {ok, {maps:merge(Env, PostE), PostT}};
        {ok, Key, PostTerm}                     -> {ok, {maps:put(Key, PostTerm, Env), PostTerm}};
        {ok, Key, Value, PostTerm}              -> {ok, {maps:put(Key, Value, Env), PostTerm}};
        Other                                   -> error:format({unrecognized_post_response, Other}, {ast, Term})
    end.

step(Meta, _Type, Scope, {module, Ctx, Path, Statements}) ->
    case map(Meta, top_level, Scope, Statements) of
        {error, Errs}    -> {error, Errs};
        {ok, {StatEnvs, NewStatements}} ->
            NewEnv = merge(StatEnvs),
            {ok, {NewEnv, {module, Ctx, Path, NewStatements}}}
    end;
step(Meta, _Type, Scope, {module, Ctx, Path, Imports, Exports, Defs}) ->
    case map(Meta, top_level, Scope, maps:values(Defs)) of
        {error, Errs}    -> {error, Errs};
        {ok, {DefEnvs, TDefs}} ->
            NewDefs = maps:from_list(lists:zip(maps:keys(Defs), TDefs)),
            NewEnv = merge(DefEnvs),
            {ok, {NewEnv, {module, Ctx, Path, Imports, Exports, NewDefs}}}
    end;
step(_Meta, _Type, _Scope, {import, _, _} = Term) -> {ok, {#{}, Term}};
step(_Meta, _Type, _Scope, {exports, _, _} = Term) -> {ok, {#{}, Term}};
step(Meta, top_level, Scope, {def, Ctx, Name, Fun}) ->
    step(Meta, expr, Scope, {def, Ctx, Name, Fun});
step(Meta, top_level, Scope, {macro, Ctx, Name, Fun}) ->
    step(Meta, expr, Scope, {macro, Ctx, Name, Fun});

step(Meta, Type, Scope, {def, Ctx, Name, Fun}) ->
    case in(Meta, Type, Scope, Fun) of
        {error, Errs}           -> {error, Errs};
        {ok, {FunEnv, TFun}}    -> {ok, {FunEnv, {def, Ctx, Name, TFun}}}
    end;

step(Meta, Type, Scope, {macro, Ctx, Name, Fun}) ->
    case in(Meta, Type, Scope, Fun) of
        {error, Errs}           -> {error, Errs};
        {ok, {FunEnv, TFun}}    -> {ok, {FunEnv, {macro, Ctx, Name, TFun}}}
    end;

step(Meta, expr, Scope, {clause, Ctx, Patterns, Expr}) ->
    case map(Meta, pattern, Scope, Patterns) of
        {error, Errs}                       -> {error, Errs};
        {ok, {PatternEnvs, TPatterns}}   -> 
            NewScope = merge([Scope | PatternEnvs]),
            case in(Meta, expr, NewScope, Expr) of
                {error, Errs}       -> {error, Errs};
                {ok, {Env, TExpr}}  -> 
                    {ok, {merge([Env | PatternEnvs]), {clause, Ctx, TPatterns, TExpr}}}
            end
    end;

step(Meta, expr, Scope, {'fun', Ctx, Clauses}) when is_list(Clauses) ->
    case map(Meta, expr, Scope, Clauses) of
        {error, Errs}           -> {error, Errs};
        {ok, {Envs, TClauses}}  -> {ok, {merge(Envs), {'fun', Ctx, TClauses}}}
    end;

step(Meta, expr, Scope, {'fun', Ctx, Expr}) ->
    case in(Meta, expr, Scope, Expr) of
        {error, Errs}       -> {error, Errs};
        {ok, {Env, TExpr}}  -> {ok, {Env, {'fun', Ctx, TExpr}}}
    end;

step(Meta, expr, Scope, {val, Ctx, Pattern, Expr}) ->
    error:map2(in(Meta, pattern, Scope, Pattern),
               in(Meta, expr, Scope, Expr),
               fun({PatternEnv, TPattern}, {Env, TExpr}) ->
                       {merge(PatternEnv, Env), {val, Ctx, TPattern, TExpr}} end);

step(Meta, Type, Scope, {application, Ctx, Expr, Args}) ->
    error:map2(map(Meta, expr, Scope, Args),
               in(Meta, Type, Scope, Expr),
               fun({ArgsEnvs, TArgs}, {ExprEnv, TExpr}) -> 
                       {merge([ExprEnv | ArgsEnvs]), {application, Ctx, TExpr, TArgs}} end);

step(Meta, _Type, Scope, {qualified_application, Ctx, ModulePath, Name, Args}) ->
    error:map(map(Meta, expr, Scope, Args),
              fun({ArgsEnvs, TArgs}) -> 
                      {merge(ArgsEnvs), {qualified_application, Ctx, ModulePath, Name, TArgs}} end);

step(Meta, _Type, Scope, {local_application, Ctx, ModulePath, Name, Args}) ->
    error:map(map(Meta, expr, Scope, Args),
              fun({ArgsEnvs, TArgs}) -> 
                      {merge(ArgsEnvs), {local_application, Ctx, ModulePath, Name, TArgs}} end);

step(Meta, _Type, Scope, {beam_application, Ctx, ModulePath, Name, Args}) ->
    error:map(map(Meta, expr, Scope, Args),
              fun({ArgsEnvs, TArgs}) -> 
                      {merge(ArgsEnvs), {beam_application, Ctx, ModulePath, Name, TArgs}} end);

step(Meta, _Type, Scope, {macro_application, Ctx, Name, Args}) ->
    error:map(map(Meta, expr, Scope, Args),
              fun({ArgsEnvs, TArgs}) -> 
                      {merge(ArgsEnvs), {macro_application, Ctx, Name, TArgs}} end);

step(Meta, Type, Scope, {tuple, Ctx, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}          -> {error, Errs};
        {ok, {Envs, TExprs}}   -> {ok, {merge(Envs), {tuple, Ctx, TExprs}}}
    end;

step(Meta, Type, Scope, {sum, Ctx, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}          -> {error, Errs};
        {ok, {Envs, TExprs}}   -> {ok, {merge(Envs), {sum, Ctx, TExprs}}}
    end;

step(Meta, Type, Scope, {list, Ctx, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}          -> {error, Errs};
        {ok, {Envs, TExprs}}   -> {ok, {merge(Envs), {list, Ctx, TExprs}}}
    end;

step(Meta, expr, Scope, {'let', Ctx, Expr, Clauses}) when is_list(Clauses) ->
    error:flatmap(in(Meta, expr, Scope, Expr),
                  fun({ExprEnv, TExpr}) ->
                          case map(Meta, expr, Scope, Clauses) of
                              {error, Errs}             -> {error, Errs};
                              {ok, {Envs, TClauses}}    -> Env = merge([ExprEnv | Envs]),
                                                           {ok, {Env, {'let', Ctx, TExpr, TClauses}}}
                          end
                  end);

step(Meta, expr, Scope, {seq, Ctx, First, Then}) ->
    case in(Meta, expr, Scope, First) of
        {error, Errs}               -> {error, Errs};
        {ok, {FirstEnv, TFirst}} -> 
            case in(Meta, expr, Scope, Then) of
                {error, Errs}           -> {error, Errs};
                {ok, {ThenEnv, TThen}}   -> 
                    {ok, {merge(FirstEnv, ThenEnv), {seq, Ctx, TFirst, TThen}}}
            end
    end;

step(Meta, Type, Scope, {dict, Ctx, Expressions}) when is_list(Expressions) ->
    case map(Meta, Type, Scope, Expressions) of
        {error, Errs}              -> {error, Errs};
        {ok, {ExprsEnvs, TExprs}}   -> 
            {ok, {merge(ExprsEnvs), {dict, Ctx, TExprs}}}
    end;

step(Meta, Type, Scope, {tagged, Ctx, Path, Val}) ->
    error:map(in(Meta, Type, Scope, Val),
	      fun({ValEnv, TVal}) -> {ValEnv, {tagged, Ctx, Path, TVal}} end);

step(Meta, Type, Scope, {TermType, Ctx, Key, Val}) when TermType =:= pair;
                                                        TermType =:= dict_pair ->
    error:map2(in(Meta, Type, Scope, Key),
               in(Meta, Type, Scope, Val),
               fun({KeyEnv, TKey}, {ValEnv, TVal}) -> 
                       {merge(KeyEnv, ValEnv), {TermType, Ctx, TKey, TVal}} end);

step(Meta, Type, Scope, {'or', Ctx, E1, E2}) ->
    error:map2(in(Meta, Type, Scope, E1),
               in(Meta, Type, Scope, E2),
               fun({T1Env, T1}, {T2Env, T2}) -> 
                       {merge(T1Env, T2Env), {'or', Ctx, T1, T2}} end);

step(_, _, _, {symbol, _, _, _} = Term)             -> {ok, {#{}, Term}};
step(_, _, _, {variable, _, _, _} = Term)           -> {ok, {#{}, Term}};
step(_, _, _, {keyword, _, _, _} = Term)            -> {ok, {#{}, Term}};
step(_, _, _, {keyword, _, _} = Term)               -> {ok, {#{}, Term}};
step(_, _, _, {qualified_symbol, _, _} = Term)      -> {ok, {#{}, Term}};
step(_, _, _, {qualified_symbol, _, _, _} = Term)   -> {ok, {#{}, Term}};
step(_, _, _, {beam_symbol, _, _, _} = Term)        -> {ok, {#{}, Term}};
step(_, _, _, {link, _, _, _} = Term)               -> {ok, {#{}, Term}};

step(_, _, _, {value, _, _, _} = Term)              -> {ok, {#{}, Term}};

step(_, Type, _, Term) ->
    error:format({unrecognized_term, Term, term_type(Term), Type}, {ast, Term}).

map(Meta, Type, Scope, Elements) ->
    Mapped = [in(Meta, Type, Scope, Elem) || Elem <- Elements],
    case error:collect(Mapped) of
        {error, Errs} -> {error, Errs};
        {ok, Zipped} -> {ok, lists:unzip(Zipped)}
    end.

map_asts(Pre, Post, Scope, ASTs) ->
    case error:collect([in({Pre, Post}, top_level, Scope, AST) || AST <- ASTs]) of
        {error, Errs}   -> {error, Errs};
        {ok, Res}       ->
            {Envs, Outs} = lists:unzip(Res),
            {ok, {merge(Envs), Outs}}
    end.

term_type(Elem) when is_tuple(Elem) -> element(1, Elem).
context(Elem) when is_tuple(Elem) -> element(2, Elem).

get_tag(Key, Term)      -> maps:get(Key, element(2, Term)).
get_tag(Key, Term, Def) -> maps:get(Key, element(2, Term), Def).

tag(Key, Term)                              -> tag(Key, Term, fun(Tag) -> Tag end).
tag(Key, Term, F) when is_function(F)       -> 
    case get_tag(Key, Term, undefined) of
        undefined   -> error:format({undefined_tag, Key},{ast, Term});
        Tag         -> with_tag(Key, F(Tag), Term)
    end;
tag(Key, Term, Tag)                         -> with_tag(Key, Tag, Term).
tag(Key, Term, F, Def) when is_function(F)  ->
    OldTag = get_tag(Key, Term, Def),
    NewTag = F(OldTag),
    with_tag(Key, NewTag, Term).

with_tag(Key, Tag, {ast, Ctx, Modules, Imports, Defs}) ->
    {ast, add_tag(Key, Tag, Ctx), 
          add_tag(Key, Tag, Modules), 
          add_tag(Key, Tag, Imports),
          maps:from_list([{K, add_tag(Key, Tag, V)} || {K, V} <- maps:to_list(Defs)])};
with_tag(Key, Tag, {A, B, C}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C)};
with_tag(Key, Tag, {A, B, C, D}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C), add_tag(Key, Tag, D)};
with_tag(Key, Tag, {A, B, C, D, E}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C), add_tag(Key, Tag, D), add_tag(Key, Tag, E)};
with_tag(Key, Tag, {A, B, C, D, E, F}) ->
    {A, add_tag(Key, Tag, B), add_tag(Key, Tag, C), add_tag(Key, Tag, D), add_tag(Key, Tag, E), add_tag(Key, Tag, F)}.

add_tag(Key, Tag, Elements) when is_list(Elements) ->
    [add_tag(Key, Tag, Elem) || Elem <- Elements];
add_tag(Key, Tag, Elem) when is_tuple(Elem) andalso size(Elem) > 2 ->
    setelement(2, Elem, maps:put(Key, Tag, element(2, Elem)));
add_tag(Key, Tag, M) when is_map(M) ->
    maps:put(Key, Tag, M);
add_tag(_, _, Other) -> Other.
