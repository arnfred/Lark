-module(new_tagger).
-export([tag/1]).

tag(AST) ->
    ast:traverse(path, fun add_path/1, fun type_tagger/3, expr, #{}, AST).

add_path({def, Ctx, Name, _, _, _}) -> [ast:term_type(Name) | maps:get(path, Ctx, [])];
add_path({type_def, Ctx, Name, _, _, _}) -> [ast:term_type(Name) | maps:get(path, Ctx, [])];
add_path(Term) -> ast:context(Term).

type_tagger(_, {def, _, _, _, _} = Term, _) -> {#{}, Term};
type_tagger(_, _, TermF) ->
    case TermF() of
        {error, Errs}                           -> {error, Errs};
        {ok, {Env, {symbol, _, _, _} = Term}}   -> ok
    end.

tag_symbol({symbol, Ctx, variable, S}) ->
    Path = maps:get(path, Ctx),
    VarPath = lists:reverse([S | Path]),
    Tag = symbol:id(VarPath),
    {#{S => Tag}, {variable, Ctx, S, Tag}};
tag_symbol({symbol, Ctx, type, T}) ->
    Path = maps:get(path, Ctx),
    {#{T => {type, Ctx, lists:reverse([T | Path])}}}.


post_type_tagger(_, _, TermF) ->
    Tag = fun(TermType, Ctx, S, T) -> {#{S => T}, {TermType, Ctx, S, T}} end,
    case TermF() of
        {error, Errs}                       -> {error, Errs};
        {ok, {Env, {type_symbol, C, S}}}    -> Tag(type, C, S, maps:get(S, Env, symbol:id(S)));
        {ok, {Env, {symbol, C, S}}}         -> Tag(var, C, S, maps:get(S, Env, symbol:id(S)));
        {ok, {Env, Term}}                   -> {Env, Term}
    end.
