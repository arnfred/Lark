-module(tagger).
-export([tag/1]).

tag({module, _, Path, ImportScope, _Exports, Defs} = Module) ->
    LocalScope = [{Name, tag_def(Name, Def, Path)} || {Name, Def} <- maps:to_list(Defs)],
    ScopePairs = LocalScope ++ [{A, T} || {A, Terms} <- maps:to_list(ImportScope), T <- Terms],
    Scope = maps:from_list(utils:group_by(ScopePairs)),
    ast:traverse(fun(_, _, _) -> ok end, fun tag_symbols/3, Scope, Module).


% Step 1: Build local scope of all top-level module definitions
tag_def(Tag, {def, _, _, _}, Path) ->
    {qualified_symbol, #{}, Path, Tag};
tag_def(Tag, {macro, _, _, _}, Path) ->
    {qualified_symbol, #{}, Path, Tag};
tag_def(_, {keyword, _, _, _} = Keyword, _) -> Keyword;
tag_def(_, {link, _, Path, Symbol}, _) -> {qualified_symbol, #{}, Path, Symbol}.


% Step 2: tag all symbols
tag_symbols(expr, Scope, {symbol, Ctx, variable, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> error:format({undefined_symbol, S}, {tagger, expr, Term});
        true    -> {ok, replace(Scope, S, Ctx)}
    end;
tag_symbols(pattern, Scope, {symbol, Ctx, variable, S} = Term) ->
    case maps:is_key(S, Scope) of
        % When variables are in scope, using them in a pattern will refer to their domain
        true    -> {ok, replace(Scope, S, Ctx)};
        % When variables aren't in scope, they are created
        false   -> Var = {variable, Ctx, S, symbol:id([ast:get_tag(parent, Term), S])},
                   {ok, S, [Var], Var}
    end;


tag_symbols(_, _, {symbol, Ctx, keyword, '_'}) -> {ok, {keyword, Ctx, '_'}};
tag_symbols(Type, Scope, {symbol, Ctx, keyword, Tag} = Term) ->
    Parent = maps:get(parent, Ctx),
    LocalTag = symbol:tag([Parent, Tag]),
    % First check if the Tag is in scope
    case maps:is_key(Tag, Scope) of
        true    -> {ok, replace(Scope, Tag, Ctx)};
        % If not in scope, check if refers to keyword defined within same def
        false   -> case maps:is_key(LocalTag, Scope) of
                       false    -> error:format({undefined_symbol, Tag}, {tagger, Type, Term});
                       true    -> {ok, replace(Scope, LocalTag, Ctx)}
                   end
    end;

tag_symbols(Type, Scope, {symbol, Ctx, _, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> error:format({undefined_symbol, S}, {tagger, Type, Term});
        true    -> {ok, replace(Scope, S, Ctx)}
    end;

tag_symbols(Type, Scope, {qualified_symbol, Ctx, Symbols} = Term) ->
    Tag = symbol:tag([S || {_, _, _, S} <- Symbols]),
    case maps:is_key(Tag, Scope) of
        false   -> error:format({undefined_symbol, Tag}, {tagger, Type, Term});
        true    -> {ok, replace(Scope, Tag, Ctx)}
    end;

tag_symbols(Type, Scope, {tagged, Ctx, Path, Expr} = Term) ->
    Tag = symbol:tag(Path),
    case maps:get(Tag, Scope, maps:get(lists:last(Path), Scope, undefined)) of
        undefined                               -> error:format({undefined_symbol, Tag},
                                                                {tagger, Type, Term});
        [{qualified_symbol, _, ModPath, Key}]   -> NewPath = ModPath ++ symbol:path(Key),
                                                   {ok, {tagged, Ctx, NewPath, Expr}};
        Keyword                                 -> error:format({misdefined_symbol, Tag, Keyword},
                                                                {tagger, Type, Term})
    end;

tag_symbols(_, _, _) -> ok.

replace(Scope, Key, Ctx) -> 
    Val = case maps:get(Key, Scope) of
              [V]               -> V;
              L when is_list(L) -> {overloaded, Ctx, Key, L}
          end,
    NewCtx = maps:merge(element(2, Val), Ctx),
    setelement(2, Val, NewCtx).
