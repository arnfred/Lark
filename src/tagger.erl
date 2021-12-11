-module(tagger).
-export([tag/1]).

-include_lib("eunit/include/eunit.hrl").

tag({module, _, Path, ImportScope, _Exports, Defs} = Module) ->
    MacroScope = maps:from_list([{Name, true} || {macro, _, Name, _} <- maps:values(Defs)]),
    case ast:traverse(fun(_, _, _) -> ok end, fun tag_macros/3, MacroScope, Module) of
        {error, Errs}   -> {error, Errs};
        {ok, {_, MacroedModule}}  ->
            LocalScope = maps:from_list([{Name, tag_def(Name, Def, Path)} || {Name, Def} <- maps:to_list(Defs)]),
            case merge_scopes(Path, LocalScope, ImportScope) of
                {error, Errs}   -> {error, Errs};
                {ok, Scope}     -> ast:traverse(fun(_, _, _) -> ok end, fun tag_symbols/3, Scope, MacroedModule)
            end
    end.

% maps:merge is simpler, but we want to error when an import conflicts with a
% local definition
merge_scopes(ModulePath, LocalScope, ImportScope) ->
    F = fun(Alias, Term) ->
                case maps:is_key(Alias, LocalScope) of
                    false	-> {ok, {Alias, Term}};
                    true	->
                        {_, Ctx, Module, Name} = Term,
                        Import = maps:get(import, Ctx),
                        ImportName = module:kind_name(Module ++ [Name]),
                        error:format({import_conflicts_with_local_def, Alias, module:kind_name(ModulePath), ImportName},
                                     {tagger, Import})
                end
        end,

    case error:collect([F(Alias, Term) || {Alias, Term} <- maps:to_list(ImportScope)]) of
        {error, Errs}   -> {error, Errs};
        {ok, Imports}   -> {ok, maps:merge(LocalScope, maps:from_list(Imports))}
    end.

% Step 1: Tag all macros
tag_macros(_, Scope, {application, _, {symbol, Ctx, _, S}, Args} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> {ok, Term};
        true    -> {ok, {macro_application, Ctx, S, Args}}
    end;
tag_macros(_, _, _) -> ok.

% Step 2: Build local scope of all top-level module definitions
tag_def(Tag, {def, _, _, _}, Path) ->
    {qualified_symbol, #{}, Path, Tag};
tag_def(Tag, {macro, _, _, _}, Path) ->
    {macro_symbol, #{}, Path, Tag};
tag_def(_, {keyword, _, _, _} = Keyword, _) -> Keyword;
tag_def(_, {link, _, Path, Symbol}, _) -> {qualified_symbol, #{}, Path, Symbol}.


% Step 3: tag all symbols
tag_symbols(expr, Scope, {symbol, _, variable, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> error:format({undefined_symbol, S}, {tagger, expr, Term});
        true    -> {ok, replace(Scope, S, Term)}
    end;
tag_symbols(pattern, Scope, {symbol, Ctx, variable, S} = Term) ->
    case maps:is_key(S, Scope) of
        % When variables are in scope, using them in a pattern will refer to their domain
        true    -> {ok, replace(Scope, S, Term)};
        % When variables aren't in scope, they are created
        false   -> {ok, S, {variable, Ctx, S, symbol:id([ast:get_tag(parent, Term), S])}}
    end;


tag_symbols(_, _, {symbol, Ctx, keyword, '_'}) -> {ok, {keyword, Ctx, '_'}};
tag_symbols(Type, Scope, {symbol, Ctx, keyword, Tag} = Term) ->
    Parent = maps:get(parent, Ctx),
    LocalTag = symbol:tag([Parent, Tag]),
    % First check if the Tag is in scope
    case maps:is_key(Tag, Scope) of
        true    -> {ok, replace(Scope, Tag, Term)};
        % If not in scope, check if refers to keyword defined within same def
        false   -> case maps:is_key(LocalTag, Scope) of
                       false    -> error:format({undefined_symbol, Tag}, {tagger, Type, Term});
                       true    -> {ok, replace(Scope, LocalTag, Term)}
                   end
    end;

tag_symbols(Type, Scope, {symbol, _, _, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> error:format({undefined_symbol, S}, {tagger, Type, Term});
        true    -> {ok, replace(Scope, S, Term)}
    end;

tag_symbols(Type, Scope, {qualified_symbol, _, Symbols} = Term) ->
    Tag = symbol:tag([S || {_, _, _, S} <- Symbols]),
    case maps:is_key(Tag, Scope) of
        false   -> error:format({undefined_symbol, Tag}, {tagger, Type, Term});
        true    -> {ok, replace(Scope, Tag, Term)}
    end;

tag_symbols(Type, Scope, {tagged, Ctx, Path, Expr} = Term) ->
    Tag = symbol:tag(Path),
    case maps:get(Tag, Scope, maps:get(lists:last(Path), Scope, undefined)) of
        undefined                           -> error:format({undefined_symbol, Tag},
                                                            {tagger, Type, Term});
        {qualified_symbol, _, ModPath, Key} -> NewPath = ModPath ++ symbol:path(Key),
                                               {ok, {tagged, Ctx, NewPath, Expr}};
        Keyword                             -> error:format({misdefined_symbol, Tag, Keyword},
                                                            {tagger, Type, Term})
    end;

tag_symbols(_, _, _) -> ok.

replace(Scope, Key, {_, Ctx, _, _})            -> 
    NewCtx = maps:merge(element(2, maps:get(Key, Scope)), Ctx),
    setelement(2, maps:get(Key, Scope), NewCtx);
replace(Scope, Key, {_, Ctx, _})     -> 
    NewCtx = maps:merge(element(2, maps:get(Key, Scope)), Ctx),
    setelement(2, maps:get(Key, Scope), NewCtx).
