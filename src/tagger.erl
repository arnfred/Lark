-module(tagger).
-export([tag/1]).

-include_lib("eunit/include/eunit.hrl").

tag({module, _, Path, ImportScope, _Exports, Defs} = Module) ->
    MacroScope = maps:from_list([{Name, true} || {macro, _, Name, _} <- maps:values(Defs)]),
    case ast:traverse(fun(_, _, _) -> ok end, fun tag_macros/3, MacroScope, Module) of
        {error, Errs}   -> {error, Errs};
        {ok, {_, MacroedModule}}  ->
            LocalScope = maps:from_list([{Name, tag_def(Def)} || {Name, Def} <- maps:to_list(Defs)]),
            case merge_scopes(Path, LocalScope, ImportScope) of
                {error, Errs}   -> {error, Errs};
                {ok, Scope}     -> ast:traverse(fun(_, _, _) -> ok end, fun tag_symbols/3, Scope, MacroedModule)
            end
    end.

% maps:merge is simpler, but we want to error when an import conflicts with a
% local definition
merge_scopes(ModulePath, LocalScope, ImportScope) ->
    F = fun(Alias, {type, _, _, _} = Term) -> {ok, {Alias, Term}};
           (Alias, Term) ->
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
tag_def({def, _, Name, Expr}) ->
    case Expr of
        {'fun', _, [{clause, _, Patterns, _} | _]}  ->
            {variable, #{}, Name, {Name, length(Patterns)}};
        _                                           ->
            {variable, #{}, Name, {Name, 0}}
    end;
tag_def({link, _, Symbol}) -> Symbol;
tag_def({type_def, _, Name, _Expr}) -> 
    {type, #{}, Name, [Name]}.


% Step 3: tag all symbols
tag_symbols(expr, Scope, {symbol, _, variable, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> error:format({undefined_symbol, S}, {tagger, expr, Term});
        true    -> {ok, replace(Scope, S, Term)}
    end;
tag_symbols(pattern, _Scope, {symbol, Ctx, variable, S} = Term) ->
    {ok, S, {variable, Ctx, S, id(Term)}};

tag_symbols(Type, Scope, {symbol, _, operator, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> error:format({undefined_symbol, S}, {tagger, Type, Term});
        true    -> {ok, replace(Scope, S, Term)}
    end;

tag_symbols(expr, Scope, {symbol, Ctx, type, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> {ok, S, {type, Ctx, S, [ast:get_tag(parent, Term), S]}};
        true    -> {ok, replace(Scope, S, Term)}
    end;
tag_symbols(pattern, Scope, {symbol, _, type, S} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> error:format({undefined_symbol, S}, {tagger, pattern, Term});
        true    -> {ok, replace(Scope, S, Term)}
    end;

tag_symbols(Type, Scope, {qualified_symbol, _, Symbols} = Term) ->
    Tag = symbol:tag([S || {_, _, _, S} <- Symbols]),
    case maps:is_key(Tag, Scope) of
        true    -> {ok, replace(Scope, Tag, Term)};
        false   -> error:format({undefined_symbol, Tag}, {tagger, Type, Term})
    end;

tag_symbols(_, _, _) -> ok.

id({symbol, _, _, S} = Term) -> symbol:id([ast:get_tag(parent, Term), S]).

replace(Scope, Key, {_, Ctx, _, _})            -> 
    NewCtx = maps:merge(element(2, maps:get(Key, Scope)), Ctx),
    setelement(2, maps:get(Key, Scope), NewCtx);
replace(Scope, Key, {_, Ctx, _})     -> 
    NewCtx = maps:merge(element(2, maps:get(Key, Scope)), Ctx),
    setelement(2, maps:get(Key, Scope), NewCtx).
