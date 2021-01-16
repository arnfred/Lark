-module(tagger).
-export([tag/1, tag/2]).

-include_lib("eunit/include/eunit.hrl").

tag(AST) -> tag(AST, #{}).

tag({ast, _, _, _, TopLevelDefs} = UnexpandedAST, ImportScope) ->
    MacroScope = maps:from_list([{Name, true} || {macro, _, Name, _} <- maps:values(TopLevelDefs)]),
    case ast:traverse(fun(_, _, _) -> ok end, fun tag_macros/3, MacroScope, UnexpandedAST) of
        {error, Errs}   -> {error, Errs};
        {ok, {_, AST}}  ->
            case ast:traverse(fun tag_defs_pre/3, fun tag_defs_post/3, AST) of
                {error, Errs}   -> {error, Errs};
                {ok, {Defs, _}} ->
                    case ast:traverse(fun add_type_path/3, fun tag_types/3, Defs, AST) of
                        {error, Errs}       -> {error, Errs};
                        {ok, {Types, _}}    ->
                            LocalScope = maps:merge(Types, Defs),
                            case merge_scopes(LocalScope, ImportScope) of
                                {error, Errs}   -> {error, Errs};
                                {ok, Scope}     -> ast:traverse(fun add_path/3, fun tag_symbols/3, Scope, AST)
                            end
                    end
            end
    end.

merge_scopes(LocalScope, ImportScope) ->
	F = fun(Alias, {type, _, _, _} = Term) -> {ok, {Alias, Term}};
	       (Alias, Term) ->
				case maps:is_key(Alias, LocalScope) of
					false	-> {ok, {Alias, Term}};
					true	->
                        {_, Ctx, Module, Name} = Term,
						Import = maps:get(import, Ctx),
                        ImportName = module:kind_name(Module ++ [Name]),
					    error:format({import_conflicts_with_local_def, Alias, ImportName},
                                     {tagger, Import})
                end
        end,

    % If a local type has been imported, we want the qualified name to refer to the import,
    % rather than the local name. For example:
    %
    % ```
    % type A -> B | C
    % import A/_
    % type T -> R | B
    % def f -> T/B
    % ```
    %
    % Here we want `f` to return `A/B` instead of `T/B`, because in the
    % definition of `T`, the type `B` refers to the imported type and not a
    % fresh type
    R = fun(V, ImportScope) -> Name = symbol:name(V),
                               case maps:is_key(Name, ImportScope) of
                                   true     -> replace(ImportScope, Name, V);
                                   false    -> V
                               end end,
    Scope = maps:from_list([{K, R(V, ImportScope)} || {K, V} <- maps:to_list(LocalScope)]),

    case error:collect([F(Alias, Term) || {Alias, Term} <- maps:to_list(ImportScope)]) of
        {error, Errs}   -> {error, Errs};
        {ok, Imports}   -> {ok, maps:merge(Scope, maps:from_list(Imports))}
    end.

% Step 1: Tag all macros
tag_macros(_, Scope, {application, _, {symbol, Ctx, _, S}, Args} = Term) ->
    case maps:is_key(S, Scope) of
        false   -> {ok, Term};
        true    -> {ok, {macro_application, Ctx, S, Args}}
    end;
tag_macros(_, _, _) -> ok.

% Step 2: Scan all top-level module definitions
tag_defs_pre(top_level, _, _)                       -> ok;
tag_defs_pre(_, _, _)                               -> skip.

tag_defs_post(top_level, _, {def, _, Name, Expr}) ->
    case Expr of
        {'fun', _, [{clause, _, Patterns, _} | _]}  ->
            {ok, Name, {variable, #{}, Name, {Name, length(Patterns)}}};
        _                                           ->
            {ok, Name, {variable, #{}, Name, {Name, 0}}}
    end;
tag_defs_post(top_level, _, {type_def, _, Name, _}) ->
    {ok, Name, {type, #{}, Name, [Name]}};
tag_defs_post(_, _, _) -> ok.


% Step 3: Scan types (but skip types local to a definition)
add_type_path(top_level, _, {def, _, _, _})  -> skip;
add_type_path(top_level, _, {macro, _, _, _})  -> skip;
add_type_path(top_level, _, {type_def, _, Name, _} = Term) -> {ok, ast:tag(path, Term, [Name])};
add_type_path(expr, Scope, {pair, Ctx, Key, Val}) ->
    case Key of
        % Rewrite operators to types when they are the key of a pair
        {symbol, ValCtx, operator, S}   -> 
            add_path(expr, Scope, {pair, Ctx, {symbol, ValCtx, type, S}, Val});
        _ -> add_path(expr, Scope, {pair, Ctx, Key, Val})
    end;
add_type_path(Type, Scope, Term)        -> add_path(Type, Scope, Term).

tag_types(_, Scope, {symbol, Ctx, type, S} = Term) -> 
    case maps:is_key(S, Scope) of
        true    -> {ok, replace(Scope, S, Term)};
        false   -> {ok, symbol:tag(path(Term)), {type, Ctx, S, path(Term)}}
    end;
tag_types(_, Scope, {symbol, _, operator, S} = Term) -> 
    case maps:is_key(S, Scope) of
        true    -> {ok, replace(Scope, S, Term)};
        false   -> {ok, Term}
    end;
tag_types(top_level, _, {type_def, Ctx, Name, _} = Term) -> 
    {ok, Name, {type, Ctx, Name, [Name]}, Term};
tag_types(expr, Scope, {type_def, Ctx, Name, _} = Term) -> 
    case maps:is_key(Name, Scope) of
        true   -> error:format({type_already_defined, Name}, {tagger, expr, Term});
        false  -> {ok, Name, {type, Ctx, Name, [Name]}, Term}
    end;
tag_types(_Type, _Scope, {symbol, _Ctx, variable, _S} = _Term) -> ok;
tag_types(_, _, _) -> ok.


% Step 3: Scan and tag all types and defs in the module
add_path(_, _, {ast, _, _, _, _} = Term) ->
    {ok, ast:tag(path, Term, fun(Tag) -> Tag end, [])};
add_path(_, _, {def, _, Name, _} = Term) ->
    {ok, ast:tag(path, Term, fun(Tag) -> [Name | Tag] end, [])};
add_path(_, _, {macro, _, Name, _} = Term) ->
    {ok, ast:tag(path, Term, fun(Tag) -> [Name | Tag] end, [])};
add_path(_, _, {type_def, _, Name, _} = Term) ->
    {change, fun tag_symbols_and_types/3, ast:tag(path, Term, [Name])};
add_path(_, _, Term) ->
    {ok, ast:tag(path, Term)}.


tag_symbols(Type, Scope, {symbol, Ctx, variable, S} = Term) ->
    case {Type, maps:is_key(S, Scope)} of
        {expr, false}    -> error:format({undefined_variable, S}, {tagger, Type, Term});
        {expr, true}     -> {ok, replace(Scope, S, Term)};
        {pattern, _}     -> {ok, S, {variable, Ctx, S, symbol:id(path(Term))}}
    end;
tag_symbols(Type, Scope, {symbol, _, SymbolType, T} = Term) ->
    case maps:is_key(T, Scope) of
        true    -> {ok, replace(Scope, T, Term)};
        false   -> error:format({undefined_symbol, SymbolType, T}, {tagger, Type, Term})
    end;
tag_symbols(Type, Scope, {qualified_symbol, _, Symbols} = Term) ->
    Tag = symbol:tag([S || {_, _, _, S} <- Symbols]),
    case maps:is_key(Tag, Scope) of
        true    -> {ok, replace(Scope, Tag, Term)};
        false   -> error:format({undefined_symbol, Tag}, {tagger, Type, Term})
    end;
tag_symbols(_, _, _) -> ok.

tag_symbols_and_types(Type, Scope, {symbol, _, variable, _} = Term) -> tag_symbols(Type, Scope, Term);
tag_symbols_and_types(Type, Scope, {qualified_symbol, _, _} = Term) -> tag_symbols(Type, Scope, Term);
tag_symbols_and_types(Type, Scope, Term) -> tag_types(Type, Scope, Term).


path({symbol, _, _, S} = Term) -> lists:reverse([S | ast:get_tag(path, Term)]).


replace(Scope, Key, {_, Ctx, _, _})            -> 
    NewCtx = maps:merge(element(2, maps:get(Key, Scope)), Ctx),
    setelement(2, maps:get(Key, Scope), NewCtx);
replace(Scope, Key, {_, Ctx, _})     -> 
    NewCtx = maps:merge(element(2, maps:get(Key, Scope)), Ctx),
    setelement(2, maps:get(Key, Scope), NewCtx).
