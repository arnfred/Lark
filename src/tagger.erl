-module(tagger).
-export([tag/1, tag/2]).

tag(AST) -> tag(AST, #{}).

tag(AST, ImportScope) ->
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
    end.

merge_scopes(LocalScope, ImportScope) ->
	F = fun(Alias, {type, _, _, _} = Term) -> {ok, {Alias, Term}};
	       (Alias, Term) ->
				case maps:is_key(Alias, LocalScope) of
					false	-> {ok, {Alias, Term}};
					true	->
                        {Tag, Ctx, Module, Name} = Term,
						Import = maps:get(import, Ctx),
                        ModuleName = module:kind_name(Module),
						case Tag of
							qualified_type 	    -> error:format({import_conflicts_with_local_type, 
                                                                 Alias, ModuleName, Name},
                                                                {tagger, Import});
							qualified_variable  -> error:format({import_conflicts_with_local_def, 
                                                                 Alias, ModuleName, Name},
                                                                {tagger, Import})
                        end
                end
        end,
    case error:collect([F(Alias, Term) || {Alias, Term} <- maps:to_list(ImportScope)]) of
        {error, Errs}   -> {error, Errs};
        {ok, Imports}   -> {ok, maps:merge(LocalScope, maps:from_list(Imports))}
    end.


% Step 1: Scan all top-level module definitions
tag_defs_pre(top_level, _, _)                       -> ok;
tag_defs_pre(_, _, _)                               -> skip.

tag_defs_post(top_level, _, {def, _, Name, Args, _}) ->
    {ok, Name, {variable, #{}, Name, {Name, length(Args)}}};
tag_defs_post(top_level, _, {type_def, _, Name, _, _}) ->
    {ok, [Name], {type, #{}, Name, [Name]}};
tag_defs_post(_, _, _) -> ok.


% Step 2: Scan types (but skip types local to a definition)
add_type_path(top_level, _, {def, _, _, _, _})  -> skip;
add_type_path(top_level, _, {type_def, _, Name, _, _} = Term) -> {ok, ast:tag(path, Term, [Name])};
add_type_path(Type, Scope, Term)        -> add_path(Type, Scope, Term).

tag_types(_, Scope, {symbol, Ctx, type, S} = Term) -> 
    % We look for S in case the type is a top-level type
    % We look for path(Term) in case the type is a tag
    case maps:is_key(S, Scope) of
        true    -> {ok, replace(Scope, S, Term)};
        false   -> {ok, path(Term), {type, Ctx, S, path(Term)}}
    end;
tag_types(top_level, _, {type_def, Ctx, Name, _, _} = Term) -> 
    {ok, Name, {type, Ctx, Name, [Name]}, Term};
tag_types(expr, Scope, {type_def, Ctx, Name, _, _} = Term) -> 
    case maps:is_key(Name, Scope) of
        true   -> error:format({type_already_defined, Name}, {tagger, expr, Term});
        false  -> {ok, Name, {type, Ctx, Name, [Name]}, Term}
    end;
tag_types(_Type, _Scope, {symbol, _Ctx, variable, _S} = _Term) -> ok;
tag_types(_, _, _) -> ok.


% Step 3: Scan and tag all types and defs in the module
add_path(_, _, {ast, _, _, _, _} = Term) ->
    {ok, ast:tag(path, Term, fun(Tag) -> Tag end, [])};
add_path(_, _, {def, _, Name, _, _} = Term) ->
    {ok, ast:tag(path, Term, fun(Tag) -> [Name | Tag] end, [])};
add_path(_, _, {type_def, _, Name, _, _} = Term) ->
    {change, fun tag_symbols_and_types/3, ast:tag(path, Term, [Name])};
add_path(_, _, Term) ->
    {ok, ast:tag(path, Term)}.


tag_symbols(Type, Scope, {symbol, _, type, T} = Term) ->
    case maps:is_key(T, Scope) of
        true    -> {ok, replace(Scope, T, Term)};
        false   -> error:format({undefined_type, T}, {tagger, Type, Term})
    end;
tag_symbols(Type, Scope, {symbol, Ctx, variable, S} = Term) ->
    io:format("Path of ~p: ~p~n", [S, path(Term)]),
    case {Type, maps:is_key(S, Scope)} of
        {pattern, true}  -> error:format({symbol_in_pattern_already_defined, S}, {tagger, Type, Term});
        {pattern, false} -> {ok, S, {variable, Ctx, S, symbol:id(path(Term))}};
        {expr, false}    -> error:format({undefined_variable, S}, {tagger, Type, Term});
        {expr, true}     -> {ok, replace(Scope, S, Term)}
    end;
tag_symbols(Type, Scope, {qualified_type, _, Symbols} = Term) ->
    Path = [S || {_, _, _, S} <- Symbols],
    case maps:is_key(Path, Scope) of
        true    -> {ok, replace(Scope, Path, Term)};
        false   -> Tag = symbol:tag({type, #{}, Path, Path}),
                   error:format({undefined_type, Tag}, {tagger, Type, Term})
    end;
tag_symbols(Type, Scope, {qualified_variable, _, Symbols} = Term) ->
    Path = [S || {_, _, _, S} <- Symbols],
    case maps:is_key(Path, Scope) of
        true    -> {ok, replace(Scope, Path, Term)};
        false   -> Tag = symbol:tag({type, #{}, Path, Path}),
                   error:format({undefined_variable, Tag}, {tagger, Type, Term})
    end;
tag_symbols(_, _, _) -> ok.

tag_symbols_and_types(Type, Scope, {symbol, _, variable, _} = Term) -> tag_symbols(Type, Scope, Term);
tag_symbols_and_types(Type, Scope, {qualified_type, _, _} = Term) -> tag_symbols(Type, Scope, Term);
tag_symbols_and_types(Type, Scope, Term) -> tag_types(Type, Scope, Term).


path({symbol, _, _, S} = Term) -> lists:reverse([S | ast:get_tag(path, Term)]).


replace(Scope, Key, {symbol, Ctx, _, _})            -> setelement(2, maps:get(Key, Scope), Ctx);
replace(Scope, Key, {qualified_type, Ctx, _})       -> setelement(2, maps:get(Key, Scope), Ctx);
replace(Scope, Key, {qualified_variable, Ctx, _})   -> setelement(2, maps:get(Key, Scope), Ctx).

    

