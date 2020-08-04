-module(parser).
-export([parse/2]).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

parse(text, Texts) ->
    case error:collect([to_ast(no_file, T) || T <- Texts]) of
        {error, Errs}   -> {error, Errs};
        {ok, ASTs}      -> 
            case format(ASTs) of
                {error, Errs}   -> {error, Errs};
                {ok, Parsed}    -> {ok, Parsed}
            end
    end;

parse(paths, Paths) ->
    case error:collect([traverse(P) || P <- Paths]) of
        {error, Errs}   -> {error, Errs};
        {ok, FileList}   ->
			Files = lists:flatten(FileList),
            case error:collect([load(F) || F <- Files]) of
                {error, Errs}   -> {error, Errs};
                {ok, Sources}    ->
                    case error:collect([to_ast(Path, Source) || {Path, Source} <- lists:zip(Files, Sources)]) of
                        {error, Errs}   -> {error, Errs};
                        {ok, ASTs}      -> 
                            case format(ASTs) of
                                {error, Errs}   -> {error, Errs};
                                {ok, Formatted} -> {ok, Formatted}
                            end
                    end
            end
    end.

format(Sources) ->
    case module:format(Sources) of
        {error, Errs}   -> {error, Errs};
        {ok, ParsedSources}    ->
            SourceDefMap = maps:from_list([{module:beam_name(Name), Module} || {_, {ast, _, Mods, _, _}} <- Sources,
																			   {module, _, Name, _} = Module <- Mods]),
            CollectedTypes = [types(Path, AST) || {Path, AST} <- ParsedSources],
                                                        
            case error:collect(CollectedTypes) of
                {error, Errs}   -> {error, Errs};
                {ok, Types}     ->
                    TypesByPath = maps:from_list([{Path, T} || {Path, _, T} <- Types]),
                    SourceTypeList = [{Name, S} || {_, Modules, T}             <- Types,
                                                   M                           <- Modules,
                                                   {module, _, Name, _} = S <- type_source(M, T)],
                    SourceMap = maps:merge(SourceDefMap, maps:from_list(SourceTypeList)),
                    error:collect([import_and_tag(Path, AST, SourceMap, maps:get(Path, TypesByPath)) ||
                                 {Path, AST} <- ParsedSources])
            end
    end.

traverse(Path) ->
    case {filelib:is_dir(Path), filelib:is_file(Path)} of
        {true, _} -> traverse(dir, Path);
        {_, true} -> traverse(file, Path);
        _         -> error:format({malformed_path, Path}, {sourcemap})
    end.

traverse(dir, Path) -> 
    case file:list_dir(Path) of
        {error, Err}     -> error:format({error_listing_dir_files, Path, Err}, {sourcemap});
        {ok, List}       -> 
            case error:collect([traverse(filename:absname_join(Path, File)) || File <- List]) of
                {error, Errs}   -> {error, Errs};
                {ok, Files}     -> {ok, lists:flatten(Files)}
            end
    end;

traverse(file, Path) ->
    case filename:extension(Path) of
        ".kind" ++ _    -> {ok, {source, Path}};
        _               -> []
    end.

% Next steps:
% 1. Tag types so they are tagged with their parent: {Parent, lists:last(T)}
% 2. Get all keys in the env and use group_by to build a new map: Parent -> [T1, T2]
% 3. Create a map keyed on paths
% 4. Create a map keyed on modules
% 5. Use the map keyed on paths to make sure that local types are provided
% 6. Use the map keyed on modules to make sure that types for other source modules are provided
%
% Important: When doing code generation, I'll need to create separate modules for the types exported in a module!
% That way, when I export a type, I can list it's subtypes as separate domain functions in this module.
% That way, I can treat an import of `a/b/T/_` as a normal import and just import all the functions in the beam file.

types_pre(top_level, _, {type_def, _, Name, _, _} = Term) -> 
	{ok, ast:tag(parent, Term, Name)};
types_pre(top_level, _, _)  -> skip;
types_pre(_, _, Term) -> {ok, ast:tag(parent, Term)}.

types_post(expr, _, {pair, _, {symbol, _, type, Name}, _} = Term) -> 
	Parent = ast:get_tag(parent, Term),
	{ok, {Parent, Name}, Term};
types_post(expr, _, {symbol, _, type, Name} = Term) -> 
	Parent = ast:get_tag(parent, Term),
	{ok, {Parent, Name}, Term};
types_post(_, _, _) -> ok.


types(Path, AST) ->
	{ast, _, Modules, _, _} = AST,
	case ast:traverse(fun types_pre/3, fun types_post/3, AST) of
		{error, Errs}   -> 
			{error, Errs};
		{ok, {Env, _}}  -> 
			GetKey = fun({K, _}) -> K end,
			GetVal = fun({_, V}) -> V end,
			Types = maps:from_list(utils:group_by(GetKey, GetVal, maps:keys(Env))),
			{Path, Modules, Types}
	end.


type_source({module, Ctx, Name, Exports}, Types) ->
    F = fun(Type, Members) ->
                Exports = maps:from_list([{T, T} || T <- Members]),
                {module, Ctx, Name ++ [Type], Exports}
        end,
    [F(T, Members) || {T, Members} <- maps:to_list(Types), maps:is_key(T, Exports)].

load({source, Path}) ->
    case file:read_file(Path) of
        {error, Err}    -> error:format({malformed_source_file, Err, Path}, {sourcemap, Path});
        {ok, Data}      -> unicode:characters_to_list(Data, utf8)
    end.

to_ast(Path, Source) -> 
    case lexer:string(Source) of
        {error, Error}          -> error:format({lexer_error, Error},{sourcemap, Path});
        {error, Error1, Error2} -> error:format({lexer_error, Error1, Error2},{sourcemap, Path});
        {ok, Tokens, _}         ->
            case grammar:parse(Tokens) of
                {error, Error}  -> error:format({parser_error, Error}, {sourcemap, Path});
                {ok, Parsed}    ->
                    case preener:preen(Parsed) of
                        {error, Errs}   -> {error, Errs};
                        {ok, Preened}       -> {ok, {Path, Preened}}
                    end
            end
    end.

import_and_tag(Path, {ast, _, _, ImportClauses, _} = AST, SourceMap, LocalTypes) ->
    F = fun(Import) -> import:import(Import, SourceMap, LocalTypes) end,
    case error:collect(lists:map(F, ImportClauses)) of
        {error, Errs} -> {error, Errs};
        {ok, NestedAliases} ->
			Aliases = lists:flatten(NestedAliases),
            ErrFun = fun({alias, ErrorCtx1, Alias, _},
						 {alias, ErrorCtx2, _, _}) -> error:format({duplicate_import, Alias},
																		{import, ErrorCtx1, ErrorCtx2}) end,
            Duplicates = utils:duplicates(Aliases, fun({alias, _, A, _}) -> A end),
            case Duplicates =:= [] of
                false   -> error:collect([ErrFun(D1, D2) || {D1, D2} <- Duplicates]);
                true    -> 
                    ImportMap = maps:from_list([{Alias, Term} || {alias, _, Alias, Term} <- Aliases]),
                    case tagger:tag(AST, ImportMap) of
                        {error, Errs}       -> {error, Errs};
                        {ok, {_, Tagged}}   -> {ok, {Path, Tagged}}
                    end
            end
    end.


-ifdef(TEST).

traverse_test_() -> 
	{timeout,3600, [?_assertMatch({ok, _}, parse(paths, ["test/dir"]))]}.


-endif.
