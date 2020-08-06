-module(parser).
-export([parse/1]).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

parse(Inputs) ->
    Paths = [Path || {path, Path} <- Inputs],
    Tag = fun() -> atom_to_list(symbol:id(no_file)) end,
    Texts = [{Tag(), Text} || {text, Text} <- Inputs],

    case error:collect([read(P) || P <- Paths]) of
        {error, Errs}   -> {error, Errs};
        {ok, ReadList}  ->
            Read = lists:flatten(ReadList),
            Sources = Texts ++ Read,
            case error:collect([to_ast(File, Source) || {File, Source} <- Sources]) of
                {error, Errs}   -> {error, Errs};
                {ok, ASTs}      -> format(ASTs)
            end
    end.

read(Path) ->
    case traverse(Path) of
        {error, Errs}   -> {error, Errs};
        {ok, Files}     ->
            case error:collect([load(F) || F <- Files]) of
                {error, Errs}   -> {error, Errs};
                {ok, Sources}    -> {ok, lists:zip([P || {source, P} <- Files], Sources)}
            end
    end.

format(Sources) ->
    case module:format(Sources) of
        {error, Errs}   -> {error, Errs};
        {ok, ParsedSources}    ->
            SourceDefMap = maps:from_list([{module:beam_name(Name), M} || {_, {ast, _, Modules, _, _}} <- ParsedSources,
                                                                          {module, _, Name, _} = M <- Modules]),
            CollectedTypes = [types(Path, AST) || {Path, AST} <- ParsedSources],

            case error:collect(CollectedTypes) of
                {error, Errs}   -> {error, Errs};
                {ok, Types}     ->
                    TypesByPath = maps:from_list([{Path, T} || {Path, _, T} <- Types]),
                    SourceTypeList = [{module:beam_name(Path), S} || {_, Modules, T}             <- Types,
                                                                     M                           <- Modules,
                                                                     {module, _, Path, _} = S    <- type_source(M, T)],
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
            {ok, {Path, Modules, Types}}
    end.


type_source({module, Ctx, Name, Exports}, Types) ->
    F = fun(Type, Members) ->
                TypeExports = maps:from_list([{T, T} || T <- Members]),
                {module, Ctx, Name ++ [Type], TypeExports}
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
    {timeout,3600, [?_assertMatch({ok, _}, parse([{path, "test/dir"}]))]}.


-endif.
