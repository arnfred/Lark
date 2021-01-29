-module(parser).
-export([parse/1, parse/2, to_ast/2]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

% Kind library files are added to priv according to Erlang tradition [2]
% The use of code:priv_dir came from the answer of the following stackoverflow question: [1]
% [1]: https://stackoverflow.com/questions/63722670/rebar3-how-do-i-refer-to-source-artifacts-of-a-library-from-erlang
% [2]: https://stackoverflow.com/questions/36231976/how-can-i-include-a-mustache-file-in-erlang-release
-define(KIND_SRC_LIB, code:priv_dir(kind) ++ "/lib").

parse(Inputs) -> parse(Inputs, #{}).
parse(Inputs, Options) ->

    % Add in the kind libraries as input paths unless options says no
    GivenPaths = [Path || {path, Path} <- Inputs],
    InputPaths = case maps:get(import_kind_libraries, Options, true) of
                     true   -> [?KIND_SRC_LIB | GivenPaths];
                     false  -> GivenPaths
                 end,

    Tag = fun() -> atom_to_list(symbol:id(no_file)) end,
    InlineTexts = [{Tag(), Text} || {text, Text} <- Inputs],

    case error:collect([load(P) || P <- InputPaths]) of
        {error, Errs}   -> {error, Errs};
        {ok, LoadedTexts}  ->
            Texts = InlineTexts ++ lists:flatten(LoadedTexts),
            case error:collect([to_ast(Id, T) || {Id, T} <- Texts]) of
                {error, Errs}   -> {error, Errs};
                {ok, Sources}      ->
                    case module:parse(Sources) of
                        {error, Errs}   -> {error, Errs};
                        {ok, ModuleMap} ->
                            F = fun(Module) -> import_and_tag(Module, ModuleMap, Options) end,
                            case error:collect([F(M) || M <- maps:values(ModuleMap)]) of
                                {error, Errs}   -> {error, Errs};
                                {ok, Formatted} ->
                                    {DepList, TaggedASTs} = lists:unzip(Formatted),
                                    case topological_sort(lists:flatten(DepList)) of
                                        {error, Errs}   -> {error, Errs};
                                        {ok, Order}     -> 
                                            {Paths, _} = lists:unzip(Sources),
                                            TaggedSources = lists:zip(Paths, TaggedASTs),
                                            {ok, reorder_asts(Order, TaggedSources)}
                                    end
                            end
                    end
            end
    end.

load(FileName) ->
    case traverse(FileName) of
        {error, Errs}   -> {error, Errs};
        {ok, Files}     -> error:collect([load_source(F) || F <- Files])
    end.

traverse(FileName) ->
    case {filelib:is_dir(FileName), filelib:is_file(FileName)} of
        {true, _} -> traverse(dir, FileName);
        {_, true} -> traverse(file, FileName);
        _         -> error:format({malformed_path, FileName}, {sourcemap})
    end.

traverse(dir, FileName) -> 
    case file:list_dir(FileName) of
        {error, Err}     -> error:format({error_listing_dir_files, FileName, Err}, {sourcemap});
        {ok, List}       -> 
            case error:collect([traverse(filename:absname_join(FileName, File)) || File <- List]) of
                {error, Errs}   -> {error, Errs};
                {ok, Files}     -> {ok, lists:flatten(Files)}
            end
    end;

traverse(file, FileName) ->
    case filename:extension(FileName) of
        ".kind" ++ _    -> {ok, {source, FileName}};
        _               -> []
    end.

types_pre(top_level, _, {type_def, _, Name, _} = Term) -> 
    {ok, ast:tag(parent, Term, Name)};
types_pre(top_level, _, _)  -> skip;
types_pre(_, _, Term) -> {ok, ast:tag(parent, Term)}.

types_post(expr, _, {pair, _, {symbol, _, type, Name}, _} = Term) -> 
    Parent = ast:get_tag(parent, Term),
    {ok, {Parent, Name}, Term};
types_post(expr, _, {pair, _, {symbol, _, operator, Name}, _} = Term) -> 
    Parent = ast:get_tag(parent, Term),
    {ok, {Parent, Name}, Term};
types_post(expr, _, {symbol, _, type, Name} = Term) -> 
    Parent = ast:get_tag(parent, Term),
    % Don't include recursive types
    case Parent == Name of
        true    -> {ok, Term};
        false   -> {ok, {Parent, Name}, Term}
    end;
types_post(_, _, _) -> ok.


types(FileName, AST) ->
    case ast:traverse(fun types_pre/3, fun types_post/3, AST) of
        {error, Errs}   -> 
            {error, Errs};
        {ok, {Env, _}}  -> 
            GetKey = fun({K, _}) -> K end,
            GetVal = fun({_, V}) -> V end,
            Types = maps:from_list(utils:group_by(GetKey, GetVal, maps:keys(Env))),
            {ok, {FileName, Types}}
    end.


type_modules({module, Ctx, Name, Exports}, Types) ->
    F = fun(Type, Members) ->
                TypeExports = maps:from_list([{T, T} || T <- Members]),
                Path = Name ++ [Type],
                {module, Ctx, Path, TypeExports}
        end,
    [F(T, Members) || {T, Members} <- maps:to_list(Types), maps:is_key(T, Exports)].

load_source({source, FileName}) ->
    case file:read_file(FileName) of
        {error, Err}    -> error:format({malformed_source_file, Err, FileName}, {sourcemap, FileName});
        {ok, Data}      -> {FileName, unicode:characters_to_list(Data, utf8)}
    end.

to_ast(FileName, Text) ->
    case lexer:string(Text) of
        {error, Error}          -> error:format({lexer_error, Error},{sourcemap, FileName});
        {error, Error1, Error2} -> error:format({lexer_error, Error1, Error2},{sourcemap, FileName});
        {ok, Tokens, _}         ->
            case syntax:parse(Tokens) of
                {error, Error}  -> error:format({parser_error, Error}, {sourcemap, FileName});
                {ok, Parsed}    ->
                    case preener:preen(FileName, Parsed) of
                        {error, Errs}   -> {error, Errs};
                        {ok, Preened}   -> {ok, {FileName, Preened}}
                    end
            end
    end.

import_and_tag({module, ModuleCtx, ModulePath, Imports, Exports, DefMap, Types},
               ModuleMap,
               Options) ->

    % Make sure to import prelude only if we're both importing prelude and all
    % kind libraries in general
    ImportPrelude = maps:get(import_prelude, Options, true) andalso maps:get(import_kind_libraries, Options, true),

	% make sure to include if we're running imports in sandboxed mode where
	% only some erlang functions are allowed
	Sandboxed = maps:get(sandboxed, Options, false),

    % Make sure not to import prelude if this is the prelude
    PreludePath = [{symbol, #{}, variable, kind}, {symbol, #{}, variable, prelude}, {symbol, #{}, variable, '_'}],
    PreludeImports = case {ImportPrelude, ModulePath} of
                         {false, _}             -> [];
                         {_, [kind, prelude]}   -> [];
                         {_, _}                 -> import:import({import, #{}, PreludePath}, ModuleMap, Types, Sandboxed)
                     end,

    F = fun(Import) -> import:import(Import, ModuleMap, Types, Sandboxed) end,
    case error:collect([F(I) || I <- Imports] ++ [PreludeImports]) of
        {error, Errs} -> {error, Errs};
        {ok, ImportList} ->
            Imports = lists:flatten(ImportList),
            Aliases = [A || {alias, _, _, _} = A <- Imports],
            Dependencies = [Dep || {dependency, _, _} = Dep <- Imports],
            ErrFun = fun({alias, ErrorCtx1, Alias, _},
                         {alias, ErrorCtx2, _, _}) -> error:format({duplicate_import, Alias},
                                                                   {import, ErrorCtx1, ErrorCtx2}) end,
            case utils:duplicates(Aliases, fun({alias, _, A, _}) -> A end) of
                []          -> ImportMap = maps:from_list([{Alias, Term} || {alias, _, Alias, Term} <- Aliases]),
                               case tagger:tag(DefMap, ImportMap) of
                                   {error, Errs}       -> {error, Errs};
                                   {ok, {_, Tagged}}   -> {ok, {Dependencies, Tagged}}
                               end;
                Duplicates  -> error:collect([ErrFun(D1, D2) || {D1, D2} <- Duplicates])
            end
    end.

% Adapted from http://rosettacode.org/wiki/Topological_sort#Erlang
% More info: https://en.wikipedia.org/wiki/Topological_sorting
topological_sort(Dependencies) ->
    G = digraph:new(),
    AddEdge = fun({dependency, _, From, To}) ->
                      digraph:add_vertex(G, From), % noop if already added
                      digraph:add_vertex(G, To), % noop if already added
                      digraph:add_edge(G, To, From)
              end,
    lists:foreach(AddEdge, Dependencies),
    case digraph_utils:topsort(G) of
        false   -> cycles(G, digraph:vertices(G), []);
        Sorted  -> {ok, Sorted}
    end.

cycles(_, [], CycleErrs) -> error:collect(CycleErrs);
cycles(G, [Vertex | Rest], CycleErrs) ->
    case digraph:get_short_cycle(G, Vertex) of
        false   -> cycles(G, Rest, CycleErrs);
        Cycle   -> 
            CycleMap = maps:from_list(lists:zip(Cycle, Cycle)),
            RemaindingVertices = [V || V <- Rest, not(maps:is_key(V, CycleMap))],
            Err = error:format({cyclical_dependencies, Cycle}, {parser}),
            cycles(G, RemaindingVertices, [Err | CycleErrs])
    end.


% Any source without any dependencies isn't included in the module order
% It needs to get added to the front of the path order
reorder_asts(FileNameOrder, TaggedSources) ->
    SourceMap = maps:from_list(TaggedSources),
    Roots = [maps:get(FileName, SourceMap) || {FileName, _} <- TaggedSources, not(lists:member(FileName, FileNameOrder))],
    Roots ++ [maps:get(FileName, SourceMap) || FileName <- FileNameOrder].

-ifdef(TEST).

traverse_test_() -> 
    {timeout,3600, [?_assertMatch({ok, _}, parse([{path, "test/dir"}], #{import_prelude => false}))]}.

topo_sort_cycle_test_() -> Actual = topological_sort([{dependency, #{}, 'A', 'B'},
                                                      {dependency, #{}, 'B', 'C'},
                                                      {dependency, #{}, 'B', 'D'},
                                                      {dependency, #{}, 'C', 'A'}]),
                          ?testError({cyclical_dependencies, ['C', 'B', 'A', 'C']}, Actual).

topo_sort_test_() -> Actual = topological_sort([{dependency, #{}, 'A', 'B'},
                                                {dependency, #{}, 'A', 'D'},
                                                {dependency, #{}, 'B', 'C'},
                                                {dependency, #{}, 'B', 'E'},
                                                {dependency, #{}, 'C', 'D'},
                                                {dependency, #{}, 'D', 'E'},
                                                {dependency, #{}, 'Q', 'B'}]),
                     ?test({ok, ['E', 'D', 'C', 'B', 'A', 'Q']}, Actual).


-endif.
