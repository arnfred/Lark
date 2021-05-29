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
    InputPaths = case maps:get(include_kind_libraries, Options, true) of
                     true   -> [?KIND_SRC_LIB | GivenPaths];
                     false  -> GivenPaths
                 end,

    Tag = fun() -> atom_to_list(symbol:id(no_file)) end,
    UnnamedInlineTexts = [{Tag(), Text} || {text, Text} <- Inputs],
    NamedInlineTexts = [{T, Text} || {text, T, Text} <- Inputs],
    InlineTexts = UnnamedInlineTexts ++ NamedInlineTexts,

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
                                    {DepList, TaggedMods} = lists:unzip(Formatted),
                                    case topological_sort(lists:flatten(DepList)) of
                                        {error, Errs}   -> {error, Errs};
                                        {ok, Order}     ->
                                            ReOrdered = reorder_modules(Order, TaggedMods),
                                            NoEmptyMods = [M || M <- ReOrdered, not(module:empty(M))],
                                            {ok, NoEmptyMods}
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
        _         -> error:format({malformed_path, FileName}, {parser})
    end.

traverse(dir, FileName) -> 
    case file:list_dir(FileName) of
        {error, Err}     -> error:format({error_listing_dir_files, FileName, Err}, {parser});
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

load_source({source, FileName}) ->
    case file:read_file(FileName) of
        {error, Err}    -> error:format({malformed_source_file, Err, FileName}, {parser, FileName});
        {ok, Data}      -> {ok, {FileName, unicode:characters_to_list(Data, utf8)}}
    end.

to_ast(FileName, Text) ->
    case lexer:string(Text) of
        {error, Error}          -> error:format({lexer_error, Error},{parser, FileName});
        {error, Error1, Error2} -> error:format({lexer_error, Error1, Error2},{parser, FileName});
        {ok, Tokens, _}         ->
            case syntax:parse(Tokens) of
                {error, Error}  -> error:format({parser_error, Error}, {parser, FileName});
                {ok, Parsed}    ->
                    case preener:preen(FileName, Parsed) of
                        {error, Errs}   -> {error, Errs};
                        {ok, Preened}   -> {ok, {FileName, Preened}}
                    end
            end
    end.

import_and_tag({module, ModuleCtx, ModulePath, ModuleImports, Exports, DefMap}, ModuleMap, Options) ->

    % Make sure to import prelude only if we're both importing prelude and all
    % kind libraries in general
    ImportPrelude = maps:get(import_prelude, Options, true) andalso maps:get(include_kind_libraries, Options, true),

    % Make sure not to import prelude if this is the prelude
    PreludeImport = case {ImportPrelude, ModulePath} of
                         {false, _}                     -> [];
                         {_, [source, lib, prelude]}    -> []; % source file module
                         {_, [source, lib, prelude, _]} -> []; % source file sub modules
                         {_, [kind, prelude]}           -> []; % declared module
                         {_, [kind, prelude, _]}        -> []; % declared sub modules
                         {_, _}                         -> [{import, ModuleCtx, [kind, prelude, '_']}]
                     end,
    WithPreludeMod = {module, ModuleCtx, ModulePath, PreludeImport ++ ModuleImports, Exports, DefMap},

    case import:import(WithPreludeMod, ModuleMap, Options) of
        {error, Errs}                   -> {error, Errs};
        {ok, {ImportMap, Dependencies}} ->
            ModuleWithImports = {module, ModuleCtx, ModulePath, ImportMap, Exports, DefMap},
            Module = case module:is_submodule(ModuleWithImports) of
                         true   -> remove_imported_defs(ModuleWithImports);
                         false  -> ModuleWithImports
                     end,
            case tagger:tag(Module) of
                {error, Errs}       -> {error, Errs};
                {ok, {_, Tagged}}   -> 
                    {ok, {Dependencies, normalize_applications(Tagged)}}
            end
    end.

% A qualified symbol is due to be either the expression part of an application,
% or itself an application with zero arguments. This function straightens out
% that ambiguity
normalize_applications(Module) ->
    Pre = fun(_, _, {application, Ctx, {qualified_symbol, _, ModulePath, Name}, Args}) ->
                  {ok, {qualified_application, Ctx, ModulePath, Name, Args}};
             (_, _, {application, Ctx, {beam_symbol, _, ModulePath, Name}, Args}) ->
                  {ok, {beam_application, Ctx, ModulePath, Name, Args}};
             (_, _, {beam_symbol, Ctx, ModulePath, Name}) ->
                  {ok, {beam_application, Ctx, ModulePath, Name, []}};
             (_, _, _) -> ok end,
    Post = fun(_, _, _) -> ok end,
    {ok, {_, Normalized}} = ast:traverse(Pre, Post, Module),
    Normalized.



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
reorder_modules(PathOrder, Modules) ->
    ModMap = maps:from_list([{P, M} || {module, _, P, _, _, _} = M <- Modules]),
    Roots = [Mod || {Path, Mod} <- maps:to_list(ModMap), not(lists:member(Path, PathOrder))],
    [maps:get(Path, ModMap) || Path <- PathOrder] ++ Roots.

remove_imported_defs({module, Ctx, Path, Imports, Exports, Defs}) ->
    NewDefs = maps:filter(fun(K, _V) -> not(maps:is_key(K, Imports)) end, Defs),
    NewExports = maps:filter(fun(K, _V) -> not(maps:is_key(K, Imports)) end, Exports),
    {module, Ctx, Path, Imports, NewExports, NewDefs}.


-ifdef(TEST).

traverse_test_() -> 
    ?test({ok, _}, parse([{path, "test/dir"}], #{import_prelude => true})).

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
