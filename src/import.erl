-module(import).
-export([import/3, import/4, is_whitelisted/2, is_local/2, is_wildcard/1]).

-define(DEFAULT_SANDBOXED, false).

import(Import, Module, ModuleMap) -> import(Import, Module, ModuleMap, #{sandboxed => ?DEFAULT_SANDBOXED}).
import(Import, {module, _, ModulePath, _, _, _} = Module, ModuleMap, Options) ->
    case import_prime(Import, Module, ModuleMap, Options) of
        {error, Errs}       -> {error, Errs};
        {ok, Imports}       -> {ok, [I || I <- Imports, not(is_circular(ModulePath, I))]}
    end.

is_circular(ModulePath, {alias, _, _, {qualified_symbol, _, ModulePath, _}}) -> true;
is_circular(_, _) -> false.

import_prime({import, _, ImportPath} = Import, Module, ModuleMap, Options) ->
    case lists:reverse(ImportPath) of
        []                              -> error:format({empty_import}, {import, Import});

        % import test/{Test: Blip}      -> [{Blip, test/Test}]
        % import test/{Test, blah}      -> [{Test, test/Test},
        %                                   {blah, test/blah}]
        [Dict | Tail] when is_map(Dict) ->
            Rest = lists:reverse(Tail),
            F = fun('_', Alias) -> error:format({import_underscore_for_name, Alias}, {import, Import});
                   (Name, '_') -> error:format({import_underscore_for_alias, Name}, {import, Import});
                   (Name, Alias) -> error:collect(import_def(Name, Alias, Rest, Module, ModuleMap, Import, Options))
                end,
            error:map(error:collect([F(Name, Alias) || {Name, Alias} <- maps:to_list(Dict)]), fun lists:flatten/1);

        % import lists                  -> [{lists/reverse, lists/reverse},
        %                                   {lists/split, lists/split},
        %                                   ...]
        [_]                             -> import_module(Import, Module, ModuleMap, Options);

        % import test/Test              -> [{Test, test/Test},
        %                                   {Test/T, test/Test/T},
        %                                   ...]
        % import kind/prelude           -> [{kind/prelude/Boolean, kind/prelude/Boolean},
        %                                   {kind/prelude/True, kind/prelude/True},
        %                                   ...]
        % import test/_                 -> [{Test, test/Test},
        %                                   {blah, test/blah}
        %                                   ...]
        [Name | T]                      ->
            case is_kind_module(ImportPath, ModuleMap) of
                true    -> import_module(Import, Module, ModuleMap, Options);
                false   -> error:collect(import_def(Name, Name, lists:reverse(T), Module, ModuleMap, Import, Options))
            end;

        _                             ->
            error:format({unrecognized_import, ImportPath}, {import, Import})
    end.

import_module({import, ImportCtx, ImportPath} = ImportTerm, Module, ModuleMap, Options) ->

    % When importing `module/T`, we import both all type members of `T` (`T/A`,
    % `T/B`, etc) and also the root type `T` itself
    {RootPath, [RootName]} = lists:split(length(ImportPath) - 1, ImportPath),
    RootImport = case (length(RootPath) > 0) andalso is_kind_module(RootPath, ModuleMap) of
                     true  ->
                         case symbol(RootPath, RootName, ModuleMap, ImportTerm) of
                             {error, Errs_}         -> [{error, Errs_}];
                             {ok, {Dep, Symbol}}    -> [{ok, {alias, ImportCtx, RootName, Symbol}},
                                                        {ok, Dep}]
                         end;
                     false -> []
                 end,

    case error:collect(RootImport) of
        {error, Errs}           -> {error, Errs};
        {ok, Root}              ->
            % We call `import_def` to create aliases for all members of `ModulePath`
            case error:collect(import_def('_', '_', ImportPath, Module, ModuleMap, ImportTerm, Options)) of
                {error, Errs}  -> {error, Errs};
                {ok, Imports}  -> Tag = fun(Alias) -> symbol:tag([RootName, Alias]) end,
                                  Aliases = [{alias, Ctx, Tag(Alias), Term} || {alias, Ctx, Alias, Term} <- Imports],
                                  Other = [Imp || Imp <- Imports, not(element(1, Imp) =:= alias)],
                                  {ok, Root ++ Aliases ++ Other}
            end
    end.

import_def(Name, Alias, ImportPath, {module, _, ModulePath, Imports, _, _}, ModuleMap, Term, Options) ->
    % Check if ImportPath a local type import. If it is, import it with a fully
    % qualified path. In case it isn't, check if it's a transitive local type import
    LocalImportModule = module:beam_name(ModulePath ++ ImportPath),
    case maps:is_key(LocalImportModule, ModuleMap) of
        true    -> 
            Mod = maps:get(LocalImportModule, ModuleMap),
            % We don't return local source as dependency to avoid triggering
            % the check for cyclical dependencies
            kind_source_aliases(Mod, Name, Alias, ModuleMap, Term);
        false   -> 
            % Check if ImportPath is a transitive local type import, e.g. an
            % extension of an existing import, for example by importing the
            % members of a type which is already in scope. If it is, import it
            % with a fully qualified path. In case it isn't, try to import it
            % from source or beam file.
            TransitiveModulePaths = [ModName || I <- Imports,
                                                not(I =:= Term),
                                                ModName <- local_mod_path(I, ImportPath, ModuleMap)],
            case TransitiveModulePaths of
                [TransitiveImportModulePath]        ->
                    % With exactly one transitive match, we can go ahead and import it
                    Mod = maps:get(module:beam_name(TransitiveImportModulePath), ModuleMap),
                    kind_source_aliases(Mod, Name, Alias, ModuleMap, Term);
                []                              -> 
                    % With zero transitive matches, we assume the import isn't of a local type
                    ImportName = module:beam_name(ImportPath),
                    case maps:get(ImportName, ModuleMap, undefined) of
                        % 2a. check if it's a compiled kind module and look up if it is
                        undefined    -> beam_function(ImportPath, Name, Alias, Term, Options);

                        % 2b. check if it's a source module and import if it is
                        ImportModule -> kind_source_function(ImportModule, Name, Alias, ModuleMap, Term)
                    end;
                MultipleTransitivePaths when length(MultipleTransitivePaths) > 1   ->
                    % With more than one candidate we throw an error since the import is ambigious
                    ModuleNames = [module:kind_name(P) || P <- MultipleTransitivePaths],
                    [error:format({multiple_transitive_import_candidates, ModuleNames}, {import, Term})]
            end
    end.

beam_function(ModulePath, Name, Alias, Term, Options) ->
    Module = module:beam_name(ModulePath),
    ModuleName = module:kind_name(ModulePath),
    case code:is_loaded(Module) of
        {file, _}   -> loaded_module_function(ModulePath, Name, Alias, Term, Options);
        false       -> case code:which(Module) of
                           non_existing -> [error:format({nonexistent_module, ModuleName}, {import, Term})];
                           _            ->
                               case code:ensure_loaded(Module) of
                                   {error, Err} ->
                                       [error:format({error_loading_module, Module, Err}, {import, Term})];
                                   _            ->
                                       loaded_module_function(ModulePath, Name, Alias, Term, Options)
                               end
                       end
    end.

loaded_module_function(ModulePath, '_', _, Term, Options) ->
    Module = module:beam_name(ModulePath),
    ExportList = erlang:apply(Module, module_info, [exports]),
    Exports = lists:delete(module_info, utils:unique([Name || {Name, _Arity} <- ExportList])),
    lists:flatten([loaded_module_function(ModulePath, Name, Name, Term, Options)
                   || Name <- Exports]);

loaded_module_function(ModulePath, Name, Alias, Term, Options) ->
    ErrorCtx = element(2, Term),
    Module = module:beam_name(ModulePath),
    ImportPath = ModulePath ++ [Name],
    ImportName = module:kind_name(ImportPath),
    Exports = erlang:get_module_info(Module, exports),
    TypeAliases = beam_subtypes(ImportPath, Alias, Term, Options),
    case maps:get(sandboxed, Options, ?DEFAULT_SANDBOXED) andalso not(is_whitelisted(Module, Name)) of
        true    -> [error:format({function_not_whitelisted, Module, Name}, {import, Term})];
        false   ->
            case lists:filter(fun({Export, _}) -> Export =:= Name end, Exports) of
                []      -> [error:format({nonexistent_export, beam, ImportName}, {import, Term})];
                _       -> [{ok, {alias, ErrorCtx, Alias,
                                  {qualified_symbol, #{import => Term}, ModulePath, Name}}} | TypeAliases]
            end
    end.

% This is a whitelist of functions without side-effects. It serves two
% functions:
%
% - To expose the compiler online, I need to know what erlang modules are
%   safe to call
% - When I try to establish the domain of a function applications of literals,
%   I check if a function is whitelisted before I call it to make sure to not
%   make destructive changes while typechecking.
%
% It's not impossible that I've overlooked something, but if you're here
% looking for a way to compromise this project, then maybe just submit a PR
% with your findings instead?
is_whitelisted(Module, Name) ->
    % Whitelist is written in the format 'module' => ['exception'] Modules not
    % on the list are not whitelisted. Any members in list of exceptions is not
    % whitelisted either
    Whitelist = #{'array' => [],
                  'base64' => [],
                  'binary' => [],
                  'calendar' => [],
                  'dict' => [],
                  'digraph' => [],
                  'digraph_utils' => [],
                  'erl_anno' => [],
                  'io_li' => [],
                  'lists' => [],
                  'maps' => [],
                  'math' => [],
                  'orddict' => [],
                  'ordsets' => [],
                  'proplists' => [],
                  'queue' => [],
                  'rand' => [],
                  'random' => [],
                  're' => [],
                  'sets' => [],
                  'sofs' => [],
                  'string' => [],
                  'unicode' => [],
                  'uri_string' => [],
                  'atomics' => [],
                  'counters' => [],
                  'erlang' => [apply, delete_module, put, erase, check_old_code,
                               check_process_cod, disconnect_node, error, exit,
                               halt, load_module, load_nif, open_port, port_call,
                               port_close, port_command, port_connect, port_control,
                               purge_module, put, self],
                  'persistent_term' => [],
                  'zlib' => []},
    maps:is_key(Module, Whitelist) andalso not(lists:member(Name, maps:get(Module, Whitelist))).


beam_subtypes(ImportPath, Alias, Term, Options) ->
    Aliases = beam_function(ImportPath, '_', '_', Term, Options),
    Ctx = element(2, Term),
    [{ok, {alias, Ctx, symbol:tag([Alias, T]),
           {qualified_symbol, #{import => Term}, ImportPath, T}}}
     || {ok, {alias, _, T, _}} <- Aliases].

kind_source_function({module, _, ModulePath, _, _, _} = Module, Name, Alias, ModuleMap, Term) ->
    [{ok, {dependency, Term, ModulePath}} | kind_source_aliases(Module, Name, Alias, ModuleMap, Term)].

kind_source_aliases({module, _, _, _, Exports, _} = Module, '_', _, ModuleMap, Term) ->
    lists:flatten([kind_source_aliases(Module, Name, Name, ModuleMap, Term)
                   || Name <- maps:keys(Exports)]);

kind_source_aliases({module, _, ModulePath, _, Exports, _}, Name, Alias, ModuleMap, Term) ->
    Ctx = element(2, Term),
    ImportPath = ModulePath ++ [Name],
    ImportName = module:kind_name(ImportPath),
    case maps:is_key(Name, Exports) of
        false   -> [error:format({nonexistent_export, source, ImportName}, {import, Term})];
        true    -> {export, _, ExportPath, _} = maps:get(Name, Exports),
                   Path = ModulePath ++ lists:droplast(ExportPath),
                   case symbol(Path, Name, ModuleMap, Term) of
                       {error, Errs}        -> [{error, Errs}];
                       {ok, {Dep, Symbol}}  ->
                           {dependency, _, DepPath} = Dep,
                           TypeAliases = kind_subtypes(DepPath, Name, Alias, ModuleMap, Term),
                           [{ok, {alias, Ctx, Alias, Symbol}} | TypeAliases]
                   end
    end.

kind_subtypes(ModulePath, Name, Alias, ModuleMap, Term) ->
    ImportPath = ModulePath ++ [Name],
    ImportBeamName = module:beam_name(ImportPath),
    Ctx = element(2, Term),
    case maps:get(ImportBeamName, ModuleMap, undefined) of
        undefined                       -> [];
        {module, _, _, _, Exports, _}   -> 
            [error:map(symbol(ImportPath, T, ModuleMap, Term),
                       fun({_Dep, Symbol}) -> {alias, Ctx, symbol:tag([Alias, T]), Symbol} end) || T <- maps:keys(Exports)]
    end.

is_kind_module(Path, ModuleMap) ->
    ModuleName = module:beam_name(Path),
    maps:is_key(ModuleName, ModuleMap).

local_mod_path({import, _, Symbols}, LocalImportPath, ModuleMap) -> 
    ImportPaths = import_paths(Symbols),
    LocalPath = case lists:reverse(LocalImportPath) of
        ['_' | _]   -> lists:droplast(LocalImportPath);
        _           -> LocalImportPath
                end,

    [Path || ImportP <- ImportPaths,
             {path, Path} <- lists:flatten(local_mod_path(ImportP, LocalPath, [])),
             Name <- [module:beam_name(Path)],
             maps:is_key(Name, ModuleMap)];

local_mod_path([], _, _) -> [];
local_mod_path(['_'], LocalPath, Prefix) -> [{path, lists:reverse(Prefix) ++ LocalPath}];
local_mod_path([H | ImportPath], [H | LocalPath], Prefix) -> local_mod_path_end(ImportPath, LocalPath, [H | Prefix]);
local_mod_path([H | ImportPath], LocalPath, Prefix) -> local_mod_path(ImportPath, LocalPath, [H | Prefix]).

local_mod_path_end([], LocalPath, Prefix) -> [{path, lists:reverse(Prefix) ++ LocalPath}];
local_mod_path_end(['_'], LocalPath, Prefix) -> [{path, lists:reverse(Prefix) ++ LocalPath}];
local_mod_path_end([H | ImportPath], [H | LocalPath], Prefix) -> local_mod_path_end(ImportPath, LocalPath, [H | Prefix]);
local_mod_path_end(_, _, _) -> [].

import_paths([]) -> [];
import_paths(Symbols) -> import_paths(Symbols, [[]]).
import_paths([], Paths) -> [lists:reverse(P) || P <- Paths];
import_paths([Dict | Rest], Paths) when is_map(Dict) ->
    NewPaths = [[Name | Path] || Path <- Paths, Name <- maps:keys(Dict)],
    import_paths(Rest, NewPaths);
import_paths([P | Rest], Paths) -> import_paths(Rest, [[P | Path] || Path <- Paths]).

is_local({import, _, [P | _] = Path}, Defs) when length(Path) < 3 -> maps:is_key(P, Defs);
is_local({import, _, _}, _) -> false.

is_wildcard({import, _, Path}) -> lists:last(Path) =:= '_'.

symbol(Path, Name, ModuleMap, Term) ->
    KindName = module:kind_name(Path),
    case maps:get(module:beam_name(Path), ModuleMap, undefined) of
        undefined                     -> error:format({nonexistent_module, source, KindName},
                                                      {import, Term});
        {module, _, _, _, _, Defs}    ->
            case maps:get(Name, Defs, undefined) of
                undefined                   -> error:format({nonexistent_export, source, KindName, Name},
                                                            {import, Term});
                {link, _, Symbol}           -> {qualified_symbol, Ctx, LinkPath, SymbolName} = Symbol,
                                               Dep = {dependency, Term, LinkPath},
                                               NewCtx = maps:put(import, Term, Ctx),
                                               {ok, {Dep, {qualified_symbol, NewCtx, LinkPath, SymbolName}}};
                _                           -> Symbol = {qualified_symbol, #{import => Term}, Path, Name},
                                               Dep = {dependency, Term, Path},
                                               {ok, {Dep, Symbol}}
            end
    end.


