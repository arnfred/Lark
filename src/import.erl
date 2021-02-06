-module(import).
-export([import/3, import/4, is_whitelisted/2]).

-include_lib("eunit/include/eunit.hrl").
import(Import, Module, ModuleMap) -> import(Import, Module, ModuleMap, #{sandboxed => false}).
import({import, _, ImportPath} = Import, Module, ModuleMap, Options) ->
    case lists:reverse(ImportPath) of
        []                              -> error:format({empty_import}, {import, Import});

        % import lists                  -> [{lists/reverse, lists/reverse},
        %                                   {lists/split, lists/split},
        %                                   ...]
        [{symbol, _, _, _}]             -> import_module(Import, Module, ModuleMap, Options);

        % import test/Test              -> [{Test, test/Test},
        %                                   {Test/T, test/Test/T},
        %                                   ...]
        % import kind/prelude           -> [{kind/prelude/Boolean, kind/prelude/Boolean},
        %                                   {kind/prelude/True, kind/prelude/True},
        %                                   ...]
        % import test/_                 -> [{Test, test/Test},
        %                                   {blah, test/blah}
        %                                   ...]
        [{symbol, _, _, Name} | T]      ->
            ModulePath = [P || {symbol, _, _, P} <- ImportPath],
            case is_kind_module(ModulePath, ModuleMap) of
                true    -> import_module(Import, Module, ModuleMap, Options);
                false   -> Rest = [S || {symbol, _, _, S} <- lists:reverse(T)],
                           error:collect(import_def(Name, Name, Rest, Module, ModuleMap, Import, Options))
            end;
        % import test/{Test: Blip}     -> [{Blip, test/Test}]
        % import test/{Test, blah}     -> [{Test, test/Test},
        %                                  {blah, test/blah}]
        [{dict, _, Elements} | Tail]    ->
            Rest = [S || {symbol, _, _, S} <- lists:reverse(Tail)],
            F = fun({pair, _, {symbol, _, _, '_'}, {symbol, _, _, Alias}} = Term) ->
                        error:format({import_underscore_for_name, Alias}, {import, Term});
                   ({pair, _, {symbol, _, _, Name}, {symbol, _, _, '_'}} = Term) ->
                        error:format({import_underscore_for_alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, T, Name}, {symbol, _, T, Alias}} = Term) ->
                        error:collect(import_def(Name, Alias, Rest, Module, ModuleMap, Term, Options));
                   ({pair, _, {symbol, _, type, Name}, {symbol, _, variable, Alias}} = Term) ->
                        error:format({import_def_alias_for_type, Alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, variable, Name}, {symbol, _, type, Alias}} = Term) ->
                        error:format({import_type_alias_for_def, Alias, Name}, {import, Term});
                   ({symbol, _, _, Name} = Term) ->
                        error:collect(import_def(Name, Name, Rest, Module, ModuleMap, Term, Options));
                   (Other) ->
                        error:format({unrecognized_dict_import, Other}, {import, Other})
                end,
            error:map(error:collect([F(Elem) || Elem <- Elements]), fun lists:flatten/1);

        _                             ->
            error:format({unrecognized_import, ImportPath}, {import, Import})
    end.

import_module({import, Ctx, ImportSymbols} = Term, Module, ModuleMap, Options) ->

    ImportPath = [P || {symbol, _, _, P} <- ImportSymbols],

    % When importing `module/T`, we import both all type members of `T` (`T/A`,
    % `T/B`, etc) and also the root type `T` itself
    {RootPath, [RootName]} = lists:split(length(ImportPath) - 1, ImportPath),
    RootImport = case (length(RootPath) > 0) andalso is_kind_module(RootPath, ModuleMap) of
                     true  -> RootSymbol = {qualified_symbol, #{import => Term}, RootPath, RootName},
                              [{alias, Ctx, RootName, RootSymbol},
                               {dependency, Ctx, RootPath}];
                     false -> []
                 end,

    % We call `import_def` to create aliases for all members of `ModulePath`
    case error:collect(import_def('_', '_', ImportPath, Module, ModuleMap, Term, Options)) of
        {error, Errs}   -> {error, Errs};
        {ok, Imports}   -> Aliases = [{alias, Ctx, symbol:tag([RootName, Alias]), Term} || 
                                      {alias, Ctx, Alias, Term} <- Imports],
                           Other = [Imp || Imp <- Imports, not(element(1, Imp) =:= alias)],
                           {ok, RootImport ++ Aliases ++ Other}
    end.

import_def(Name, Alias, ImportPath, {module, _, ModulePath, Imports, _, _, _}, ModuleMap, Term, Options) ->
    % Check if ImportPath a local type import. If it is, import it with a fully
    % qualified path. In case it isn't, check if it's a transitive local type import
    LocalImportModule = module:beam_name(ModulePath ++ ImportPath),
    case maps:is_key(LocalImportModule, ModuleMap) of
        true    -> 
            Mod = maps:get(LocalImportModule, ModuleMap),
            % We don't return local source as dependency to avoid triggering
            % the check for cyclical dependencies
            no_deps(kind_source_function(Mod, Name, Alias, ModuleMap, Term));
        false   -> 
            % Check if ImportPath is a transitive local type import. If it is,
            % import it with a fully qualified path. In case it isn't, try to
            % import it from source or beam file.
            TransitiveModulePaths = [ModName || I <- Imports,
                                                ModName <- local_mod_path(I, ImportPath, ModuleMap)],
            case TransitiveModulePaths of
                [TransitiveImportModulePath]        ->
                    % With exactly one transitive match, we can go ahead and import it
                    Mod = maps:get(module:beam_name(TransitiveImportModulePath), ModuleMap),
                    no_deps(kind_source_function(Mod, Name, Alias, ModuleMap, Term));
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
    case maps:get(sandboxed, Options) andalso not(is_whitelisted(Module, Name)) of
        true    -> [error:format({function_not_whitelisted, Module, Name}, {import, Term})];
        false   ->
            case lists:filter(fun({Export, _}) -> Export =:= Name end, Exports) of
                []      -> [error:format({nonexistent_import, beam, ImportName}, {import, Term})];
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

kind_source_function({module, _, ModulePath, _, _, _, _} = Module, Name, Alias, ModuleMap, Term) ->
    [{ok, {dependency, Term, ModulePath}} | kind_source_aliases(Module, Name, Alias, ModuleMap, Term)].

kind_source_aliases({module, _, _, _, Exports, _, _} = Module, '_', _, ModuleMap, Term) ->
    lists:flatten([kind_source_aliases(Module, Name, Name, ModuleMap, Term)
                   || Name <- maps:keys(Exports)]);

kind_source_aliases({module, _, ModulePath, _, Exports, _, _}, Name, Alias, ModuleMap, Term) ->
    Ctx = element(2, Term),
    ImportPath = ModulePath ++ [Name],
    ImportName = module:kind_name(ImportPath),
    ImportBeamName = module:beam_name(ImportPath),
    TypeAliases = kind_subtypes(ImportPath, ImportBeamName, Alias, ModuleMap, Term),
    case maps:is_key(Name, Exports) of
        false   -> [error:format({nonexistent_import, source, ImportName}, {import, Term})];
        true    -> [{ok, {alias, Ctx, Alias,
                          {qualified_symbol, #{import => Term}, ModulePath, Name}}} | TypeAliases]
    end.

kind_subtypes(ImportPath, ImportBeamName, Alias, ModuleMap, Term) ->
    Ctx = element(2, Term),
    case maps:get(ImportBeamName, ModuleMap, undefined) of
        {module, _, _, _, Exports, _, _}    -> [{ok, {alias, Ctx, symbol:tag([Alias, T]),
                                                      {qualified_symbol, #{import => Term}, ImportPath, T}}}
                                                || T <- maps:keys(Exports)];
        undefined                           -> []
    end.

is_kind_module(Path, ModuleMap) ->
    ModuleName = module:beam_name(Path),
    maps:is_key(ModuleName, ModuleMap).

no_deps(Imports) -> 
    case error:collect(Imports) of
        {error, Errs}   -> [E || {error, _} = E <- Imports];
        {ok, Aliases}   -> [{ok, A} || A <- Aliases, not(element(1, A) =:= dependency)]
    end.

local_mod_path({import, _, Symbols}, LocalImportPath, ModuleMap) -> 
    ImportPath = [P || {symbol, _, _, P} <- Symbols],
    case lists:reverse(LocalImportPath) of
        ['_' | _]   ->
            LocalPath = lists:droplast(LocalImportPath),
            [Path || {path, Path} <- lists:flatten(local_mod_path(ImportPath, LocalPath, [])),
                     Name <- [module:beam_name(Path)],
                     maps:is_key(Name, ModuleMap)];
        _           ->
            LocalPath = LocalImportPath,
            [Path || {path, Path} <- lists:flatten(local_mod_path(ImportPath, LocalPath, [])),
                     Name <- [module:beam_name(Path)],
                     maps:is_key(Name, ModuleMap)]
    end;

local_mod_path([], _, _) -> [];
local_mod_path(['_'], LocalPath, Prefix) -> [{path, lists:reverse(Prefix) ++ LocalPath}];
local_mod_path([H | ImportPath], [H | LocalPath], Prefix) -> local_mod_path_end(ImportPath, LocalPath, [H | Prefix]);
local_mod_path([H | ImportPath], LocalPath, Prefix) -> local_mod_path(ImportPath, LocalPath, [H | Prefix]).

local_mod_path_end([], LocalPath, Prefix) -> [{path, lists:reverse(Prefix) ++ LocalPath}];
local_mod_path_end(['_'], LocalPath, Prefix) -> [{path, lists:reverse(Prefix) ++ LocalPath}];
local_mod_path_end([H | ImportPath], [H | LocalPath], Prefix) -> local_mod_path_end(ImportPath, LocalPath, [H | Prefix]);
local_mod_path_end(_, _, _) -> [].
    
