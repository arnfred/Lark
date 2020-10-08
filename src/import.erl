-module(import).
-export([import/3, import/4]).

import(Import, SourceMap, LocalTypes) -> import(Import, SourceMap, LocalTypes, false).
import({import, _, ImportPath} = Import, SourceMap, LocalTypes, Sandboxed) ->
    case lists:reverse(ImportPath) of
        []                             -> error:format({empty_import}, {import, Import});

        % import lists                 -> [{lists/reverse, lists/reverse},
        %                                  {lists/split, lists/split},
        %                                  ...]
        [{symbol, _, _, _}]         -> import_module(ImportPath, SourceMap, LocalTypes, Import, Sandboxed);

        % import test/Test             -> [{Test, test/Test},
        %                                  {Test/T, test/Test/T},
        %                                  ...]
        % import kind/prelude          -> [{kind/prelude/Boolean, kind/prelude/Boolean},
        %                                  {kind/prelude/True, kind/prelude/True},
        %                                  ...]
        % import test/_                -> [{Test, test/Test},
        %                                  {blah, test/blah}]
        [{symbol, _, _, Name} | T]     ->
            ModulePath = [P || {symbol, _, _, P} <- ImportPath],
            ModuleName = module:beam_name(ModulePath),
            case maps:is_key(ModuleName, SourceMap) of
                true    -> import_module(ImportPath, SourceMap, LocalTypes, Import, Sandboxed);
                false   -> Rest = lists:reverse(T),
                           error:collect(import_def(Name, Name, Rest, SourceMap, LocalTypes, Import, Sandboxed))
            end;
        % import test/{Test: Blip}     -> [{Blip, test/Test}]
        % import test/{Test, blah}     -> [{Test, test/Test},
        %                                  {blah, test/blah}]
        [{dict, _, Elements} | Tail]   ->
            Rest = lists:reverse(Tail),
            F = fun({pair, _, {symbol, _, _, '_'}, {symbol, _, _, Alias}} = Term) ->
                        error:format({import_underscore_for_name, Alias}, {import, Term});
                   ({pair, _, {symbol, _, _, Name}, {symbol, _, _, '_'}} = Term) ->
                        error:format({import_underscore_for_alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, T, Name}, {symbol, _, T, Alias}} = Term) ->
                        error:collect(import_def(Name, Alias, Rest, SourceMap, LocalTypes, Term, Sandboxed));
                   ({pair, _, {symbol, _, type, Name}, {symbol, _, variable, Alias}} = Term) ->
                        error:format({import_def_alias_for_type, Alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, variable, Name}, {symbol, _, type, Alias}} = Term) ->
                        error:format({import_type_alias_for_def, Alias, Name}, {import, Term});
                   ({symbol, _, _, Name} = Term) ->
                        error:collect(import_def(Name, Name, Rest, SourceMap, LocalTypes, Term, Sandboxed));
                   (Other) ->
                        error:format({unrecognized_dict_import, Other}, {import, Other})
                end,
            error:map(error:collect([F(Elem) || Elem <- Elements]), fun lists:flatten/1);

        _                             ->
            error:format({unrecognized_import, ImportPath}, {import, Import})
    end.

import_module(ImportPath, SourceMap, LocalTypes, ImportTerm, Sandboxed) ->

    % import kind/prelude           -> path relative to module name: `kind/prelude/Option`
    % import kind/prelude/Option    -> path relative to type: `Option/Nil`
    {Module, Type} = lists:splitwith(fun ({symbol, _, Kind, _}) -> Kind =:= variable end, ImportPath),
    ModulePath = case Type of
                     [] -> [P || {symbol, _, _, P} <- Module];
                     _  -> [T || {symbol, _, _, T} <- Type]
                 end,

    case error:collect(import_def('_', '_', ImportPath, SourceMap, LocalTypes, ImportTerm, Sandboxed)) of
        {error, Errs}   -> {error, Errs};
        {ok, Aliases}   -> {ok, [{alias, Ctx, ModulePath ++ [Alias], Term} || {alias, Ctx, Alias, Term} <- Aliases]}
    end.

import_def(Name, Alias, ImportPath, SourceMap, LocalTypes, Term, Sandboxed) ->
    case lists:reverse(ImportPath) of

        % 1. Check if it's a local type import and if it is, look it up in the typemap
        [{symbol, _, type, Parent}]             -> local_type(Parent, Name, Alias, LocalTypes, Term);

        % 2. Otherwise, import from source or beam file
        _                                       ->
            ModulePath = [P || {symbol, _, _, P} <- ImportPath],
            ModuleName = module:beam_name(ModulePath),
            case maps:get(ModuleName, SourceMap, undefined) of
                % 2a. check if it's a compiled kind module and look up if it is
                undefined    -> beam_function(ModulePath, Name, Alias, Term, Sandboxed);

                % 2b. check if it's a source module and import if it is
                Module       -> kind_source_function(Module, Name, Alias, SourceMap, Term)
            end
    end.

local_type(Parent, '_', _, LocalTypes, Term) ->
    Ctx = element(2, Term),
    case maps:get(Parent, LocalTypes, undefined) of
        undefined   -> [error:format({undefined_local_type, Parent, maps:keys(LocalTypes)},
                                     {import, Term})];
        SubTypes    -> [{ok, {alias, Ctx, T, {type, #{import => Ctx}, T, [Parent, T]}}} || T <- SubTypes]
    end;

local_type(Parent, Name, Alias, LocalTypes, Term) ->
    Ctx = element(2, Term),
    case maps:is_key(Parent, LocalTypes) of
        false   -> [error:format({undefined_local_type, Parent, maps:keys(LocalTypes)},
                                {import, Term})];
        true    ->
            case lists:member(Name, maps:get(Parent, LocalTypes)) of
                false   -> [error:format({undefined_local_type, Parent, Name, maps:keys(LocalTypes)},
                                        {import, Term})];
                true    -> [{ok, {alias, Ctx, Alias, {type, #{import => Ctx}, Alias, [Parent, Name]}}}]
            end
    end.


beam_function(ModulePath, Name, Alias, Term, Sandboxed) ->
    Module = module:beam_name(ModulePath),
    ModuleName = module:kind_name(ModulePath),
    ImportName = module:kind_name(ModulePath ++ [Name]),
    case code:is_loaded(Module) of
        {file, _}   -> loaded_module_function(ModulePath, Name, Alias, Term, Sandboxed);
        false       -> case code:which(Module) of
                           non_existing ->
                               case get_tag(Name) of
                                   qualified_type       -> [error:format({nonexistent_import, source, ImportName}, {import, Term})];
                                   qualified_variable   -> [error:format({nonexistent_module, ModuleName}, {import, Term})]
                               end;
                           _            ->
                               case code:ensure_loaded(Module) of
                                   {error, Err} ->
                                       [error:format({error_loading_module, Module, Err}, {import, Term})];
                                   _            ->
                                       loaded_module_function(ModulePath, Name, Alias, Term, Sandboxed)
                               end
                       end
    end.

loaded_module_function(ModulePath, '_', _, Term, Sandboxed) ->
    Module = module:beam_name(ModulePath),
    ExportList = erlang:apply(Module, module_info, [exports]),
    Exports = lists:delete(module_info, utils:unique([Name || {Name, _Arity} <- ExportList])),
    lists:flatten([loaded_module_function(ModulePath, Name, Name, Term, Sandboxed)
                   || Name <- Exports]);

loaded_module_function(ModulePath, Name, Alias, Term, Sandboxed) ->
    ErrorCtx = element(2, Term),
    Module = module:beam_name(ModulePath),
    ImportPath = ModulePath ++ [Name],
    ImportName = module:kind_name(ImportPath),
    Exports = erlang:apply(Module, module_info, [exports]),
    TypeAliases = beam_subtypes(get_tag(Name), ImportPath, Alias, Term, Sandboxed),
    case Sandboxed andalso not(is_whitelisted(Module, Name)) of
        true    -> [error:format({function_not_whitelisted, Module, Name}, {import, Term})];
        false   ->
            case lists:filter(fun({Export, _}) -> Export =:= Name end, Exports) of
                []      -> [error:format({nonexistent_import, beam, ImportName}, {import, Term})];
                _       -> [{ok, {alias, ErrorCtx, Alias,
                                  {get_tag(Name), #{import => Term}, ModulePath, Name}}} | TypeAliases]
            end
    end.

% To expose the compiler online, I maintain a whitelist of erlang modules that
% are safe to call. It's not impossible that I've overlooked something, but if
% you're here looking for a way to compromise this project, then maybe just
% submit a PR with your findings instead?
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
                  'timer' => [exit_after, kill_after, apply_after],
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


beam_subtypes(qualified_variable, _, _, _, _) -> [];
beam_subtypes(qualified_type, ImportPath, Alias, Term, Sandboxed) ->
    Ctx = element(2, Term),
    [{ok, {alias, Ctx, [Alias, T],
           {get_tag(T), #{import => Term}, ImportPath, T}}}
     || {ok, {alias, _, T, _}} <- beam_function(ImportPath, '_', '_', Term, Sandboxed)].

kind_source_function({module, _, ModulePath, _} = Module, Name, Alias, SourceMap, Term) ->
    [{dependency, Term, ModulePath} | kind_source_aliases(Module, Name, Alias, SourceMap, Term)].

kind_source_aliases({module, _, _, Exports} = Module, '_', _, SourceMap, Term) ->
    lists:flatten([kind_source_aliases(Module, Name, Name, SourceMap, Term)
                   || Name <- maps:keys(Exports)]);

kind_source_aliases({module, _, ModulePath, Exports}, Name, Alias, SourceMap, Term) ->
    Ctx = element(2, Term),
    ImportPath = ModulePath ++ [Name],
    ImportName = module:kind_name(ImportPath),
    ImportBeamName = module:beam_name(ImportPath),
    TypeAliases = kind_subtypes(get_tag(Name), ImportPath, ImportBeamName, Alias, SourceMap, Term),
    case maps:is_key(Name, Exports) of
        false   -> [error:format({nonexistent_import, source, ImportName}, {import, Term})];
        true    -> [{ok, {alias, Ctx, Alias,
                          {get_tag(Name), #{import => Term}, ModulePath, Name}}} | TypeAliases]
    end.

kind_subtypes(qualified_variable, _, _, _, _, _) -> [];
kind_subtypes(qualified_type, ImportPath, ImportBeamName, Alias, SourceMap, Term) ->
    Ctx = element(2, Term),
    case maps:get(ImportBeamName, SourceMap, undefined) of
        {module, _, _, Types} -> [{ok, {alias, Ctx, [Alias, T],
                                        {get_tag(T), #{import => Term}, ImportPath, T}}}
                                  || T <- maps:keys(Types)];
        undefined             -> []
    end.


% With exports coming directly from beam files we don't know from the context if they are a variable or a type
% To import them correctly we decide if they are a type or a variable based on whether the first letter is uppercase or lowercase.
% This isn't great, but here we are.
% This is going to suck when I try to implement inline functions and types...
get_tag(Name) ->
    First = string:substr(atom_to_list(Name), 1, 1),
    case string:lowercase(First) =:= First of
        true    -> qualified_variable;
        false   -> qualified_type
    end.
