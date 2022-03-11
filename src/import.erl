-module(import).
-export([import/3, import/2, is_whitelisted/2]).

-define(DEFAULT_SANDBOXED, false).
-include_lib("eunit/include/eunit.hrl").

import(Module, ModuleMap) -> import(Module, ModuleMap, #{}).
import({module, _, ModulePath, Imports, _, _}, ModuleMap, Options) ->
    FlatImports = lists:flatten([flatten(I) || I <- Imports]),
    Scope = scope(ModulePath, ModuleMap),
    case aliases(FlatImports, Scope, ModuleMap) of
        {error, Errs}       -> {error, Errs};
        {ok, Aliases}       ->
            case whitelisted(Aliases, ModuleMap, Options) of
                {error, Errs}       -> {error, Errs};
                {ok, _Whitelisted}  ->
                    Deps = utils:unique(lists:flatten([dep(ModulePath, A) || A <- Aliases,
                                                                             not(is_local(A, ModulePath))])),
                    AliasGroups = utils:group_by(fun({alias, _, Alias, _}) -> symbol:tag(Alias) end,
                                                 fun({alias, _, _, Term}) -> Term end,
                                                 Aliases),
                    NoCtx = fun(Term) -> setelement(2, Term, #{}) end,
                    ImportMap = maps:from_list([{K, utils:unique(G, NoCtx)} || {K, G} <- AliasGroups]),
                    {ok, {ImportMap, Deps}}
            end
    end.



flatten({import, _, ImportPath} = I)    -> [{Alias, Path, I} || {Alias, Path} <- flatten(ImportPath, [[]])].
flatten([D], Res) when is_map(D)        -> [{V, lists:reverse([K | R])} || R <- Res, {K, V} <- maps:to_list(D)];
flatten([E], Res)                       -> [{E, lists:reverse([E | R])} || R <- Res];
flatten([D | Rest], Res) when is_map(D) -> flatten(Rest, [[Name | R] || R <- Res, Name <- maps:keys(D)]);
flatten([E | Rest], Res)                -> flatten(Rest, [[E | R] || R <- Res]).



scope(ModulePath, ModuleMap) ->
    % The export scope is a pair of paths {K, P}. For any module `a/b/c` with
    % an import of `a/b` then K would be `c` and P would be `a/b/c`
    ExportScope = [{P ++ [D], P ++ ExportPath} || {P, {module, _, _, _, Exports, _}} <- maps:to_list(ModuleMap),
                                                  {D, {export, _, ExportPath, _}} <- maps:to_list(Exports)],

    % We include in the prefix scope all modules for which the local module
    % path is a prefix. Example: If the local module is `a/b` then we include
    % `a/b/c` as `c` and `a/b/c/d` as `c/d`
    PrefixScope = [{P -- ModulePath, P} || {_, P} <- ExportScope, lists:prefix(ModulePath, P)],

    % We include in the local scope any keywords or tagged values defined in
    % the current module, so that if the local module defines `def t -> (A |
    % B)`, then `t/A` and `t/B` are in the import scope.
    {module, _, _, _, _, LocalDefs} = maps:get(ModulePath, ModuleMap),
    LocalScope = [{(P -- ModulePath) ++ [D], P ++ [D]} || {P, {module, _, _, _, _, Defs}} <- maps:to_list(ModuleMap),
                                                           D <- maps:keys(Defs),
                                                           maps:is_key(module:lark_name([lists:last(P), D]),  LocalDefs)],

    utils:unique(PrefixScope ++ ExportScope ++ LocalScope).



aliases(ImportPaths, Scope, ModuleMap) -> aliases(ImportPaths, Scope, ModuleMap, [], []).
aliases([], _, _, [], Res) -> {ok, Res};
aliases([], _, _, Errors, _) -> error:collect(lists:reverse(Errors));
aliases([{Alias, [beam | IPath], Term} | Paths], Scope, ModuleMap, Errors, Res) ->
    case beam(Alias, IPath, Term) of
        {error, Errs}   -> aliases(Paths, Scope, ModuleMap, [{error, Errs} | Errors], Res);
        {ok, Aliases}   -> aliases(Paths, Scope, ModuleMap, Errors, Res ++ Aliases)
    end;
aliases([{Alias, IPath, Term} | Paths], Scope, ModuleMap, Errors, Res) ->
    % When Import path is prefix of module path(s) in scope
    % - We collect all matching module paths in scope, subtract the prefix and add them to scope
    % - We create aliases qualified by the remainding module path
    TrimmedPath = trim_wildcard(IPath),
    Aliases = [lark(Alias, K, IPath, P, ModuleMap, Term) || {K, P} <- Scope,
                                                            lists:prefix(TrimmedPath, K),
                                                            length(IPath) =< length(K)],
    ScopeF = fun({alias, _, AP, {qualified_symbol, _, MP, Name}}) -> {AP, MP ++ [Name]};
                ({alias, _, AP, {keyword, _, MP, Name}}) -> {AP, MP ++ [Name]}
             end,
    NewScope = [ScopeF(A) || A <- Aliases],
    case NewScope of
        []  -> Err = case lists:last(IPath) of
                         '_'    -> error:format({nonexistent_module, module:lark_name(TrimmedPath)},
                                                {import, Term});
                         _      -> error:format({nonexistent_export, source, module:lark_name(IPath)},
                                                {import, Term})
                     end,
               aliases(Paths, Scope, ModuleMap, [Err | Errors], Res);
        _   -> aliases(Paths, Scope ++ NewScope, ModuleMap, Errors, Res ++ Aliases)
    end.

dep(ModulePath, {alias, _, _, {qualified_symbol, _, P, _}}) -> [{dependency, #{}, ModulePath, P}];
dep(ModulePath, {alias, _, _, {keyword, _, P, _}}) -> [{dependency, #{}, ModulePath, P}];
dep(_, {alias, _, _, {beam_symbol, _, _, _}}) -> [].

is_local({alias, _, _, {qualified_symbol, _, ModulePath, _}}, ModulePath) -> true;
is_local({alias, _, _, {keyword, _, ModulePath, _}}, ModulePath) -> true;
is_local(_, _) -> false.

trim_wildcard(Path) ->
    {Init, [Last]} = lists:split(length(Path) - 1, Path),
    case Last of
        '_' -> Init;
        _   -> Path
    end.

% ImportPath: a/b/c/_, K: `a/b/c/D`, Alias: _ -> AliasPath: D
lark('_', K, ImportPath, Path, ModuleMap, Term) ->
    lark(K -- ImportPath, Path, ModuleMap, Term);
% ImportPath: a/b/c, K: `a/b/c/D`, Alias: f -> AliasPath: f/D
lark(Alias, K, ImportPath, Path, ModuleMap, Term) -> 
    AliasPath = [Alias | K -- ImportPath],
    lark(AliasPath, Path, ModuleMap, Term).
lark(AliasPath, Path, ModuleMap, Term) ->
    {ModulePath, [DefName]} = lists:split(length(Path) -1, Path),
    {module, _, _, _, _, Defs} = maps:get(ModulePath, ModuleMap),
    case maps:get(DefName, Defs) of
        {keyword, Ctx, _, _} = Keyword  -> NewCtx = maps:put(import, Term, Ctx),
                                           KeywordTerm = setelement(2, Keyword, NewCtx),
                                           {alias, NewCtx, AliasPath, KeywordTerm};
        {link, Ctx, LinkPath, Name}     -> NewCtx = maps:put(import, Term, Ctx),
                                           LinkTerm = {qualified_symbol, NewCtx, LinkPath, Name},
                                           {alias, Ctx, AliasPath, LinkTerm};
        Def                             -> Ctx = symbol:ctx(Def),
                                           NewCtx = maps:put(import, Term, Ctx),
                                           Symbol = {qualified_symbol, NewCtx, ModulePath, DefName},
                                           {alias, Ctx, AliasPath, Symbol}
    end.


beam(_, [ModulePath], Term) -> 
    case beam('_', '_', [ModulePath], Term) of
        {error, Errs}   -> {error, Errs};
        {ok, Aliases}   -> QualifiedAliases = [{alias, Ctx, [ModulePath, Alias], ATerm} ||
                                               {alias, Ctx, [Alias], ATerm} <- Aliases],
                           {ok, QualifiedAliases}
    end;
beam(Alias, Path, Term) ->
    {ModulePath, [Name]} = lists:split(length(Path) - 1, Path),
    beam(Alias, Name, ModulePath, Term).
beam(Alias, Name, ModulePath, Term) ->
    Module = module:beam_name(ModulePath),
    ModuleName = module:lark_name(ModulePath),
    case code:is_loaded(Module) of
        {file, _}   -> error:collect(loaded_module_function(ModulePath, Name, Alias, Term));
        false       -> case code:which(Module) of
                           non_existing -> error:format({nonexistent_module, ModuleName}, {import, Term});
                           _            ->
                               case code:ensure_loaded(Module) of
                                   {error, Err} ->
                                       error:format({error_loading_module, Module, Err}, {import, Term});
                                   _            ->
                                       error:collect(loaded_module_function(ModulePath, Name, Alias, Term))
                               end
                       end
    end.

loaded_module_function(ModulePath, '_', _, Term) ->
    Module = module:beam_name(ModulePath),
    ExportList = erlang:apply(Module, module_info, [exports]),
    Exports = lists:delete(module_info, utils:unique([Name || {Name, _Arity} <- ExportList])),
    lists:flatten([loaded_module_function(ModulePath, Name, Name, Term)
                   || Name <- Exports]);

loaded_module_function(ModulePath, Name, Alias, Term) ->
    ErrorCtx = element(2, Term),
    Module = module:beam_name(ModulePath),
    ImportPath = ModulePath ++ [Name],
    ImportName = module:lark_name(ImportPath),
    Exports = erlang:get_module_info(Module, exports),
    SubAliases = beam_subtypes(ImportPath ++ [Name], Alias, Term),
    case lists:filter(fun({Export, _}) -> Export =:= Name end, Exports) of
        []      -> [error:format({nonexistent_export, beam, ImportName}, {import, Term})];
        _       -> [{ok, {alias, ErrorCtx, [Alias],
                          {beam_symbol, #{import => Term}, ModulePath, Name}}} | SubAliases]
    end.

beam_subtypes(ImportPath, Alias, Term) ->
    case beam('_', ImportPath ++ ['_'], Term) of
        {error, _}      -> [];
        {ok, Aliases}   -> [{ok, {alias, symbol:ctx(Term), [Alias | T], {beam_symbol, #{import => Term}, ImportPath, T}}}
                            || {alias, _, T, _} <- Aliases]
    end.

whitelisted(Aliases, ModuleMap, Options) ->
    Extract = fun({alias, _, _, {beam_symbol, _, ModulePath, Name}}) -> {ModulePath, Name};
                 ({alias, _, _, {qualified_symbol, _, ModulePath, Name}}) -> {ModulePath, Name};
                 ({alias, _, _, {keyword, _, ModulePath, Name}}) -> {ModulePath, Name} end,
    Check = fun({alias, _, _, Symbol} = Alias) ->
                {ModulePath, Name} = Extract(Alias),
                case maps:is_key(ModulePath, ModuleMap) orelse is_whitelisted(module:beam_name(ModulePath), Name) of
                    true    -> {ok, Alias};
                    false   -> Term = maps:get(import, symbol:ctx(Symbol)),
                               error:format({function_not_whitelisted, ModulePath, Name}, {import, Term})
                end end,
    case maps:get(sandboxed, Options, ?DEFAULT_SANDBOXED) of
        false   -> {ok, Aliases};
        true    -> error:collect(lists:map(Check, Aliases))
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
                  'io_lib' => [],
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
                               purge_module, put, self, cancel_timer, delete_module,
                               hibernate, nif_error, spawn, spawn_link, spawn_monitor,
                               spawn_opt, spawn_request, suspend_process, system_flag,
                               system_info, system_monitor, system_profile,
                               register, resume_process, send, send_after,
                               send_nosuspend],
                  'persistent_term' => [],
                  'zlib' => []},
    maps:is_key(Module, Whitelist) andalso not(lists:member(Name, maps:get(Module, Whitelist))).
