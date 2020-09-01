-module(import).
-export([import/3]).

import({import, _, ImportPath} = Import, SourceMap, LocalTypes) ->
    case lists:reverse(ImportPath) of
        []                             -> error:format({empty_import}, {import, Import});

        % import lists                 -> [{lists/reverse, lists/reverse},
        %                                  {lists/split, lists/split},
        %                                  ...]
        [{symbol, _, _, Name}]         -> import_module(Name, ImportPath, SourceMap, LocalTypes, Import);

        % import test/Test             -> [{Test, test/Test}]
        % import test/_                -> [{Test, test/Test},
        %                                  {blah, test/blah}]
        [{symbol, _, _, Name} | T]     -> Rest = lists:reverse(T),
                                          error:collect(import_def(Name, Name, Rest, SourceMap, LocalTypes, Import));
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
                        error:collect(import_def(Name, Alias, Rest, SourceMap, LocalTypes, Term));
                   ({pair, _, {symbol, _, type, Name}, {symbol, _, variable, Alias}} = Term) ->
                        error:format({import_def_alias_for_type, Alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, variable, Name}, {symbol, _, type, Alias}} = Term) ->
                        error:format({import_type_alias_for_def, Alias, Name}, {import, Term});
                   ({symbol, _, _, Name} = Term) ->
                        error:collect(import_def(Name, Name, Rest, SourceMap, LocalTypes, Term));
                   (Other) ->
                        error:format({unrecognized_dict_import, Other}, {import, Other})
                end,
            error:map(error:collect([F(Elem) || Elem <- Elements]), fun lists:flatten/1);

        Other                               ->
            error:format({unrecognized_import, ImportPath}, {import, Import})
    end.

import_module(ModuleName, ImportPath, SourceMap, LocalTypes, ErrorCtx) ->
    case error:collect(import_def('_', '_', ImportPath, SourceMap, LocalTypes, ErrorCtx)) of
        {error, Errs}   -> {error, Errs};
        {ok, Aliases}   -> [{alias, Ctx, [ModuleName, Alias], Term} || {alias, Ctx, Alias, Term} <- Aliases]
    end.

import_def(Name, Alias, ImportPath, SourceMap, LocalTypes, ErrorCtx) ->
    case lists:reverse(ImportPath) of

        % 1. Check if it's a local type import and if it is, look it up in the typemap
        [{symbol, _, type, Parent}]             -> local_type(Parent, Name, Alias, LocalTypes, ErrorCtx);

        % 2. Otherwise, import from source or beam file
        _                                       ->
            ModulePath = [P || {symbol, _, _, P} <- ImportPath],
            ModuleName = module:beam_name(ModulePath),
            case maps:get(ModuleName, SourceMap, undefined) of
                % 2a. check if it's a compiled kind module and look up if it is
                undefined    -> beam_function(ModulePath, Name, Alias, ErrorCtx);

                % 2b. check if it's a source module and import if it is
                Module       -> kind_source_function(Module, Name, Alias, SourceMap, ErrorCtx)
            end
    end.

local_type(Parent, '_', _, LocalTypes, ErrorCtx) ->
    case maps:get(Parent, LocalTypes, undefined) of
        undefined   -> [error:format({undefined_local_type, Parent, maps:keys(LocalTypes)},
                                     {import, ErrorCtx})];
        SubTypes    -> [{ok, {alias, ErrorCtx, T, {type, #{import => ErrorCtx}, T, [Parent, T]}}} || T <- SubTypes]
    end;

local_type(Parent, Name, Alias, LocalTypes, ErrorCtx) ->
    case maps:is_key(Parent, LocalTypes) of
        false   -> [error:format({undefined_local_type, Parent, maps:keys(LocalTypes)},
                                {import, ErrorCtx})];
        true    -> 
            case lists:member(Name, maps:get(Parent, LocalTypes)) of
                false   -> [error:format({undefined_local_type, Parent, Name, maps:keys(LocalTypes)},
                                        {import, ErrorCtx})];
                true    -> [{ok, {alias, ErrorCtx, Alias, {type, #{import => ErrorCtx}, Alias, [Parent, Name]}}}]
            end
    end.


beam_function(ModulePath, Name, Alias, ErrorCtx) ->
    Module = module:beam_name(ModulePath),
    ModuleName = module:kind_name(ModulePath),
    ImportName = module:kind_name(ModulePath ++ [Name]),
    case code:is_loaded(Module) of
        {file, _}   -> loaded_module_function(ModulePath, Name, Alias, ErrorCtx);
        false       -> case code:which(Module) of
                           non_existing -> 
                               case get_tag(Name) of
                                   qualified_type       -> [error:format({nonexistent_import, source, ImportName}, {import, ErrorCtx})];
                                   qualified_variable   -> [error:format({nonexistent_module, ModuleName}, {import, ErrorCtx})]
                               end;
                           _            -> 
                               case code:ensure_loaded(Module) of
                                   {error, Err} -> 
                                       [error:format({error_loading_module, Module, Err}, {import, ErrorCtx})];
                                   _            -> 
                                       loaded_module_function(ModulePath, Name, Alias, ErrorCtx)
                               end
                       end
    end.

loaded_module_function(ModulePath, '_', _, ErrorCtx) ->
    Module = module:beam_name(ModulePath),
    ExportList = erlang:apply(Module, module_info, [exports]),
    Exports = lists:delete(module_info, utils:unique([Name || {Name, _Arity} <- ExportList])),
    lists:flatten([loaded_module_function(ModulePath, Name, Name, ErrorCtx) 
                   || Name <- Exports]);

loaded_module_function(ModulePath, Name, Alias, ErrorCtx) ->
    Module = module:beam_name(ModulePath),
    ImportPath = ModulePath ++ [Name],
    ImportName = module:kind_name(ImportPath),
    Exports = erlang:apply(Module, module_info, [exports]),
    TypeAliases = beam_subtypes(get_tag(Name), ImportPath, Alias, ErrorCtx),
    case lists:filter(fun({Export, _}) -> Export =:= Name end, Exports) of
        []      -> [error:format({nonexistent_import, beam, ImportName}, {import, ErrorCtx})];
        _       -> [{ok, {alias, ErrorCtx, Alias,
                          {get_tag(Name), #{import => ErrorCtx}, ModulePath, Name}}} | TypeAliases]
    end.

beam_subtypes(qualified_variable, _, _, _) -> [];
beam_subtypes(qualified_type, ImportPath, Alias, ErrorCtx) ->
    [{ok, {alias, ErrorCtx, [Alias, T],
           {get_tag(T), #{import => ErrorCtx}, ImportPath, T}}}
     || {ok, {alias, _, T, _}} <- beam_function(ImportPath, '_', '_', ErrorCtx)].

kind_source_function({module, _, ModulePath, _} = Module, Name, Alias, SourceMap, ErrorCtx) ->
    [{dependency, ErrorCtx, ModulePath} | kind_source_aliases(Module, Name, Alias, SourceMap, ErrorCtx)].

kind_source_aliases({module, _, _, Exports} = Module, '_', _, SourceMap, ErrorCtx) ->
    lists:flatten([kind_source_aliases(Module, Name, Name, SourceMap, ErrorCtx) 
                   || Name <- maps:keys(Exports)]);

kind_source_aliases({module, _, ModulePath, Exports}, Name, Alias, SourceMap, ErrorCtx) ->
    ImportPath = ModulePath ++ [Name],
    ImportName = module:kind_name(ImportPath),
    ImportBeamName = module:beam_name(ImportPath),
    TypeAliases = kind_subtypes(get_tag(Name), ImportPath, ImportBeamName, Alias, SourceMap, ErrorCtx),
    case maps:is_key(Name, Exports) of
        false   -> [error:format({nonexistent_import, source, ImportName}, {import, ErrorCtx})];
        true    -> [{ok, {alias, ErrorCtx, Alias, 
                          {get_tag(Name), #{import => ErrorCtx}, ModulePath, Name}}} | TypeAliases]
    end.

kind_subtypes(qualified_variable, _, _, _, _, _) -> [];
kind_subtypes(qualified_type, ImportPath, ImportBeamName, Alias, SourceMap, ErrorCtx) ->
    case maps:get(ImportBeamName, SourceMap, undefined) of
        {module, _, _, Types} -> [{ok, {alias, ErrorCtx, [Alias, T],
                                        {get_tag(T), #{import => ErrorCtx}, ImportPath, T}}} 
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
