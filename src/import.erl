-module(import).
-export([import/3]).

import({import, _, Path} = Import, SourceMap, LocalTypes) ->
    case lists:reverse(Path) of
        []                             -> error:format({empty_import}, {import, Import});

        % import lists                 -> [{lists/reverse, lists/reverse},
        %                                  {lists/split, lists/split},
        %                                  ...]
        [{symbol, _, _, Name}]         -> import_module(Name, Path, SourceMap, LocalTypes, Import);

        % import test/Test             -> [{Test, test/Test}]
        % import test/_                -> [{Test, test/Test},
        %                                  {blah, test/blah}]
        [{symbol, _, _, Name} | T]     -> Rest = lists:reverse(T),
                                          import_def(Name, Name, Rest, SourceMap, LocalTypes, Import);
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
                        import_def(Name, Alias, Rest, SourceMap, LocalTypes, Term);
                   ({pair, _, {symbol, _, type, Name}, {symbol, _, variable, Alias}} = Term) ->
                        error:format({import_def_alias_for_type, Alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, variable, Name}, {symbol, _, type, Alias}} = Term) ->
                        error:format({import_type_alias_for_def, Alias, Name}, {import, Term});
                   ({symbol, _, _, Name} = Term) ->
                        import_def(Name, Name, Rest, SourceMap, LocalTypes, Term);
                   (Other) ->
                        error:format({unrecognized_dict_import, Other}, {import, Other})
                end,
            error:map(error:collect([F(Elem) || Elem <- Elements]), fun lists:flatten/1);

        Other                               ->
            error:format({unrecognized_import, Other}, {import, Import})
    end.

import_module(ModuleName, Path, SourceMap, LocalTypes, ErrorCtx) ->
    case import_def('_', '_', Path, SourceMap, LocalTypes, ErrorCtx) of
        {error, Errs}   -> {error, Errs};
        {ok, Aliases}   -> [{alias, Ctx, [ModuleName, Alias], Term} || {alias, Ctx, Alias, Term} <- Aliases]
    end.

import_def(Name, Alias, Path, SourceMap, LocalTypes, ErrorCtx) ->
    case lists:reverse(Path) of

        % 1. Check if it's a local type import and if it is, look it up in the typemap
        [{symbol, _, type, Parent}]             -> local_type(Parent, Name, Alias, LocalTypes, ErrorCtx);

        % 2. Otherwise, import from source or beam file
        _                                       ->
            Tag = get_tag(Path),
            ModulePath = [P || {symbol, _, _, P} <- Path],
            ModuleName = module:beam_name(ModulePath),
            io:format("SourceMap for ~p: ~p~n", [ModuleName, SourceMap]),
            case maps:get(ModuleName, SourceMap, undefined) of
                % 2a. check if it's a compiled kind module and look up if it is
                undefined    -> beam_function(Tag, ModulePath, Name, Alias, ErrorCtx);

                % 2b. check if it's a source module and import if it is
                Module       -> kind_source_function(Tag, Module, Name, Alias, ErrorCtx)
            end
    end.

local_type(Parent, '_', _, LocalTypes, ErrorCtx) ->
    case maps:get(Parent, LocalTypes, undefined) of
        undefined   -> error:format({undefined_local_type, Parent, maps:keys(LocalTypes)},
                                    {import, ErrorCtx});
        SubTypes    -> {ok, [{alias, ErrorCtx, T, {type, #{import => ErrorCtx}, T, [Parent, T]}} || T <- SubTypes]}
    end;

local_type(Parent, Name, Alias, LocalTypes, ErrorCtx) ->
    case maps:is_key(Parent, LocalTypes) of
        false   -> error:format({undefined_local_type, Parent, maps:keys(LocalTypes)},
                                {import, ErrorCtx});
        true    -> 
            case lists:member(Name, maps:get(Parent, LocalTypes)) of
                false   -> error:format({undefined_local_type, Parent, Name, maps:keys(LocalTypes)},
                                        {import, ErrorCtx});
                true    -> {ok, [{alias, ErrorCtx, Alias, {type, #{import => ErrorCtx}, Alias, [Parent, Name]}}]}
            end
    end.


beam_function(Tag, ModulePath, Name, Alias, ErrorCtx) ->
    Module = module:beam_name(ModulePath),
    io:format("Beam Module: ~p~n", [Module]),
    case code:is_loaded(Module) of
        {file, _}   -> loaded_module_function(Tag, ModulePath, Name, Alias, ErrorCtx);
        false       -> case code:which(Module) of
                           non_existing -> 
                               error:format({nonexistent_module, Module}, {import, ErrorCtx});
                           _            -> 
                               case code:ensure_loaded(Module) of
                                   {error, Err} -> 
                                       error:format({error_loading_module, Module, Err}, {import, ErrorCtx});
                                   _            -> 
                                       loaded_module_function(Tag, ModulePath, Name, Alias, ErrorCtx)
                               end
                       end
    end.

loaded_module_function(Tag, ModulePath, '_', _, ErrorCtx) ->
    Module = module:beam_name(ModulePath),
    io:format("Loaded Module: ~p~n", [Module]),
    ExportList = erlang:apply(Module, module_info, [exports]),
    Exports = lists:delete(module_info, utils:unique([Name || {Name, _Arity} <- ExportList])),
    {ok, [{alias, ErrorCtx, Name,
           {Tag, #{import => ErrorCtx}, ModulePath, Name}} || Name <- Exports]};

loaded_module_function(Tag, ModulePath, Name, Alias, ErrorCtx) ->
    Module = module:beam_name(ModulePath),
    Exports = erlang:apply(Module, module_info, [exports]),
    case lists:filter(fun({Export, _}) -> Export =:= Name end, Exports) of
        []      -> error:format({nonexistent_import, beam, Module, Name}, {import, ErrorCtx});
        _       -> {ok, [{alias, ErrorCtx, Alias,
                          {Tag, #{import => ErrorCtx}, ModulePath, Name}}]}
    end.

kind_source_function(Tag, {module, _, ModulePath, Exports}, '_', _, ErrorCtx) ->
    Aliases = [{alias, ErrorCtx, Name,
                {Tag, #{import => ErrorCtx}, ModulePath, Name}} || Name <- maps:keys(Exports)],
    {ok, Aliases};

kind_source_function(Tag, {module, _, ModulePath, Exports}, Name, Alias, ErrorCtx) ->
    io:format("Exports for ~p: ~p~n", [Name, Exports]),
    case maps:is_key(Name, Exports) of
        false   -> error:format({nonexistent_import, source, module:kind_name(ModulePath), Name}, {import, ErrorCtx});
        true    -> {ok, [{alias, ErrorCtx, Alias, 
                          {Tag, #{import => ErrorCtx}, ModulePath, Name}}]}
    end.

get_tag(Path) -> case lists:reverse(Path) of
                      [{symbol, _, _, '_'} | [{symbol, _, variable, _} | _]] -> qualified_variable;
                      [{symbol, _, _, '_'} | [{symbol, _, type, _} | _]]     -> qualified_type;
                      [{symbol, _, variable, _} | _]                         -> qualified_variable;
                      [{symbol, _, type, _} | _]                             -> qualified_type
                  end.

