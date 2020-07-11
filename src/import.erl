-module(import).
-export([format/2]).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").



format({import, _, Path} = Import, SourceMap) ->
    case lists:reverse(Path) of
        []                                  -> error:format({empty_import}, {import, Import});
        [{symbol, _, _, Elem}]              -> error:format({single_item_import, Elem}, {import, Import});

        % test/Test             -> [{Test, test/Test}]
        % test/_                -> [{Test, test/Test},
        %                           {blah, test/blah}]
        [{symbol, _, _, Name} | T]          -> Rest = lists:reverse(T),
                                               imports(Name, Name, Rest, SourceMap, Import);
        % test/{Test: Blip}     -> [{Blip, test/Test}]
        % test/{Test, blah}     -> [{Test, test/Test},
        %                           {blah, test/blah}]
        [{dict, _, Elements} | Tail]        -> 
            Rest = lists:reverse(Tail),
            F = fun({pair, _, {symbol, _, _, '_'}, {symbol, _, _, Alias}} = Term) ->
                        error:format({import_underscore_for_name, Alias}, {import, Term});
                   ({pair, _, {symbol, _, _, Name}, {symbol, _, _, '_'}} = Term) ->
                        error:format({import_underscore_for_alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, T, Name}, {symbol, _, T, Alias}} = Term) ->
                        imports(Name, Alias, Rest, SourceMap, Term);
                   ({pair, _, {symbol, _, type, Name}, {symbol, _, variable, Alias}} = Term) ->
                        error:format({import_def_alias_for_type, Alias, Name}, {import, Term});
                   ({pair, _, {symbol, _, variable, Name}, {symbol, _, type, Alias}} = Term) ->
                        error:format({import_type_alias_for_def, Alias, Name}, {import, Term});
                   ({symbol, _, _, Name} = Term) ->
                        imports(Name, Name, Rest, SourceMap, Term);
                   (Other) ->
                        error:format({unrecognized_dict_import, Other}, {import, Other})
                end,
            error:map(error:collect([F(Elem) || Elem <- Elements]), fun lists:flatten/1);

        Other                               ->
            error:format({unrecognized_import, Other}, {import, Import})
    end.


imports(Name, Alias, Path, SourceMap, ErrorCtx) ->
    case lists:reverse(Path) of

        % 1. check if it's an erlang module and import if it is
        [{_, _, _, Module}, {_, _, _, erlang}]  -> beam_function(Module, Name, Alias, ErrorCtx);
        [{_, _, _, erlang}]                     -> beam_function(erlang, Name, Alias, ErrorCtx);

        % 2. Check if it's a local type import and if it is, add a rewrite instruction
        [{symbol, _, type, Parent}]             -> {ok, [{rewrite, ErrorCtx, Alias, [Parent, Name]}]};

        % 3. Check if it's a non-local type import and add the import + rewrite instruction
        [{symbol, _, type, Parent} | Tail]      ->
            case imports(Parent, Parent, lists:reverse(Tail), SourceMap, ErrorCtx) of 
                {error, Errs} -> {error, Errs};
                {ok, Imports} -> {ok, [{rewrite, ErrorCtx, Alias, [Parent, Name]} | Imports]}
            end;

        % 4. Otherwise, import from source or beam file
        _                                       ->
            ModuleName = module:beam_name([P || {symbol, _, _, P} <- Path]),
            case maps:get(ModuleName, SourceMap, undefined) of
                % 4a. check if it's a compiled kind module and look up if it is
                undefined    -> beam_function(ModuleName, Name, Alias, ErrorCtx);

                % 4b. check if it's a source module and import if it is + adding source module to dependencies
                Module       -> kind_source_function(Module, Name, Alias, ErrorCtx)
            end
    end.


beam_function(Module, Name, Alias, ErrorCtx) ->
    io:format("Beam Module: ~p~n", [Module]),
    case code:is_loaded(Module) of
        {file, _}   -> loaded_module_function(Module, Name, Alias, ErrorCtx);
        false       -> case code:which(Module) of
                           non_existing -> 
                               error:format({nonexistent_module, Module}, {import, ErrorCtx});
                           _            -> 
                               case code:ensure_loaded(Module) of
                                   {error, Err} -> 
                                       error:format({error_loading_module, Module, Err}, {import, ErrorCtx});
                                   _            -> 
                                       loaded_module_function(Module, Name, Alias, ErrorCtx)
                               end
                       end
    end.

loaded_module_function(Module, '_', _, ErrorCtx) ->
    io:format("Loaded Module: ~p~n", [Module]),
    Exports = erlang:apply(Module, module_info, [exports]),
    {ok, [{alias, ErrorCtx, Name, [Module, Name, Arity]} || {Name, Arity} <- Exports]};

loaded_module_function(Module, Name, Alias, ErrorCtx) ->
    Exports = erlang:apply(Module, module_info, [exports]),
    case lists:filter(fun({Export, _}) -> Export =:= Name end, Exports) of
        []      -> error:format({nonexistent_import, beam, Module, Name}, {import, ErrorCtx});
        Matches -> {ok, [{alias, ErrorCtx, Alias, [Module, Name, Arity]} || {_, Arity} <- Matches]}
    end.

kind_source_function({module, _, ModuleName, _, Exports}, '_', _, ErrorCtx) ->
    Dependency = {dependency, ErrorCtx, ModuleName},
    Aliases = [{alias, ErrorCtx, Name, [ModuleName, Name]} || Name <- maps:keys(Exports)],
    {ok, Aliases ++ [Dependency]};

kind_source_function({module, _, ModuleName, KindName, Exports}, Name, Alias, ErrorCtx) ->
    Dependency = {dependency, ErrorCtx, ModuleName},
    case maps:get(Name, Exports, undefined) of
        undefined   -> error:format({nonexistent_import, source, KindName, Name}, {import, ErrorCtx});
        _           -> {ok, [{alias, ErrorCtx, Alias, [ModuleName, Name]}, Dependency]}
    end.


-ifdef(TEST).

test_format(Symbols, SourceMap) -> 
    format({import, #{}, [{symbol, #{}, variable, S} || S <- Symbols]}, SourceMap).

test_format(Symbols, Mappings, SourceMap) -> 
    F = fun({Name, Name}) -> {symbol, #{}, variable, Name};
           ({Alias, Name}) -> {pair, #{}, {symbol, #{}, variable, Alias}, {symbol, #{}, variable, Name}} end,
    Module = [{symbol, #{}, variable, S} || S <- Symbols],
    Path = Module ++ [{dict, #{}, [F(Elem) || Elem <- maps:to_list(Mappings)]}],
    format({import, #{}, Path}, SourceMap).

erlang_import_test() ->
    Actual = test_format([erlang, atom_to_list], #{}),
    ?assertMatch({ok, [{alias, _, atom_to_list, [erlang, atom_to_list, 1]}]}, Actual).

erlang_module_import_test() ->
    Actual = test_format([erlang, lists, reverse], #{}),
    ?assertMatch({ok, [{alias, _, reverse, [lists, reverse, 1]},
                       {alias, _, reverse, [lists, reverse, 2]}]}, Actual).

source_import_test() ->
    SourceMap = #{module_name => {module, #{}, module_name, 'module/name', #{test_fun => blup}}},
    Actual = test_format([module_name, test_fun], SourceMap),
    ?assertMatch({ok, [{alias, _, test_fun, [module_name, test_fun]},
                       {dependency, _, module_name}]}, Actual).

beam_wildcard_test() ->
    Actual = test_format([random, '_'], #{}),
    ?assertMatch({ok, [{alias, _, seed0, [random, seed0, 0]},
                       {alias, _, seed, [random, seed, 0]},
                       {alias, _, seed, [random, seed, 1]},
                       {alias, _, seed, [random, seed, 3]},
                       {alias, _, uniform, [random, uniform, 0]},
                       {alias, _, uniform, [random, uniform, 1]},
                       {alias, _, uniform_s, [random, uniform_s, 1]},
                       {alias, _, uniform_s, [random, uniform_s, 2]},
                       {alias, _, module_info, [random, module_info, 0]},
                       {alias, _, module_info, [random, module_info, 1]}]}, Actual).

source_wildcard_test() ->
    SourceMap = #{blap => {module, #{}, blap, blap, #{fun1 => blip,
                                                      fun2 => blop,
                                                      fun3 => blup}}},
    Actual = test_format([blap, '_'], SourceMap),
    ?assertMatch({ok, [{alias, _, fun1, [blap, fun1]},
                       {alias, _, fun2, [blap, fun2]},
                       {alias, _, fun3, [blap, fun3]},
                       {dependency, _, blap}]}, Actual).

beam_dict_test() ->
    Actual = test_format([random], #{seed => glunk, uniform => uniform}, #{}),
    ?assertMatch({ok, [{alias, _, glunk, [random, seed, 0]},
                       {alias, _, glunk, [random, seed, 1]},
                       {alias, _, glunk, [random, seed, 3]},
                       {alias, _, uniform, [random, uniform, 0]},
                       {alias, _, uniform, [random, uniform, 1]}]}, Actual).

source_dict_test() ->
    SourceMap = #{blap => {module, #{}, blap, blap, #{fun1 => blip,
                                                      fun2 => blop,
                                                      fun3 => blup}}},
    Actual = test_format([blap], #{fun1 => blarg, fun2 => fun2}, SourceMap),
    ?assertMatch({ok, [{alias, _, blarg, [blap, fun1]},
                       {dependency, _, blap},
                       {alias, _, fun2, [blap, fun2]},
                       {dependency, _, blap}]}, Actual).

module_type_test() ->
    SourceMap = #{blap => {module, #{}, blap, blap, #{'Blup' => blip}}},
    Path = [{symbol, #{}, variable, blap}, {symbol, #{}, type, 'Blup'}, {symbol, #{}, type, 'A'}],
    Actual = format({import, #{}, Path}, SourceMap),
    ?assertMatch({ok, [{rewrite, _, 'A', ['Blup', 'A']},
                       {alias, _, 'Blup', [blap, 'Blup']},
                       {dependency, _, blap}]}, Actual).

wildcard_type_test() ->
    SourceMap = #{blap => {module, #{}, blap, blap, #{'Blup' => blip}}},
    Path = [{symbol, #{}, variable, blap}, {symbol, #{}, type, 'Blup'}, {symbol, #{}, variable, '_'}],
    Actual = format({import, #{}, Path}, SourceMap),
    ?assertMatch({ok, [{rewrite, _, '_', ['Blup', '_']},
                       {alias, _, 'Blup', [blap, 'Blup']},
                       {dependency, _, blap}]}, Actual).

local_type_test() ->
    Path = [{symbol, #{}, type, 'Blup'}, {symbol, #{}, variable, 'A'}],
    Actual = format({import, #{}, Path}, #{}),
    ?assertMatch({ok, [{rewrite, _, 'A', ['Blup', 'A']}]}, Actual).

errors_test_() ->
    SourceMap = #{blap => {module, #{}, blap, blap, #{'Blup' => blip}}},
    [?_errorMatch({empty_import},
                  format({import, #{}, []}, #{})),
     ?_errorMatch({single_item_import, 'Blah'},
                  test_format(['Blah'], #{})),
     ?_errorMatch({import_underscore_for_alias, blah},
                  test_format(['blap'], #{blah => '_'}, SourceMap)),
     ?_errorMatch({import_underscore_for_name, blah},
                  test_format(['blap'], #{'_' => blah}, SourceMap)),
     ?_errorMatch({import_def_alias_for_type, a, 'A'},
                  format({import, #{}, [{symbol, #{}, variable, blap},
                                        {dict, #{}, [{pair, #{},
                                                      {symbol, #{}, type, 'A'},
                                                      {symbol, #{}, variable, a}}]}]}, #{})),
     ?_errorMatch({import_type_alias_for_def, 'A', a},
                  format({import, #{}, [{symbol, #{}, variable, blap},
                                        {dict, #{}, [{pair, #{},
                                                      {symbol, #{}, variable, a},
                                                      {symbol, #{}, type, 'A'}}]}]}, #{})),
     ?_errorMatch({unrecognized_dict_import, {blup}},
                  format({import, #{}, [{symbol, #{}, variable, blap},
                                        {dict, #{}, [{blup}]}]}, #{})),
     ?_errorMatch({unrecognized_import, [{blip}]},
                  format({import, #{}, [{blip}]}, #{})),
     ?_errorMatch({nonexistent_module, blap},
                  test_format([blap, blip], #{})),
     ?_errorMatch({nonexistent_import, beam, random, blip},
                  test_format([random, blip], #{})),
     ?_errorMatch({nonexistent_import, source, blap, blip},
                  test_format([blap, blip], SourceMap))].

-endif.

                   
                   

        









% Dynamic vs Static import
%
% A dynamic import isn't checked by the compiler and is done at run-time. This
% means that we can import for example a function. Then when calling a symbol
% in noun(args) form, we call the function with the noun as a keyword and expect
% it to return a function that can be called with the arguments. This is useful
% for stuff like a 'sys' module for calling command line functions.
%
% A static import covers over a dictionary with known keys at compile time. A
% module dictionary for example might consists of a few defs and types:
%
% module ABC {
%   not: (Boolean -> Boolean)
%   List
%   xor: (Boolean Boolean -> Boolean)
%   reverse: (a -> b) where (a.length == b.length
%   			   a ~ Ordered)
% }
%
% In the code, an import might then look like this: `import abc/List` to import
% the `List` type, or `import abc/_` for a global import of all module
% declarations. To import a few elements, use `import ABC/{List, not:
% abc_not}`. The latter will import the function `not` under the name of
% `abc_not`.
%
% For qualified imports, it's possible to import the module as `import ABC` and
% import individual elements.
%
% To implement this, here's the steps we'll need:
% 1. Add module definition to lexer, parser
% 2. rework `kind.erl` to work with code with and without modules (e.g. insert
%    module lines in code snippets that don't have them)
% 3. Add file method to load a file instead of text to `kind`
% 4. Create lib directory and add prelude file
% 5. add import definition to lexer, parser
% 6. add tests
%
