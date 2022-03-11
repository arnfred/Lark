-module(module_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

load(FileName, Code) -> 
    {ok, {FileName, AST}} = parser:to_ast(FileName, Code),
    module:parse([{FileName, AST}]).

-define(setup(FileName, Code, Tests), {setup, fun() -> load(FileName, Code) end, fun(_) -> ok end, Tests}).


root_def_test_() ->
    ?setup("test/file.lark",
           "def blah -> noop",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] :=
                           {module,
                            #{filename := "test/file.lark", line := 0},
                            [source, test, file],
                            [],
                            #{},
                            #{blah := {def, #{}, blah, _}}}}, Modules)
           end).

root_import_test_() ->
    ?setup("test/file.lark",
           "import noop",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module,
                                                #{filename := "test/file.lark"},
                                                [source, test, file],
                                                [{import, #{}, [noop]}],
                                                #{},
                                                #{}}}, Modules)
           end).

empty_module_test_() ->
    ?setup("test/file.lark",
           "module test/module1 ()",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, #{}, _, [], _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [],
                                           #{},
                                           #{}}}, Modules)
           end).

module_def_test_() ->
    ?setup("test/file.lark",
           "module test/module1 (
                def blah -> noop
            )",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [],
                                           #{},
                                           #{blah := {def, #{}, blah, {symbol, _, variable, noop}}}}}, Modules)
           end).

module_import_test_() ->
    ?setup("test/file.lark",
           "module test/module1 (
                import blah/_
            )",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [{import, #{}, [blah, '_']}],
                                           #{},
                                           #{}}}, Modules)
           end).

module_export_test_() ->
    ?setup("test/file.lark",
           "module test/module1 (
                export {t: A, t/A} 
                def t -> A
            )",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1', 't'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [],
                                           #{'t' := {export, #{}, ['t'], {symbol, #{}, keyword, 'A'}},
                                             'A' := {export, #{}, ['t', 'A'], none}},
                                           #{'t' := _}}}, Modules)
           end).


module_multiple_export_test_() ->
    ?setup("test/file.lark",
           "module test/module1 (
                export {t: A} 
                def t -> A
                export {t/A} 
            )",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1', 't'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [],
                                           #{'t' := {export, #{}, ['t'], {symbol, #{}, keyword, 'A'}},
                                             'A' := {export, #{}, ['t', 'A'], none}},
                                           #{'t' := _}}}, Modules)
           end).

root_export_test_() ->
    ?setup("test/file.lark",
           "module test/module1 (export {t: A, t/A})
            def t -> A",
           fun(Error) ->
                   ?testError({export_missing, t}, {export_missing, 't/A'}, Error)
           end).

export_missing_test_() ->
    ?setup("test/file.lark",
           "module test/module1 (export {t, t/A})",
           fun(Error) ->
                    [?testError({export_missing, 't'}, Error)]
           end).

export_qualified_symbol_test_() ->
    ?setup("test/file.lark",
           "module test/module1 (export {t/t/A})",
           fun(Error) ->
                    [?testError({export_missing, 't/t/A'}, Error)]
           end).

subtype_test_() ->
    ?setup("test/file.lark",
           "def t -> A",
           fun({ok, Modules}) ->
                   [?test(#{[source, test, file] :=
                            {module, _, _, _, _,
                             #{'t' := {def, _, 't', {symbol, _, keyword, 'A'}},
                               't/A' := {keyword, _, [source, test, file, t], 'A'}}}}, Modules),
                    ?test(#{[source, test, file, t] :=
                            {module,
                             #{line := 1},
                             [source, test, file, 't'],
                             [],
                             #{},
                             #{'A' := {keyword, _, [source, test, file, t], 'A'}}}}, Modules)]
           end).

subtype_tagged_test_() ->
    ?setup("test/file.lark",
           "def t -> (R: A)",
           fun({ok, Modules}) ->
                   [?test(#{[source, test, file] :=
                            {module, _, _, _, _,
                             #{'t' := {def, _, 't',
                                       {tagged, _, ['t', 'R'], {symbol, _, keyword, 'A'}}},
                               't/A' := {keyword, _, [source, test, file, t], 'A'},
                               't/R' := {def, _, 't/R',
                                         {'fun', _,
                                          [{clause, _,
                                            [{pair, _,
                                              {symbol, _, variable, Subst},
                                              {symbol, _, keyword, 'A'}}],
                                            {tagged, _, ['t', 'R'], {symbol, _, variable, Subst}}}]}}}}}, Modules),
                    ?test(#{['source', 'test', 'file', 't'] :=
                            {module,
                             #{line := 1},
                             [source, test, file, 't'],
                             _,
                             #{},
                             #{'A' := {keyword, _, [source, test, file, t], 'A'},
                               'R' := {link, _, [source, test, file], 't/R'}}}}, Modules)]
           end).

duplicate_module_error_test_() ->
    ?setup("test/file.lark",
           "module test/module1 ()
            module test/module1 ()",
           fun(Error) ->
                    [?testError({duplicate_module, 'test/module1', "test/file.lark", "test/file.lark"}, Error)]
           end).

nested_link_test_() ->
    ?setup("test/file.lark",
            "module t (def blup -> (Blip | Blap)
                       import blup/_
                       def flup -> (Flip | Blap)
                       import flup/{Flip, Blap: Blop}
                       def blonk -> (Blank | Blop)
                       export {blonk})",
           fun({ok, Modules}) ->
                   [?test(#{['t'] :=
                            {module, _, _, _, _, 
                             #{'blup' := _,
                               'blup/Blap' := _,
                               'blup/Blip' := _,
                               'flup' := _,
                               'flup/Flip' := _,
                               'flup/Blap' := _,
                               'blonk' := _,
                               'blonk/Blank' := _,
                               'blonk/Blop' := _}}}, Modules),
                    ?test(#{['t', 'blup'] :=
                            {module, _, _, _, _, #{'Blip' := _, 'Blap' := _}}}, Modules),
                    ?test(#{['t', 'flup'] :=
                            {module, _, _, _, _,
                             #{'Flip' := {keyword, _, [t, flup], 'Flip'},
                               'Blap' := {keyword, _, [t, blup], 'Blap'}}}}, Modules),
                    ?test(#{['t', 'blonk'] :=
                            {module, _, _, _, _,
                             #{'Blank' := {keyword, _, [t, blonk], 'Blank'},
                               'Blop' := {keyword, _, [t, blup], 'Blap'}}}}, Modules)]
           end).

local_keyword_test_() ->
    ?setup("test/file.lark",
           "def t -> A
            import t/A
            def s -> A
            def r -> s/A",
    fun({ok, Modules}) ->
            [?test(#{[source, test, file] := 
                     {module, _, _, _, _,
                      #{'t' := {def, _, 't', {symbol, _, _, 'A'}},
                        's' := {def, _, 's', {symbol, _, _, 'A'}},
                        'r' := {def, _, 'r', {qualified_symbol, _, [{_, _, _, 's'}, {_, _, _, 'A'}]}},
                        't/A' := {keyword, _, [source, test, file, t], 'A'},
                        's/A' := {keyword, _, [source, test, file, t], 'A'}}}}, Modules),
             ?test(#{[source, test, file, 's'] :=
                     {module, _, _, _, _,
                      #{'A' := {keyword, _, [source, test, file, t], 'A'}}}}, Modules)]
    end).

local_module_keyword_test_() ->
    ?setup("test/file.lark",
           "module test (export {s}
                         def t -> A
                         import t/A
                         def s -> A
                         def r -> s/A)
           ",
    fun({ok, Modules}) ->
            [?test(#{[test] := 
                     {module, _, _, _, _,
                      #{'t' := {def, _, 't', {symbol, _, _, 'A'}},
                        's' := {def, _, 's', {symbol, _, _, 'A'}},
                        'r' := {def, _, 'r', {qualified_symbol, _, [{_, _, _, 's'}, {_, _, _, 'A'}]}},
                        't/A' := {keyword, _, [test, t], 'A'},
                        's/A' := {keyword, _, [test, t], 'A'}}}}, Modules),
             ?test(#{[test, 's'] :=
                     {module, _, _, _, _,
                      #{'A' := {keyword, _, [test, t], 'A'}}}}, Modules)]
    end).


empty_module_import_test_() ->
    ?setup("test/file.lark",
           "module non-empty (export {boolean}
                              def boolean -> True | False)
            module empty ()",
    fun({ok, Modules}) ->
            [?test(#{[source, test, file] := {module, _, _, [{import, _, ['non-empty']}], _, _}}, Modules)]
    end).

no_fictionous_root_modules_test_() ->
    % When a module implements a submodule (because it contains keywords or
    % tagged values) we don't want the root module to also create this
    % submodule
    ?setup("test/file.lark",
           "module non-empty (export {boolean}
                              def boolean -> True | False)",
    fun({ok, Modules}) -> [?testEqual(maps:is_key([source, test, file, boolean], Modules), false)] end).

module_root_module_self_import_test_() ->
    % When the root module imports a def in the module, this import shouldn't
    % also apply to the module itself
    ?setup("test/file.lark",
           "module m (export {inc}
                      def inc n -> #(n, n))
            import m/inc",
           fun({ok, Modules}) ->
                   [?test(#{[m] := {module, _, _, [], _, _}}, Modules)]
           end).

