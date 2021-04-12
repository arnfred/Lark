-module(module_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

load(FileName, Code) -> 
    {ok, {FileName, AST}} = parser:to_ast(FileName, Code),
    module:parse([{FileName, AST}]).

-define(setup(FileName, Code, Tests), {setup, fun() -> load(FileName, Code) end, fun(_) -> ok end, Tests}).


root_def_test_() ->
    ?setup("test/file.kind",
           "def blah -> noop",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module,
                                                #{filename := "test/file.kind", line := 0},
                                                [source, test, file],
                                                [],
                                                #{blah := {export, _, [blah], none}},
                                                #{blah := {def, #{}, blah, _}}}}, Modules)
           end).

root_import_test_() ->
    ?setup("test/file.kind",
           "import noop",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module,
                                                #{filename := "test/file.kind"},
                                                [source, test, file],
                                                [{import, #{}, [noop]}],
                                                #{},
                                                #{}}}, Modules)
           end).

empty_module_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {}",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [{import, #{}, [source, test, file, #{}]}],
                                           #{},
                                           #{}}}, Modules)
           end).

module_def_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {} (
                def blah -> noop
            )",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [{import, #{}, [source, test, file, #{}]}],
                                           #{},
                                           #{blah := {def, #{}, blah, {symbol, _, variable, noop}}}}}, Modules)
           end).

module_import_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {} (
                import blah/_
            )",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [{import, #{}, [source, test, file, #{}]},
                                            {import, #{}, [blah, '_']}],
                                           #{},
                                           #{}}}, Modules)
           end).

module_export_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {t: A, t/A} (
                def t -> A
            )",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['test', 'module1', 't'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [{import, #{}, [source, test, file, #{}]},
                                            {import, #{}, ['t']}],
                                           #{'t' := {export, #{}, ['t'], {symbol, #{}, keyword, 'A'}},
                                             'A' := {export, #{}, ['t', 'A'], none}},
                                           #{'t' := _}}}, Modules)
           end).

root_export_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {t: A, t/A}
            def t -> A",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, _},
                           ['source', 'test', 'file', 't'] := {module, _, _, _, _, _},
                           ['test', 'module1'] := {module,
                                           #{},
                                           [test, module1],
                                           [{import, #{}, [source, test, file, #{}]}],
                                           #{'t' := {export, #{}, ['t'], {symbol, #{}, keyword, 'A'}},
                                             'A' := {export, #{}, ['t', 'A'], none}},
                                           #{}}}, Modules)
           end).

export_missing_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {t, t/A}",
           fun(Error) ->
                    [?testError({export_missing, 't'}, Error)]
           end).

export_unsupported_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {t/t/A}",
           fun(Error) ->
                    [?testError({export_unsupported, 't/t/A'}, Error)]
           end).

subtype_test_() ->
    ?setup("test/file.kind",
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
                             #{'A' := {export, _, ['A'], _}},
                             #{'A' := {keyword, _, [source, test, file, t], 'A'}}}}, Modules)]
           end).

subtype_tagged_test_() ->
    ?setup("test/file.kind",
           "def t -> R: A",
           fun({ok, Modules}) ->
                   [?test(#{[source, test, file] :=
                            {module, _, _, _, _,
                             #{'t' := {def, _, 't',
                                       {tagged, _, ['t', 'R'], {symbol, _, keyword, 'A'}}},
                               't/A' := {keyword, _, [source, test, file, t], 'A'},
                               't/R' := {def, _, 'R',
                                         {tagged, _, [source, test, file, 't', 'R'], {symbol, _, keyword, 'A'}}}}}}, Modules),
                    ?test(#{['source', 'test', 'file', 't'] :=
                            {module,
                             #{line := 1},
                             [source, test, file, 't'],
                             _,
                             #{'A' := {export, _, ['A'], _},
                               'R' := {export, _, ['R'], _}},
                             #{'A' := {keyword, _, [source, test, file, t], 'A'},
                               'R' := {link, _, [source, test, file], 't/R'}}}}, Modules)]
           end).

duplicate_module_error_test_() ->
    ?setup("test/file.kind",
           "module test/module1 {}
            module test/module1 {}",
           fun(Error) ->
                    [?testError({duplicate_module, 'test/module1', "test/file.kind", "test/file.kind"}, Error)]
           end).

module_def_link_test_() ->
    ?setup("test/file.kind",
           "def f -> noop
            module test/module1 {f}",
           fun({ok, Modules}) ->
                   ?test(#{['source', 'test', 'file'] := {module, _, _, _, _, #{f := _}},
                           ['test', 'module1'] := {module,
                                           #{line := 2},
                                           [test, module1],
                                           _,
                                           #{f := {export, _, [f], _}},
                                           #{f := {link, _, [source, test, file], f}}}}, Modules)
           end).

module_sub_def_link_test_() ->
    ?setup("test/file.kind",
           "def t -> (A | B)
            module test/module1 {t}",
           fun({ok, Modules}) ->
                   [?test(#{['source', 'test', 'file'] :=
                            {module, _, _, _, _, 
                             #{'t' := {def, _, 't', _},
                               't/A' := {keyword, _, _, 'A'},
                               't/B' := {keyword, _, _, 'B'}}}},
                          Modules),
                    ?test(#{['test', 'module1'] :=
                            {module,
                             #{line := 2},
                             [test, module1],
                             _,
                             #{'t' := {export, _, ['t'], _}},
                             #{'t' := {link, _, [source, test, file], 't'}}}},
                          Modules),
                    ?test(#{['test', 'module1', 't'] :=
                            {module, _, [test, module1, 't'], _,
                             #{'A' := _, 'B' := _},
                             #{'A' := {keyword, _, [source, test, file, t], 'A'},
                               'B' := {keyword, _, [source, test, file, t], 'B'}}}},
                          Modules),
                    ?test(#{['source', 'test', 'file', 't'] :=
                            {module, _, [source, test, file, 't'], _,
                             #{'A' := _, 'B' := _},
                             #{'A' := {keyword, _, [source, test, file, t], 'A'},
                               'B' := {keyword, _, [source, test, file, t], 'B'}}}},
                         Modules)]
           end).

nested_link_test_() ->
    ?setup("test/file.kind",
           "def blup -> (Blip | Blap)
            import blup/_
            def flup -> (Flip | Blap)
            import flup/{Flip, Blap: Blop}
            def blonk -> (Blank | Blop)
            module t {blonk}",
           fun({ok, Modules}) ->
                   [?test(#{['source', 'test', 'file'] :=
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
                    ?test(#{['source', 'test', 'file', 'blup'] :=
                            {module, _, _, _, _, #{'Blip' := _, 'Blap' := _}}}, Modules),
                    ?test(#{['source', 'test', 'file', 'flup'] :=
                            {module, _, _, _, _,
                             #{'Flip' := {keyword, _, [source, test, file, flup], 'Flip'},
                               'Blap' := {keyword, _, [source, test, file, blup], 'Blap'}}}}, Modules),
                    ?test(#{['source', 'test', 'file', 'blonk'] :=
                            {module, _, _, _, _,
                             #{'Blank' := {keyword, _, [source, test, file, blonk], 'Blank'},
                               'Blop' := {keyword, _, [source, test, file, blup], 'Blap'}}}}, Modules),
                   ?test(#{['t', 'blonk'] :=
                           {module, _, _, _, _,
                            #{'Blank' := {keyword, _, [source, test, file, blonk], 'Blank'},
                              'Blop' := {keyword, _, [source, test, file, blup], 'Blap'}}}}, Modules)]
           end).

local_keyword_test_() ->
    ?setup("test/file.kind",
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
    ?setup("test/file.kind",
           "module test {s} (def t -> A
                             import t/A
                             def s -> A
                             def r -> s/A)",
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
