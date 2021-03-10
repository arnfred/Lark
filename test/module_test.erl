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
                   ?test(#{source_test_file := {module,
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
                   ?test(#{source_test_file := {module,
                                                #{filename := "test/file.kind"},
                                                [source, test, file],
                                                [{import, #{}, [noop]}],
                                                #{},
                                                #{}}}, Modules)
           end).

empty_module_test_() ->
    ?setup("test/file.kind",
           "module test_module {}",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [source, test, file, #{}]}],
                                           #{},
                                           #{}}}, Modules)
           end).

module_def_test_() ->
    ?setup("test/file.kind",
           "module test_module {} (
                def blah -> noop
            )",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [source, test, file, #{}]}],
                                           #{},
                                           #{blah := {def, #{}, blah, {symbol, _, variable, noop}}}}}, Modules)
           end).

module_import_test_() ->
    ?setup("test/file.kind",
           "module test_module {} (
                import blah/_
            )",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [source, test, file, #{}]},
                                            {import, #{}, [blah, '_']}],
                                           #{},
                                           #{}}}, Modules)
           end).

module_export_test_() ->
    ?setup("test/file.kind",
           "module test_module {T: A, T/A} (
                type T -> A
            )",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _, _},
                           test_module_T := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [source, test, file, #{}]},
                                            {import, #{}, ['T']}],
                                           #{'T' := {export, #{}, ['T'], {symbol, #{}, type, 'A'}},
                                             'A' := {export, #{}, ['T', 'A'], none}},
                                           #{'T' := _}}}, Modules)
           end).

root_export_test_() ->
    ?setup("test/file.kind",
           "module test_module {T: A, T/A}
            type T -> A",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _, _},
                           source_test_file_T := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [source, test, file, #{}]}],
                                           #{'T' := {export, #{}, ['T'], {symbol, #{}, type, 'A'}},
                                             'A' := {export, #{}, ['T', 'A'], none}},
                                           #{}}}, Modules)
           end).

export_missing_test_() ->
    ?setup("test/file.kind",
           "module test_module {T, T/A}",
           fun(Error) ->
                    [?testError({export_missing, 'T'}, Error)]
           end).

export_already_defined_test_() ->
    ?setup("test/file.kind",
           "module test_module {T/A} (
                type T -> A
                type A -> A)",
           fun(Error) ->
                    [?testError({export_already_defined, 'T/A', 'A'}, Error)]
           end).

export_unsupported_test_() ->
    ?setup("test/file.kind",
           "module test_module {t/T/A}",
           fun(Error) ->
                    [?testError({export_unsupported, 't/T/A'}, Error)]
           end).

subtype_test_() ->
    ?setup("test/file.kind",
           "type T -> A",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _,
                                                #{'T' := {type_def, _, 'T', {symbol, _, type, 'A'}}}},
                           source_test_file_T := {module,
                                                  #{line := 1},
                                                  [source, test, file, 'T'],
                                                  [{import, #{}, [source, test, file, #{'T' := 'T'}]}],
                                                  #{'A' := {export, _, ['A'], _}},
                                                  #{'A' := {type_def, _, 'A', {symbol, _, type, 'A'}}}}}, Modules)
           end).

subtype_tagged_test_() ->
    ?setup("test/file.kind",
           "type T -> R: A",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _,
                                                #{'T' := {type_def, _, 'T', {tagged, _, ['T', 'R'], {symbol, _, type, 'A'}}}}},
                           source_test_file_T := {module,
                                           #{line := 1},
                                           [source, test, file, 'T'],
                                           _,
                                           #{'A' := {export, _, ['A'], _},
                                             'R' := {export, _, ['R'], _}},
                                           #{'A' := {type_def, _, 'A', {symbol, #{}, type, 'A'}},
                                             'R' := {type_def, _, 'R', {tagged, _, ['T', 'R'], {symbol, _, type, 'A'}}}}
                                          }}, Modules)
           end).

duplicate_module_error_test_() ->
    ?setup("test/file.kind",
           "module test_module {}
            module test_module {}",
           fun(Error) ->
                    [?testError({duplicate_module, 'test_module', "test/file.kind", "test/file.kind"}, Error)]
           end).

module_def_link_test_() ->
    ?setup("test/file.kind",
           "def f -> noop
            module test_module {f}",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _, #{f := _}},
                           test_module := {module,
                                           #{line := 2},
                                           [test_module],
                                           _,
                                           #{f := {export, _, [f], _}},
                                           #{f := {link, _, {qualified_symbol, _, [source, test, file], f}}}}}, Modules)
           end).

module_sub_def_link_test_() ->
    ?setup("test/file.kind",
           "def T -> (A | B)
            module test_module {T}",
           fun({ok, Modules}) ->
                   ?test(#{source_test_file := {module, _, _, _, _, #{'T' := _}},
                           test_module := {module,
                                           #{line := 2},
                                           [test_module],
                                           _,
                                           #{'T' := {export, _, ['T'], _}},
                                           #{'T' := {link, _, {qualified_symbol, _, [source, test, file], 'T'}}}},
                          test_module_T := {module, _, [test_module, 'T'], _,
                                            #{'A' := _, 'B' := _},
                                            #{'A' := {link, _, {qualified_symbol, _, [source, test, file, 'T'], 'A'}},
                                              'B' := {link, _, {qualified_symbol, _, [source, test, file, 'T'], 'B'}}}},
                          source_test_file_T := {module, _, [source, test, file, 'T'], _,
                                                 #{'A' := _, 'B' := _},
                                                 #{'A' := _, 'B' := _}}}, Modules)
           end).

nested_link_test_() ->
    ?setup("test/file.kind",
           "type Blup -> (Blip | Blap)
            import Blup/_
            type Flup -> (Flip | Blap)
            import Flup/{Flip, Blap: Blop}
            type Blonk -> (Blank | Blop)
            module t {Blonk}",
           fun({ok, Modules}) ->
                   [?test(#{source_test_file := {module, _, _, _, _, #{'Blup' := _, 'Flup' := _, 'Blonk' := _}}}, Modules),
                    ?test(#{source_test_file_Blup := {module, _, _, _, _, #{'Blip' := _, 'Blap' := _}}}, Modules),
                    ?test(#{source_test_file_Flup := {module, _, _, _, _,
                                                      #{'Flip' := {type_def, _, _, _},
                                                        'Blap' := {link, _, {qualified_symbol, _,
                                                                             [source, test, file, 'Blup'],
                                                                             'Blap'}}}}}, Modules),
                    ?test(#{source_test_file_Blonk := {module, _, _, _, _,
                                                       #{'Blank' := {type_def, _, _, _},
                                                         'Blop' := {link, _, {qualified_symbol, _,
                                                                              [source, test, file, 'Blup'],
                                                                              'Blap'}}}}}, Modules),
                   ?test(#{t_Blonk := {module, _, _, _, _,
                                       #{'Blank' := {link, _, {qualified_symbol, _,
                                                               [source, test, file, 'Blonk'], 'Blank'}},
                                         'Blop' := {link, _, {qualified_symbol, _,
                                                              [source, test, file, 'Blup'], 'Blap'}}}}}, Modules)]
           end).
