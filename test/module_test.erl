-module(module_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

load(FileName, Code) -> 
    {ok, {FileName, AST}} = parser:to_ast(FileName, Code),
    module:parse([{FileName, AST}]).

-define(setup(FileName, Code, Tests), {setup, fun() -> load(FileName, Code) end, fun(_) -> ok end, Tests}).


root_def_test_() ->
    ?setup("test_file",
           "def blah -> noop",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module,
                                         #{filename := "test_file", line := 0},
                                         [test_file],
                                         [],
                                         #{blah := {export, _, [blah], none}},
                                         #{blah := {def, #{}, blah, _}}}}, Modules)
           end).

root_import_test_() ->
    ?setup("test_file",
           "import noop",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module,
                                         #{filename := "test_file"},
                                         [test_file],
                                         [{import, #{}, [{symbol, _, variable, noop}]}],
                                         #{},
                                         #{}}}, Modules)
           end).

empty_module_test_() ->
    ?setup("test_file",
           "module test_module {}",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [{symbol, #{}, variable, test_file}, {symbol, _, variable, '_'}]}],
                                           #{},
                                           #{}}}, Modules)
           end).

module_def_test_() ->
    ?setup("test_file",
           "module test_module {} (
                def blah -> noop
            )",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [{symbol, #{}, variable, test_file}, {symbol, _, variable, '_'}]}],
                                           #{},
                                           #{blah := {def, #{}, blah, {symbol, _, variable, noop}}}}}, Modules)
           end).

module_import_test_() ->
    ?setup("test_file",
           "module test_module {} (
                import blah/_
            )",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [{symbol, #{}, variable, test_file}, {symbol, _, variable, '_'}]},
                                            {import, #{}, [{symbol, #{}, variable, blah}, {symbol, _, variable, '_'}]}],
                                           #{},
                                           #{}}}, Modules)
           end).

module_export_test_() ->
    ?setup("test_file",
           "module test_module {T: A, T/A} (
                type T -> A
            )",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module, _, _, _, _, _},
                           test_module_T := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [{symbol, #{}, variable, test_file}, {symbol, _, variable, '_'}]},
                                            {import, #{}, [{symbol, #{}, type, 'T'}]}],
                                           #{'T' := {export, #{}, ['T'], {symbol, #{}, type, 'A'}},
                                             'A' := {export, #{}, ['T', 'A'], none}},
                                           #{'T' := _}}}, Modules)
           end).

root_export_test_() ->
    ?setup("test_file",
           "module test_module {T: A, T/A}
            type T -> A",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module, _, _, _, _, _},
                           test_file_T := {module, _, _, _, _, _},
                           test_module := {module,
                                           #{},
                                           [test_module],
                                           [{import, #{}, [{symbol, #{}, variable, test_file}, {symbol, _, variable, '_'}]},
                                            {import, #{}, [{symbol, #{}, type, 'T'}]}],
                                           #{'T' := {export, #{}, ['T'], {symbol, #{}, type, 'A'}},
                                             'A' := {export, #{}, ['T', 'A'], none}},
                                           #{}}}, Modules)
           end).

export_missing_test_() ->
    ?setup("test_file",
           "module test_module {T, T/A}",
           fun(Error) ->
                    [?testError({export_missing, 'T'}, Error)]
           end).

export_already_defined_test_() ->
    ?setup("test_file",
           "module test_module {T/A} (
                type T -> A
                type A -> A)",
           fun(Error) ->
                    [?testError({export_already_defined, 'T/A', 'A'}, Error)]
           end).

export_unsupported_test_() ->
    ?setup("test_file",
           "module test_module {t/T/A}",
           fun(Error) ->
                    [?testError({export_unsupported, 't/T/A'}, Error)]
           end).

unintended_sub_module_test_() ->
    {"When modules are formed in the beginning of the pipeline, we nominally
     don't know that the constant `B` is a type imported from `T`. In this
     instance, `Q` doesn't define any constants and we shouldn't create a
     module called `test_file_Q`. To make sure we don't, the `module` module
     filters out the locally imported constants and only creates modules for
     definitions or types that define their own constants.",
     ?setup("test_file",
            "type T -> (A | B)
            import T/_
            type P -> (A | S)
            type Q -> B",

            fun({ok, Modules}) ->
                    ?test(['test_file', 'test_file_P', 'test_file_T'], maps:keys(Modules))
            end)}.

subtype_test_() ->
    ?setup("test_file",
           "type T -> A",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module, _, _, _, _,
                                         #{'T' := {type_def, _, 'T', {symbol, _, type, 'A'}}}},
                           test_file_T := {module,
                                           #{line := 1},
                                           [test_file, 'T'],
                                           [{import, #{}, [{symbol, #{}, variable, test_file},
                                                           {dict, _, [{symbol, _, variable, 'T'}]}]}],
                                           #{'A' := {export, _, ['A'], _}},
                                           #{'A' := {type_def, _, 'A', {symbol, _, type, 'A'}}}}}, Modules)
           end).

subtype_tagged_test_() ->
    ?setup("test_file",
           "type T -> R: A",
           fun({ok, Modules}) ->
                   ?test(#{test_file := {module, _, _, _, _,
                                         #{'T' := {type_def, _, 'T', {tagged, _, ['T', 'R'], {symbol, _, type, 'A'}}}}},
                           test_file_T := {module,
                                           #{line := 1},
                                           [test_file, 'T'],
                                           _,
                                           #{'A' := {export, _, ['A'], _},
                                             'R' := {export, _, ['R'], _}},
                                           #{'A' := {type_def, _, 'A', {symbol, #{}, type, 'A'}},
                                             'R' := {type_def, _, 'R', {tagged, _, ['T', 'R'], {symbol, _, type, 'A'}}}}
                                          }}, Modules)
           end).

duplicate_module_error_test_() ->
    ?setup("test_file",
           "module test_module {}
            module test_module {}",
           fun(Error) ->
                    [?testError({duplicate_module, 'test_module', "test_file", "test_file"}, Error)]
           end).
