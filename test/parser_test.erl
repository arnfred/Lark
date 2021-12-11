-module(parser_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

local_type_test_() ->
    Module = 
    "def xor\n"
    "    True False -> True,\n"
    "    False True -> True,\n"
    "    _ _ -> False",
    ?test({ok, _}, parser:parse([{text, Module}])).

nested_local_type_test_() ->
    Module =
    "def blup -> Blip | Blap
     import blup/_
     def flup -> Flip | Blap
     def main -> flup/Blap",
    {ok, Modules} = parser:parse([{text, test_code, Module}], #{include_kind_libraries => false}),
    ModuleMap = maps:from_list([{Path, Mod} || {module, _, Path, _, _, _} = Mod <- Modules]),
    ?test(#{[source, test_code] := {module, _, [source, test_code], _, _,
                                   #{main := {def, _, 'main',
                                              {keyword, _, [source, test_code, 'blup'], 'Blap'}}}}}, ModuleMap).


local_type_alias_test_() ->
    Module = 
    "import kind/prelude/boolean/{True: T, False: F}\n"
    "def xor\n"
    "    T F -> T,\n"
    "    F T -> T,\n"
    "    _ _ -> F",
    ?test({ok, _}, parser:parse([{text, Module}])).

local_type_no_import_test_() ->
    Module = 
    "def boolean -> True | False\n"
    "import boolean/False\n"
    "def notxor\n"
    "    True False -> False,\n"
    "    False True -> False,\n"
    "    _ _ -> False",
    ?testError({undefined_symbol, 'True'}, parser:parse([{text, Module}], #{import_prelude => false})).

local_type_wildcard_test_() ->
    Module = 
    "def boolean -> True | False\n"
    "import boolean/_\n"
    "def xor\n"
    "    True False -> True,\n"
    "    False True -> True,\n"
    "    _ _ -> False",
    ?test({ok, _}, parser:parse([{text, Module}], #{import_prelude => false})).

external_type_test_() ->
    Module1 = 
    "module blup (export {boolean}; def boolean -> True | False)",
    Module2 = "import blup/boolean/_
               def xor
                   True False -> True,
                   False True -> True,
                   _ _ -> False",
    ?test({ok, _}, parser:parse([{text, Module1}, {text, Module2}], #{import_prelude => false})).

undefined_external_type_test_() ->
    Module1 = 
    "module blup (export {boolean}
                  def boolean -> True | False)",
    Module2 =
    "import blup/Blap/_",
    ?testError({nonexistent_module, 'blup/Blap'},
               parser:parse([{text, Module1}, {text, Module2}], #{import_prelude => false})).

source_def_test_() ->
    Module1 = 
    "module blup (export {identity}
                  def identity a -> a)",
    Module2 =
    "import blup/identity\n"
    "def blap a -> a.identity",
    ?test({ok, _}, parser:parse([{text, Module2}, {text, Module1}])).

unexported_source_def_test_() ->
    Module1 = 
    "module blup (def identity a -> a)",
    Module2 =
    "import blup/identity\n"
    "def blap a -> a.identity",
    ?testError({nonexistent_export, source, 'blup/identity'}, parser:parse([{text, Module2}, {text, Module1}])).

beam_def_test_() ->
    Module =
    "import beam/lists/reverse\n"
    "def blap a -> a.reverse",
    ?test({ok, _}, parser:parse([{text, Module}])).

unexported_beam_def_test_() ->
    Module =
    "import beam/lists/blup\n"
    "def blap a -> a.blup",
    ?testError({nonexistent_export, beam, 'lists/blup'}, parser:parse([{text, Module}])).

wildcard_beam_def_test_() ->
    Module =
    "import beam/lists/_\n"
    "def blap a -> a.reverse",
    ?test({ok, _}, parser:parse([{text, Module}])).

import_conflict_test_() ->
    Module1 = 
    "module blup (export {identity}
                  def identity a -> a)",
    Module2 = 
    "module blip (export {identity}
                  def identity a -> a)",
    Module3 =
    "import blup/identity\n"
    "import blip/_\n"
    "def blap a -> a.identity",
    ?testError({duplicate_import, 'identity', 'source/test_code_3', 'blip/identity', 'blup/identity'},
               parser:parse([{text, test_code_2, Module2}, {text, test_code_1, Module1}, {text, test_code_3, Module3}])).

import_already_defined_test_() ->
    Module1 = 
    "module blip (export {identity}
                  def identity a -> a)",
    Module2 =
    "import blip/identity\n"
    "def identity a -> a",
    ?testError({import_conflicts_with_local_def, 'identity', 'source/test_code_2', 'blip/identity'},
               parser:parse([{text, test_code_2, Module2}, {text, test_code_1, Module1}])).

wildcard_import_type_already_defined_test_() ->
    Module1 = 
    "module blip (export {t}
                  def t -> A | B)",
    Module2 =
    "import blip/_\n"
    "def t -> Q | R",
    ?testError({import_conflicts_with_local_def, 't', 'source/test_code_2', 'blip/t'},
               parser:parse([{text, test_code_2, Module2}, {text, test_code_1, Module1}])).

import_alias_already_defined_test_() ->
    Module1 = 
    "module blip (export {identity}
                  def identity a -> a)",
    Module2 =
    "import blip/{identity: id}\n"
    "def id a -> a",
    ?testError({import_conflicts_with_local_def, 'id', 'source/test_code_2', 'blip/identity'},
               parser:parse([{text, test_code_2, Module2}, {text, test_code_1, Module1}])).

multiple_beam_import_test_() ->
    Module =
    "import beam/maps/_
     import beam/lists/_
     def blap a -> a.reverse.from_list",
    ?testError({duplicate_import, merge, 'source/test_code', 'lists/merge','maps/merge' },
               parser:parse([{text, test_code, Module}], #{include_kind_libraries => false})).

qualified_beam_import_test_() ->
    Module =
    "import beam/lists/_
     def blap -> reverse
     def blop a -> reverse(a)",
    ?test({ok, [{module, _, _, _, _,
                 #{blap := {def, _, 'blap',
                            {beam_symbol, _, [lists], reverse}},
                   blop := {def, _, _, {'fun', _, [{clause, _, [{variable, _, a, A}],
                                                    {beam_application, _, [lists], reverse,
                                                     [{variable, _, a, A}]}}]}}}}]},
          parser:parse([{text, Module}], #{include_kind_libraries => false})).

qualified_source_import_test_() ->
    Module1 = 
    "module blip (export {t}
                  def t -> (A | B))",
    Module2 =
    "import blip/t\n"
    "def blap -> t/A",
    {ok, Modules} = parser:parse([{text, test_code_1, Module1}, {text, test_code_2, Module2}],
                                 #{include_kind_libraries => false}),
    ModuleMap = maps:from_list([{module:kind_name(Path), Mod} || {module, _, Path, _, _, _} = Mod <- Modules]),
    ?test(#{'source/test_code_2' := {module, _, [source, test_code_2], _, _,
                 #{blap := {def, _, 'blap',
                            {keyword, _, [blip, t], 'A'}}}}}, ModuleMap).

qualified_local_import_test_() ->
    Module = 
    "def t -> (A | B)\n"
    "import t/A\n"
    "def blap -> A",
    {ok, Modules} = parser:parse([{text, test_code, Module}],
                                 #{include_kind_libraries => false}),
    ModuleMap = maps:from_list([{module:kind_name(Path), Mod} || {module, _, Path, _, _, _} = Mod <- Modules]),
    ?test(#{'source/test_code' := {module, _, [source, test_code], _, _,
                                   #{blap := {def, _, 'blap',
                                              {keyword, _, [source, test_code, 't'], 'A'}}}}}, ModuleMap).

module_order_test_() ->
    Module = "module test (export {t}
                           def t -> (A | B))",
    {ok, Modules} = parser:parse([{text, test_code, Module}], #{include_kind_libraries => false}),
    ModuleNames = [module:kind_name(M) || M <- Modules],
    ?test(['test', 'test/t'], ModuleNames).

prelude_keyword_import_test_() ->
    Source = "def f False -> True",
    {ok, Modules} = parser:parse([{text, test_code, Source}], #{include_kind_libraries => true}),
    ModuleMap = maps:from_list([{module:kind_name(Path), Mod} || {module, _, Path, _, _, _} = Mod <- Modules]),
    ?test(#{'source/test_code' := {module, _, _, _, _,
                                   #{f := {def, _, 'f',
                                           {'fun', _, [{clause, _,
                                                        [{keyword, _, [kind, prelude, boolean], 'False'}],
                                                        {keyword, _, [kind, prelude, boolean], 'True'}}]}}}}},
          ModuleMap).
