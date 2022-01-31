-module(import_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

module_map(FileName, Code) -> 
    {ok, {FileName, AST}} = parser:to_ast(FileName, Code),
    {ok, Modules} = module:parse([{FileName, AST}]),
    Modules.

test_import(ImportPath, ModuleMap) -> test_import(ImportPath, ModuleMap, #{}).
test_import(ImportPath, ModuleMap, Options) ->
    Mod = empty_module(ImportPath),
    import:import(Mod, maps:put([test_module], Mod, ModuleMap), Options).

empty_module(ImportPath) -> {module, #{}, [test_module], [{import, #{}, ImportPath}], #{}, #{}}.

erlang_import_test_() ->
    Mod = empty_module([beam, erlang, atom_to_list]),
    Actual = import:import(Mod, #{[test_module] => Mod}),
    ?test({ok, {#{atom_to_list := [{beam_symbol, _, [erlang], atom_to_list}]}, []}}, Actual).

erlang_module_import_test_() ->
    Actual = test_import([beam, lists], #{}),
    ?test({ok, {#{'lists/reverse' := [{beam_symbol, _, [lists], reverse}]}, []}}, Actual).

source_import_test_() ->
    Code = "module test/mod (export {test_fun}
                             def test_fun -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([test, mod, test_fun], ModuleMap),
    ?test({ok, {#{test_fun := [{qualified_symbol, _, [test,mod], test_fun}]},
                [{dependency, _, [test_module], [test, mod]}]}}, Actual).

source_import_sub_path_test_() ->
    Code = "module a/b/c/d (export {test_fun}
                            def test_fun -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([a, b, c], ModuleMap),
    ?test({ok, {#{'c/d/test_fun' := [{qualified_symbol, _, [a, b, c, d], test_fun}]},
                [{dependency, _, [test_module], [a, b, c, d]}]}}, Actual).

source_import_sub_path_alias_test_() ->
    Code = "module a/b/c/d (export {test_fun}
                            def test_fun -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([a, b, #{c => alias}], ModuleMap),
    ?test({ok, {#{'alias/d/test_fun' := [{qualified_symbol, _, [a, b, c, d], test_fun}]},
                [{dependency, _, [test_module], [a, b, c, d]}]}}, Actual).

source_import_sub_path_multi_mod_test_() ->
    Code = "module a/b/c/d (export {test_fun}
                            def test_fun -> noop)
            module a/b/c/other (export {qworp}
                                def qworp -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([a, b, c], ModuleMap),
    ?test({ok, {#{'c/d/test_fun' := [{qualified_symbol, _, [a, b, c, d], test_fun}],
                  'c/other/qworp' := [{qualified_symbol, _, [a, b, c, other], qworp}]},
                [{dependency, _, [test_module], [a, b, c, d]},
                 {dependency, _, [test_module], [a, b, c, other]}]}}, Actual).


beam_wildcard_test_() ->
    Actual = test_import([beam, random, '_'], #{}),
    ?test({ok, {#{seed0 := [{beam_symbol, _, [random], seed0}],
                  seed := [{beam_symbol, _, [random], seed}],
                  uniform := [{beam_symbol, _, [random], uniform}],
                  uniform_s := [{beam_symbol, _, [random], uniform_s}]}, []}}, Actual).

source_wildcard_test_() ->
    Code = "module blap (
                export {fun1, fun2, fun3}
                def fun1 -> noop
                def fun2 -> noop
                def fun3 -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([blap, '_'], ModuleMap),
    ?test({ok, {#{fun1 := [{qualified_symbol, _, [blap], fun1}],
                  fun2 := [{qualified_symbol, _, [blap], fun2}],
                  fun3 := [{qualified_symbol, _, [blap], fun3}]},
               [{dependency, _, [test_module], [blap]}]}}, Actual).

beam_dict_test_() ->
    Actual = test_import([beam, random, #{seed => glunk, uniform => uniform}], #{}),
    ?test({ok, {#{glunk := [{beam_symbol, _, [random], seed}],
                  uniform := [{beam_symbol, _, [random], uniform}]}, []}}, Actual).

source_dict_test_() ->
    Code = "module blap (
                export {fun1, fun2, fun3}
                def fun1 -> noop
                def fun2 -> noop
                def fun3 -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([blap, #{fun1 => blarg, fun2 => fun2}], ModuleMap),
    ?test({ok, {#{blarg := [{qualified_symbol, _, [blap], fun1}],
                  fun2 := [{qualified_symbol, _, [blap], fun2}]},
                [{dependency, _, [test_module], [blap]}]}}, Actual).

module_keyword_test_() ->
    Code = "module m (export {blup}
                      def blup -> A)",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([m, 'blup'], ModuleMap),
    ?test({ok, {#{'blup' := [{qualified_symbol, _, [m], 'blup'}],
                  'blup/A' := [{keyword, _, [m, blup], 'A'}]},
                [{dependency, _, [test_module], [m]},
                 {dependency, _, [test_module], [m, blup]}]}}, Actual).

module_sub_keyword_test_() ->
    Code = "module m (export {blup}
                      def blup -> A)",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([m, 'blup', 'A'], ModuleMap),
    ?test({ok, {#{'A' := [{keyword, _, [m, blup], 'A'}]},
                [{dependency, _, [test_module], [m, blup]}]}}, Actual).

wildcard_keyword_test_() ->
    Code = "module m (export {blup}
                      def blup -> (A | B))",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([m, 'blup', '_'], ModuleMap),
    ?test({ok, {#{'A' := [{keyword, _, [m, blup], 'A'}],
                  'B' := [{keyword, _, [m, blup], 'B'}]},
               [{dependency, _, [test_module], [m, blup]}]}}, Actual).

transitive_local_keyword_test_() ->
    Code = "module m (export {blup}
                      def blup -> (A | B))
            module test_module (import m/blup/A)",
    ModuleMap = module_map("root_module", Code),
    #{[test_module] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := [{keyword, _, [m, blup], 'A'}]},
                [{dependency, _, [test_module], [m, blup]}]}}, Actual).

transitive_local_keyword_dict_import_test_() ->
    Code = "module test/module1 (export {t, s}
                                 def t -> A
                                 def s -> B)
            module test/module2 (import test/module1/{t: q, s}
                                 import s/_
                                 import q/_)",
    ModuleMap = module_map("root_module", Code),
    #{[test, module2] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := [{keyword, _, [test, module1, t], 'A'}],
                  'B' := [{keyword, _, [test, module1, s], 'B'}],
                  'q' := [{qualified_symbol, _, [test, module1], 't'}],
                  'q/A' := [{keyword, _, [test, module1, t], 'A'}],
                  's' := [{qualified_symbol, _, [test, module1], 's'}],
                  's/B' := [{keyword, _, [test, module1, s], 'B'}]},
                [{dependency, _, [test,module2], [test, module1]},
                 {dependency, _, [test,module2], [test, module1, s]},
                 {dependency, _, [test,module2], [test, module1, t]}]}}, Actual).

transitive_path_overlap_test_() ->
    Code = "module a/b/c (export {t}
                          def t -> A)
            import a/b/c
            import c/t",
    ModuleMap = module_map("test_module", Code), 
    #{[source, test_module] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'t' := [{qualified_symbol, _, [a, b, c], 't'}],
                  't/A' := [{keyword, _, [a, b, c, t], 'A'}]},
                [{dependency, _, [source, test_module], [a, b, c]},
                 {dependency, _, [source, test_module], [a, b, c, t]}]}}, Actual).

ambigious_transitive_local_keyword_test_() ->
    Code = "module test/module1 (export {blup}
                                 def blup -> (A | B))
            module test/module2 (export {blup}
                                 def blup -> (C | D))
            module test/module3 (import test/module1/_
                                 import test/module2/_
                                 import blup)",
    ModuleMap = module_map("test_file", Code),
    #{[test, module3] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{blup := [{qualified_symbol, _, [test, module1], blup},
                           {qualified_symbol, _, [test, module2], blup}],
                  'blup/A' := [{keyword, _, [test, module1, blup], 'A'}],
                  'blup/B' := [{keyword, _, [test, module1, blup], 'B'}],
                  'blup/C' := [{keyword, _, [test, module2, blup], 'C'}],
                  'blup/D' := [{keyword, _, [test, module2, blup], 'D'}]},
                [{dependency,_,[test,module3],[test,module1]},
                 {dependency,_,[test,module3],[test,module1,blup]},
                 {dependency,_,[test,module3],[test,module2]},
                 {dependency,_,[test,module3],[test,module2,blup]}]}}, Actual).

qualified_export_test_() ->
    Code = "module test/module1 (export {t/A}
                                 def t -> (A | B))
            module test/module2 (import test/module1/_)",
    ModuleMap = module_map("test_file", Code),
    #{[test, module2] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := [{keyword, _, [test, module1, t], 'A'}]},
               [{dependency, _, [test, module2], [test, module1, t]}]}}, Actual).

errors_test_() ->
    Code = "def blup -> A",
    ModuleMap = module_map("blap", Code),
    [?testError({nonexistent_export, source, 'blap/blip'},
                  test_import([blap, blip], #{})),
     ?testError({nonexistent_export, beam, 'random/blip'},
                  test_import([beam, random, blip], #{})),
     ?testError({nonexistent_export, source, 'source/blap/blip'},
                  test_import([source, blap, blip], ModuleMap))].

sandbox_test_() ->
    Options = #{sandboxed => true},
	[?testError({function_not_whitelisted, [timer], exit_after},
				test_import([beam, timer, exit_after], #{}, Options)),
	 ?testError({function_not_whitelisted, [filelib], is_dir},
                test_import([beam, filelib, is_dir], #{}, Options)),
	 ?test({ok, _}, test_import([beam, lists, reverse], #{}, Options))].

sandbox_keyword_test_() ->
    Options = #{sandboxed => true},
    Code = "module test/module1 (export {t/A}
                                 def t -> (A | B))
            module test/module2 (import test/module1/_)",
    ModuleMap = module_map("test_file", Code),
    #{[test, module2] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap, Options),
    ?test({ok, {#{'A' := [{keyword, _, [test, module1, t], 'A'}]},
               [{dependency, _, [test, module2], [test, module1, t]}]}}, Actual).


import_qualified_module_name_test_() ->
    Code = "module lark/prelude (export {option}
                                 def option -> Option)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([lark, prelude], ModuleMap),
    [?test({ok, {#{'prelude/option' := [{qualified_symbol, _, [lark, prelude], 'option'}],
                   'prelude/option/Option' := [{keyword, _, [lark, prelude, option], 'Option'}]},
                 [{dependency, _, [test_module], [lark, prelude]},
                  {dependency, _, [test_module], [lark, prelude, option]}]}}, Actual)].

nested_link_test_() ->
    Code = "module t (export {blonk}
                      def blup -> (Blip | Blap)
                      import blup/_
                      def flup -> (Flip | Blap)
                      import flup/{Flip, Blap: Blop}
                      def blonk -> (Blank | Blop))",
    ModuleMap = module_map("test/file.lark", Code),
    Actual = test_import([t, 'blonk'], ModuleMap),
    [?test({ok, {#{'blonk' := [{qualified_symbol, _, [t], 'blonk'}],
                  'blonk/Blank' := [{keyword, _, [t, blonk], 'Blank'}],
                  'blonk/Blop' := [{keyword, _, [t, blup], 'Blap'}]},
                 [{dependency, _, [test_module], [t]},
                  {dependency, _, [test_module], [t, blonk]},
                  {dependency, _, [test_module], [t, blup]}]}}, Actual)].


local_constant_test_() ->
    Code = "module test (export {s, r}
                         def t -> A
                         import t/A
                         def s -> A
                         def r -> s/A)",
    ModuleMap = module_map("test/file.lark", Code),
    Actual = test_import([test, 's', 'A'], ModuleMap),
    [?test({ok, {#{'A' := [{keyword, _, [test, t], 'A'}]},
                [{dependency, _, [test_module], [test, t]}]}}, Actual)].

local_import_test_() ->
    Code = "def t -> (A | B)
            import t/_",
    ModuleMap = module_map("test/file.lark", Code),
    #{[source, test, file] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    [?test({ok, {#{'A' := [{keyword, _, [source, test, file, t], 'A'}]}, _}}, Actual),
     ?test({ok, {#{'B' := [{keyword, _, [source, test, file, t], 'B'}]}, _}}, Actual)].

empty_module_import_test_() ->
    Code = "module test (def boolean -> True | False)",
    ModuleMap = module_map("test/file.lark", Code),
    #{[source, test, file] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    [?test({ok, {#{}, []}}, Actual)].

root_import_in_module_test_() ->
    Code = "import beam/erlang/<
            module test (def lt a b -> a < b)",
    ModuleMap = module_map("test/file.lark", Code),
    #{[test] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    [?test({ok, {#{'<' := [{beam_symbol, _, [erlang], '<'}]}, []}}, Actual)].

overloaded_imports_test_() ->
    Code = "module test (import beam/lists/map
                         import beam/maps/map)",
    ModuleMap = module_map("test/file.lark", Code),
    #{[test] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    [?test({ok, {#{'map' := [{beam_symbol, _, [lists], map},
                             {beam_symbol, _, [maps], map}]}, []}}, Actual)].
    

