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
    ?test({ok, {#{atom_to_list := {beam_symbol, _, [erlang], atom_to_list}}, []}}, Actual).

erlang_module_import_test_() ->
    Actual = test_import([beam, lists], #{}),
    ?test({ok, {#{reverse := {beam_symbol, _, [lists], reverse}}, []}}, Actual).

source_import_test_() ->
    Code = "module test/mod {test_fun} (def test_fun -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([test, mod, test_fun], ModuleMap),
    ?test({ok, {#{test_fun := {qualified_symbol, _, [test,mod], test_fun}},
                [{dependency, _, [test_module], [test, mod]}]}}, Actual).

source_import_sub_path_test_() ->
    Code = "module a/b/c/d {test_fun} (def test_fun -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([a, b, c], ModuleMap),
    ?test({ok, {#{'c/d/test_fun' := {qualified_symbol, _, [a, b, c, d], test_fun}},
                [{dependency, _, [test_module], [a, b, c, d]}]}}, Actual).

source_import_sub_path_alias_test_() ->
    Code = "module a/b/c/d {test_fun} (def test_fun -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([a, b, #{c => alias}], ModuleMap),
    ?test({ok, {#{'alias/d/test_fun' := {qualified_symbol, _, [a, b, c, d], test_fun}},
                [{dependency, _, [test_module], [a, b, c, d]}]}}, Actual).

source_import_sub_path_multi_mod_test_() ->
    Code = "module a/b/c/d {test_fun} (def test_fun -> noop)
            module a/b/c/other {qworp} (def qworp -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([a, b, c], ModuleMap),
    ?test({ok, {#{'c/d/test_fun' := {qualified_symbol, _, [a, b, c, d], test_fun},
                  'c/other/qworp' := {qualified_symbol, _, [a, b, c, other], qworp}},
                [{dependency, _, [test_module], [a, b, c, d]},
                 {dependency, _, [test_module], [a, b, c, other]}]}}, Actual).


beam_wildcard_test_() ->
    Actual = test_import([beam, random, '_'], #{}),
    ?test({ok, {#{seed0 := {beam_symbol, _, [random], seed0},
                  seed := {beam_symbol, _, [random], seed},
                  uniform := {beam_symbol, _, [random], uniform},
                  uniform_s := {beam_symbol, _, [random], uniform_s}}, []}}, Actual).

source_wildcard_test_() ->
    Code = "module blap {fun1, fun2, fun3} (
                def fun1 -> noop
                def fun2 -> noop
                def fun3 -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([blap, '_'], ModuleMap),
    ?test({ok, {#{fun1 := {qualified_symbol, _, [blap], fun1},
                  fun2 := {qualified_symbol, _, [blap], fun2},
                  fun3 := {qualified_symbol, _, [blap], fun3}},
               [{dependency, _, [test_module], [blap]}]}}, Actual).

beam_dict_test_() ->
    Actual = test_import([beam, random, #{seed => glunk, uniform => uniform}], #{}),
    ?test({ok, {#{glunk := {beam_symbol, _, [random], seed},
                  uniform := {beam_symbol, _, [random], uniform}}, []}}, Actual).

source_dict_test_() ->
    Code = "module blap {fun1, fun2, fun3} (
                def fun1 -> noop
                def fun2 -> noop
                def fun3 -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([blap, #{fun1 => blarg, fun2 => fun2}], ModuleMap),
    ?test({ok, {#{blarg := {qualified_symbol, _, [blap], fun1},
                  fun2 := {qualified_symbol, _, [blap], fun2}},
                [{dependency, _, [test_module], [blap]}]}}, Actual).

module_keyword_test_() ->
    Code = "def blup -> A",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([source, blap, 'blup'], ModuleMap),
    ?test({ok, {#{'blup' := {qualified_symbol, _, [source, 'blap'], 'blup'},
                  'blup/A' := {keyword, _, [source, 'blap', blup], 'A'}},
                [{dependency, _, [test_module], [source, blap]},
                 {dependency, _, [test_module], [source, blap, blup]}]}}, Actual).

module_sub_keyword_test_() ->
    Code = "def blup -> A",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([source, blap, 'blup', 'A'], ModuleMap),
    ?test({ok, {#{'A' := {keyword, _, [source, 'blap', blup], 'A'}},
                [{dependency, _, [test_module], [source, blap, blup]}]}}, Actual).

wildcard_keyword_test_() ->
    Code = "def blup -> (A | B)",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([source, blap, 'blup', '_'], ModuleMap),
    ?test({ok, {#{'A' := {keyword, _, [source, 'blap', blup], 'A'},
                  'B' := {keyword, _, [source, 'blap', blup], 'B'}},
               [{dependency, _, [test_module], [source, blap, blup]}]}}, Actual).

transitive_local_keyword_test_() ->
    Code = "def blup -> (A | B)
            module test_module {} (import blup/A)",
    ModuleMap = module_map("root_module", Code),
    #{[test_module] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := {keyword, _, [source, root_module, blup], 'A'},
                  'blup' := {qualified_symbol, _, [source, root_module], 'blup'},
                  'blup/A' := {keyword, _, [source, root_module, blup], 'A'},
                  'blup/B' := {keyword, _, [source, root_module, blup], 'B'}},
                [{dependency, _, [test_module], [source, root_module]},
                 {dependency, _, [test_module], [source, root_module, blup]}]}}, Actual).

transitive_local_keyword_dict_import_test_() ->
    Code = "module test/module1 {t, s} (def t -> A
                                        def s -> B)
            module test/module2 {} (import test/module1/{t: q, s}
                                    import s/_
                                    import q/_)",
    ModuleMap = module_map("root_module", Code),
    #{[test, module2] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := {keyword, _, [test, module1, t], 'A'},
                  'B' := {keyword, _, [test, module1, s], 'B'},
                  'q' := {qualified_symbol, _, [test, module1], 't'},
                  'q/A' := {keyword, _, [test, module1, t], 'A'},
                  's' := {qualified_symbol, _, [test, module1], 's'},
                  's/B' := {keyword, _, [test, module1, s], 'B'}},
                [{dependency, _, [test,module2], [test, module1]},
                 {dependency, _, [test,module2], [test, module1, s]},
                 {dependency, _, [test,module2], [test, module1, t]}]}}, Actual).

transitive_path_overlap_test_() ->
    Code = "module a/b/c {t} (def t -> A)
            import a/b/c
            import c/t",
    ModuleMap = module_map("test_module", Code), 
    #{[source, test_module] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'t' := {qualified_symbol, _, [a, b, c], 't'},
                  't/A' := {keyword, _, [a, b, c, t], 'A'}},
                [{dependency, _, [source, test_module], [a, b, c]},
                 {dependency, _, [source, test_module], [a, b, c, t]}]}}, Actual).

ambigious_transitive_local_keyword_test_() ->
    Code = "module test/module1 {blup} (def blup -> (A | B))
            module test/module2 {blup} (def blup -> (C | D))
            module test/module3 {} (import test/module1/_
                                    import test/module2/_
                                    import blup)",
    ModuleMap = module_map("test_file", Code),
    #{[test, module3] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?testError({duplicate_import, 'blup', 'test/module3', 'test/module2/blup', 'test/module1/blup'}, Actual).

qualified_export_test_() ->
    Code = "module test/module1 {t/A} (def t -> (A | B))
            module test/module2 {} (import test/module1/_)",
    ModuleMap = module_map("test_file", Code),
    #{[test, module2] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := {keyword, _, [test, module1, t], 'A'}},
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

import_qualified_module_name_test_() ->
    Code = "module kind/prelude {option} (def option -> Option)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([kind, prelude], ModuleMap),
    [?test({ok, {#{'prelude/option' := {qualified_symbol, _, [kind, prelude], 'option'},
                   'prelude/option/Option' := {keyword, _, [kind, prelude, option], 'Option'}},
                 [{dependency, _, [test_module], [kind, prelude]},
                  {dependency, _, [test_module], [kind, prelude, option]}]}}, Actual)].

link_def_test_() ->
    Code = "def f -> noop
            module t {f}",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([t, f], ModuleMap),
    [?test({ok, {#{f := {qualified_symbol, _, [source, test_file], 'f'}},
                 [{dependency, _, [test_module], [source, test_file]}]}}, Actual)].


link_sub_def_test_() ->
    Code = "def t -> (A | B)
            module test {t}",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([test, 't'], ModuleMap),
    [?test({ok, {#{'t' := {qualified_symbol, _, [source, test_file], 't'},
                   't/A' := {keyword, _, [source, test_file, t], 'A'},
                   't/B' := {keyword, _, [source, test_file, t], 'B'}},
                 [{dependency, _, [test_module], [source, test_file]},
                  {dependency, _, [test_module], [source, test_file, t]}]}}, Actual)].

nested_link_test_() ->
    Code = "def blup -> (Blip | Blap)
            import blup/_
            def flup -> (Flip | Blap)
            import flup/{Flip, Blap: Blop}
            def blonk -> (Blank | Blop)
            module t {blonk}",
    ModuleMap = module_map("test/file.kind", Code),
    Actual = test_import([t, 'blonk'], ModuleMap),
    [?test({ok, {#{'blonk' := {qualified_symbol, _, [source, test, file], 'blonk'},
                  'blonk/Blank' := {keyword, _, [source, test, file, blonk], 'Blank'},
                  'blonk/Blop' := {keyword, _, [source, test, file, blup], 'Blap'}},
                 [{dependency, _, [test_module], [source, test, file]},
                  {dependency, _, [test_module], [source, test, file, blonk]},
                  {dependency, _, [test_module], [source, test, file, blup]}]}}, Actual)].


local_constant_test_() ->
    Code = "module test {s, r} (def t -> A
                                import t/A
                                def s -> A
                                def r -> s/A)",
    ModuleMap = module_map("test/file.kind", Code),
    Actual = test_import([test, 's', 'A'], ModuleMap),
    [?test({ok, {#{'A' := {keyword, _, [test, t], 'A'}},
                [{dependency, _, [test_module], [test, t]}]}}, Actual)].

nested_module_import_conflict_test_() ->
    Code = "module test {boolean} (def boolean -> True | False)
            def t test/boolean -> test/boolean/True",
    ModuleMap = module_map("test/file.kind", Code),
    Actual = import:import(maps:get([test], ModuleMap), ModuleMap),
    [?test({ok, {#{'t' := {qualified_symbol, _, [source, test, file], 't'}},
                 [{dependency, _, [test], [source, test, file]}]}}, Actual)].
