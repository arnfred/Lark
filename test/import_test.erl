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

module_type_test_() ->
    Code = "type Blup -> A",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([source, blap, 'Blup'], ModuleMap),
    ?test({ok, {#{'Blup' := {qualified_symbol, _, [source, 'blap'], 'Blup'},
                  'Blup/A' := {qualified_symbol, _, [source, 'blap'], 'Blup/A'}},
                [{dependency, _, [test_module], [source, blap]}]}}, Actual).

module_sub_type_test_() ->
    Code = "type Blup -> A",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([source, blap, 'Blup', 'A'], ModuleMap),
    ?test({ok, {#{'A' := {qualified_symbol, _, [source, 'blap'], 'Blup/A'}},
               [{dependency, _, [test_module], [source, blap]}]}}, Actual).

wildcard_type_test_() ->
    Code = "type Blup -> (A | B)",
    ModuleMap = module_map("blap", Code),
    Actual = test_import([source, blap, 'Blup', '_'], ModuleMap),
    ?test({ok, {#{'A' := {qualified_symbol, _, [source, 'blap'], 'Blup/A'},
                  'B' := {qualified_symbol, _, [source, 'blap'], 'Blup/B'}},
               [{dependency, _, [test_module], [source, blap]}]}}, Actual).

transitive_local_type_test_() ->
    Code = "type Blup -> (A | B)
            module test_module {} (import Blup/A)",
    ModuleMap = module_map("root_module", Code),
    #{[test_module] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := {qualified_symbol, _, [source, root_module], 'Blup/A'},
                  'Blup' := {qualified_symbol, _, [source, root_module], 'Blup'},
                  'Blup/A' := {qualified_symbol, _, [source, root_module], 'Blup/A'},
                  'Blup/B' := {qualified_symbol, _, [source, root_module], 'Blup/B'}},
                [{dependency, _, [test_module], [source, root_module]}]}}, Actual).

transitive_local_type_dict_import_test_() ->
    Code = "module test/module1 {T, S} (type T -> A
                                        type S -> B)
            module test/module2 {} (import test/module1/{T: Q, S}
                                    import S/_
                                    import Q/_)",
    ModuleMap = module_map("root_module", Code),
    #{[test, module2] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := {qualified_symbol, _, [test, module1], 'T/A'},
                  'B' := {qualified_symbol, _, [test, module1], 'S/B'},
                  'Q' := {qualified_symbol, _, [test, module1], 'T'},
                  'Q/A' := {qualified_symbol, _, [test, module1], 'T/A'},
                  'S' := {qualified_symbol, _, [test, module1], 'S'},
                  'S/B' := {qualified_symbol, _, [test, module1], 'S/B'}},
                [{dependency, _, [test,module2], [test, module1]}]}}, Actual).

transitive_path_overlap_test_() ->
    Code = "module a/b/c {T} (type T -> A)
            import a/b/c
            import c/T",
    ModuleMap = module_map("test_module", Code), 
    #{[source, test_module] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'T' := {qualified_symbol, _, [a, b, c], 'T'},
                  'T/A' := {qualified_symbol, _, [a, b, c], 'T/A'}},
                [{dependency, _, [source, test_module], [a, b, c]}]}}, Actual).

ambigious_transitive_local_type_test_() ->
    Code = "module test/module1 {Blup} (type Blup -> (A | B))
            module test/module2 {Blup} (type Blup -> (C | D))
            module test/module3 {} (import test/module1/_
                                    import test/module2/_
                                    import Blup)",
    ModuleMap = module_map("test_file", Code),
    #{[test, module3] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?testError({duplicate_import, 'Blup', 'test/module3', 'test/module2/Blup', 'test/module1/Blup'}, Actual).

qualified_export_test_() ->
    Code = "module test/module1 {T/A} (type T -> (A | B))
            module test/module2 {} (import test/module1/_)",
    ModuleMap = module_map("test_file", Code),
    #{[test, module2] := Mod} = ModuleMap,
    Actual = import:import(Mod, ModuleMap),
    ?test({ok, {#{'A' := {qualified_symbol, _, [test, module1], 'T/A'}},
               [{dependency, _, [test, module2], [test, module1]}]}}, Actual).

errors_test_() ->
    Code = "type Blup -> A",
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
    Code = "module kind/prelude {Option} (type Option -> Option)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([kind, prelude], ModuleMap),
    [?test({ok, {#{'prelude/Option' := {qualified_symbol, _, [kind, prelude], 'Option'}},
                 [{dependency, _, [test_module], [kind, prelude]}]}}, Actual)].

link_def_test_() ->
    Code = "def f -> noop
            module t {f}",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([t, f], ModuleMap),
    [?test({ok, {#{f := {qualified_symbol, _, [source, test_file], 'f'}},
                 [{dependency, _, [test_module], [source, test_file]}]}}, Actual)].


link_sub_def_test_() ->
    Code = "type T -> (A | B)
            module t {T}",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([t, 'T'], ModuleMap),
    [?test({ok, {#{'T' := {qualified_symbol, _, [source, test_file], 'T'},
                   'T/A' := {qualified_symbol, _, [source, test_file], 'T/A'},
                   'T/B' := {qualified_symbol, _, [source, test_file], 'T/B'}},
                 [{dependency, _, [test_module], [source, test_file]}]}}, Actual)].

nested_link_test_() ->
    Code = "type Blup -> (Blip | Blap)
            import Blup/_
            type Flup -> (Flip | Blap)
            import Flup/{Flip, Blap: Blop}
            type Blonk -> (Blank | Blop)
            module t {Blonk}",
    ModuleMap = module_map("test/file.kind", Code),
    Actual = test_import([t, 'Blonk'], ModuleMap),
    [?test({ok, {#{'Blonk' := {qualified_symbol, _, [source, test, file], 'Blonk'},
                  'Blonk/Blank' := {qualified_symbol, _, [source, test, file], 'Blonk/Blank'},
                  'Blonk/Blop' := {qualified_symbol, _, [source, test, file], 'Blup/Blap'}},
                 [{dependency, _, [test_module], [source, test, file]}]}}, Actual)].


local_constant_test_() ->
    Code = "module t {S, R} (type T -> A
                             import T/A
                             type S -> A
                             type R -> S/A)",
    ModuleMap = module_map("test/file.kind", Code),
    Actual = test_import([t, 'S', 'A'], ModuleMap),
    [?test({ok, {#{'A' := {qualified_symbol, _, [t], 'T/A'}},
                [{dependency, _, [test_module], [t]}]}}, Actual)].
