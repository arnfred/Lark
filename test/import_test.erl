-module(import_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

module_map(FileName, Code) -> 
    {ok, {FileName, AST}} = parser:to_ast(FileName, Code),
    {ok, Modules} = module:parse([{FileName, AST}]),
    Modules.

import(Path) -> {import, #{}, symbols(Path)}.

symbols(Symbols) -> symbols(Symbols, []).
symbols([], Out) -> lists:reverse(Out);
symbols([S | Symbols], Out) when is_atom(S) -> symbols(Symbols, [{symbol, #{}, variable, S} | Out]);
symbols([S | Symbols], Out) when is_map(S) -> 
    symbols(Symbols, [{dict, #{}, [{pair, #{},
                                   {symbol, #{}, variable, K},
                                   {symbol, #{}, variable, V}} || {K, V} <- maps:to_list(S)]} | Out]).

empty_module() -> {module, #{}, [test_module], [], #{}, #{}, #{}}.

test_import(ImportPath, ModuleMap) ->
	import:import({import, #{}, symbols(ImportPath)}, empty_module(), ModuleMap).

erlang_import_test_() ->
    Actual = test_import([erlang, atom_to_list], #{}),
    ?test({ok, [{alias, _, atom_to_list, {qualified_symbol, _, [erlang], atom_to_list}}]}, Actual).

erlang_module_import_test_() ->
    Actual = test_import([lists, reverse], #{}),
    ?test({ok, [{alias, _, reverse, {qualified_symbol, _, [lists], reverse}}]}, Actual).

source_import_test_() ->
    Code = "module test/mod {test_fun} (def test_fun -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([test_mod, test_fun], ModuleMap),
    ?test({ok, [{dependency, _, [test,mod]},
                {alias, _, test_fun, {qualified_symbol, _, [test,mod], test_fun}}]}, Actual).

beam_wildcard_test_test() ->
    Actual = test_import([random, '_'], #{}),
    ?test({ok, [{alias, _, seed0, {qualified_symbol, _, [random], seed0}},
                {alias, _, seed, {qualified_symbol, _, [random], seed}},
                {alias, _, uniform, {qualified_symbol, _, [random], uniform}},
                {alias, _, uniform_s, {qualified_symbol, _, [random], uniform_s}}]}, Actual).

source_wildcard_test_() ->
    Code = "module blap {fun1, fun2, fun3} (
                def fun1 -> noop
                def fun2 -> noop
                def fun3 -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([blap, '_'], ModuleMap),
    ?test({ok, [{dependency, _, [blap]},
                {alias, _, fun1, {qualified_symbol, _, [blap], fun1}},
                {alias, _, fun2, {qualified_symbol, _, [blap], fun2}},
                {alias, _, fun3, {qualified_symbol, _, [blap], fun3}}]}, Actual).

beam_dict_test_() ->
    Actual = test_import([random, #{seed => glunk, uniform => uniform}], #{}),
    ?test({ok, [{alias, _, glunk, {qualified_symbol, _, [random], seed}},
                {alias, _, uniform, {qualified_symbol, _, [random], uniform}}]}, Actual).

source_dict_test_() ->
    Code = "module blap {fun1, fun2, fun3} (
                def fun1 -> noop
                def fun2 -> noop
                def fun3 -> noop)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([blap, #{fun1 => blarg, fun2 => fun2}], ModuleMap),
    ?test({ok, [{dependency, _, [blap]},
                {alias, _, blarg, {qualified_symbol, _, [blap], fun1}},
                {dependency, _, [blap]},
                {alias, _, fun2, {qualified_symbol, _, [blap], fun2}}]}, Actual).

module_type_test_() ->
    Code = "type Blup -> A",
    ModuleMap = module_map("blap", Code),
    Actual = import:import(import([blap, 'Blup']), empty_module(), ModuleMap),
    ?test({ok, [{alias, _, 'Blup', {qualified_symbol, _, ['blap'], 'Blup'}},
                {dependency, _, [blap]},
                {alias, _, 'Blup/A', {qualified_symbol, _, ['blap', 'Blup'], 'A'}},
                {dependency, _, [blap, 'Blup']}]}, Actual).

module_sub_type_test_() ->
    Code = "type Blup -> A",
    ModuleMap = module_map("blap", Code),
    Actual = import:import(import([blap, 'Blup', 'A']), empty_module(), ModuleMap),
    ?test({ok, [{dependency, _, [blap, 'Blup']},
                {alias, _, 'A', {qualified_symbol, _, ['blap', 'Blup'], 'A'}}]}, Actual).

wildcard_type_test_() ->
    Code = "type Blup -> (A | B)",
    ModuleMap = module_map("blap", Code),
    Actual = import:import(import([blap, 'Blup', '_']), empty_module(), ModuleMap),
    ?test({ok, [{dependency, _, [blap, 'Blup']},
                       {alias, _, 'A', {qualified_symbol, _, ['blap', 'Blup'], 'A'}},
                       {alias, _, 'B', {qualified_symbol, _, ['blap', 'Blup'], 'B'}}]}, Actual).


local_type_test_() ->
    Code = "type Blup -> (A | B)",
    ModuleMap = module_map("blap", Code),
    #{blap := Mod} = ModuleMap,
    Actual = import:import(import(['Blup', 'A']), Mod, ModuleMap),
    ?test({ok, [{alias, _, 'A', {qualified_symbol, _, [blap, 'Blup'], 'A'}}]}, Actual).

local_type_underscore_test_() ->
    Code = "type Blup -> (A | B)",
    ModuleMap = module_map("blap", Code),
    #{blap := Mod} = ModuleMap,
    Actual = import:import(import(['Blup', '_']), Mod, ModuleMap),
    ?test({ok, [{alias, _, 'A', {qualified_symbol, _, [blap, 'Blup'], 'A'}},
                {alias, _, 'B', {qualified_symbol, _, [blap, 'Blup'], 'B'}}]}, Actual).

transitive_local_type_test_() ->
    Code = "type Blup -> (A | B)
            module test_module {} (import Blup/A)",
    ModuleMap = module_map("root_module", Code),
    #{test_module := {module, _, _, Imports, _, _, _} = Mod} = ModuleMap,
    Actual = import:import(lists:last(Imports), Mod, ModuleMap),
    ?test({ok, [{alias, _, 'A', {qualified_symbol, _, [root_module, 'Blup'], 'A'}}]}, Actual).

transitive_path_overlap_test_() ->
    Code = "module a/b/c {T} (type T -> A)
            import a/b/c
            import c/T",
    ModuleMap = module_map("test_module", Code), 
    #{test_module := {module, _, _, Imports, _, _, _} = Mod} = ModuleMap,
    Actual = import:import(lists:last(Imports), Mod, ModuleMap),
    ?test({ok, [{alias, _, 'T', {qualified_symbol, _, [a, b, c], 'T'}},
                {alias, _, 'T/A', {qualified_symbol, _, [a, b, c, 'T'], 'A'}}]}, Actual).

ambigious_transitive_local_type_test_() ->
    Code = "module test/module1 {Blup} (type Blup -> (A | B))
            module test/module2 {Blup} (type Blup -> (C | D))
            module test/module3 {} (import test/module1/_
                                    import test/module2/_
                                    import Blup)",
    ModuleMap = module_map("test_file", Code),
    #{test_module3 := {module, _, _, Imports, _, _, _} = Mod} = ModuleMap,
    Actual = import:import(lists:last(Imports), Mod, ModuleMap),
    ?testError({multiple_transitive_import_candidates, ['test/module1/Blup', 'test/module2/Blup']}, Actual).

errors_test_() ->
    Code = "type Blup -> A",
    ModuleMap = module_map("blap", Code),
    [?_errorMatch({empty_import},
                  import:import({import, #{}, []}, empty_module(), #{})),
     ?_errorMatch({import_underscore_for_alias, blah},
                  import:import({import, #{}, [{symbol, #{}, variable, 'blap'},
                                               {dict, #{}, [{pair, #{},
                                                             {symbol, #{}, variable, 'blah'},
                                                             {symbol, #{}, variable, '_'}}]}]},
                                empty_module(), ModuleMap)),
     ?_errorMatch({import_underscore_for_name, blah},
                  import:import({import, #{}, [{symbol, #{}, variable, 'blap'},
                                               {dict, #{}, [{pair, #{},
                                                             {symbol, #{}, variable, '_'},
                                                             {symbol, #{}, variable, 'blah'}}]}]},
                                empty_module(), ModuleMap)),
     ?_errorMatch({import_def_alias_for_type, a, 'A'},
                  import:import({import, #{}, [{symbol, #{}, variable, blap},
                                        {dict, #{}, [{pair, #{},
                                                      {symbol, #{}, type, 'A'},
                                                      {symbol, #{}, variable, a}}]}]},
                                empty_module(), #{})),
     ?_errorMatch({import_type_alias_for_def, 'A', a},
                  import:import({import, #{}, [{symbol, #{}, variable, blap},
                                        {dict, #{}, [{pair, #{},
                                                      {symbol, #{}, variable, a},
                                                      {symbol, #{}, type, 'A'}}]}]},
                                empty_module(), #{})),
     ?_errorMatch({unrecognized_dict_import, {blup}},
                  import:import({import, #{}, [{symbol, #{}, variable, blap},
                                        {dict, #{}, [{blup}]}]}, empty_module(), #{})),
     ?_errorMatch({unrecognized_import, [{blip}]},
                  import:import({import, #{}, [{blip}]}, empty_module(), #{})),
     ?_errorMatch({nonexistent_module, blap},
                  test_import([blap, blip], #{})),
     ?_errorMatch({nonexistent_import, beam, 'random/blip'},
                  test_import([random, blip], #{})),
     ?_errorMatch({nonexistent_import, source, 'blap/blip'},
                  test_import([blap, blip], ModuleMap))].

sandbox_test_() ->
    Options = #{sandboxed => true},
	[?testError({function_not_whitelisted, timer, exit_after},
				import:import({import, #{}, symbols(['timer', 'exit_after'])}, empty_module(), #{}, Options)),
	 ?testError({function_not_whitelisted, filelib, is_dir},
				import:import({import, #{}, symbols([filelib, is_dir])}, empty_module(), #{}, Options)),
	 ?test({ok, _},
		   import:import({import, #{}, symbols([lists, reverse])}, empty_module(), #{}, Options))].

import_qualified_module_name_test_() ->
    Code = "module kind/prelude {Option} (type Option -> Option)",
    ModuleMap = module_map("test_file", Code),
    Actual = test_import([kind, prelude], ModuleMap),
    [?test({ok, [{alias, _,
                      'prelude/Option',
                      {qualified_symbol, _, [kind, prelude], 'Option'}},
                 {dependency, _, [kind, prelude]}]}, Actual)].
