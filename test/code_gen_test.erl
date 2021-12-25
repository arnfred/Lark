-module(code_gen_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(setup(Code, Tests), {setup, fun() -> lark:load(Code, #{target => [lark, test]}) end, fun clean/1, Tests}).

clean({error, _}) -> noop;
clean({ok, Module}) -> true = code:soft_purge(Module),
                       true = code:delete(Module).


identity_run_test_() ->
    {"define and call the identity function",
     ?setup("module lark/test (export {id}
                               def id a -> a)",
            fun({ok, _}) ->
                    [?test(2, lark_test:id(2)),
                     ?test(1.3, lark_test:id(1.3)),
                     ?test("string", lark_test:id("string")),
                     ?test(atom, lark_test:id(atom))]
            end)}.

always_true_test_() ->
    ?setup("module lark/test (export { alwaysTrue }
                              def alwaysTrue a -> True)",
           fun({ok, _}) -> ?test('lark/prelude/boolean/True', lark_test:alwaysTrue(2)) end).

pattern_match_test_() ->
    ?setup("module lark/test (export { rexor }
                              def rexor
                                True -> False,
                                False -> True)",
           fun({ok, _}) ->
                   [?test('lark/prelude/boolean/True', lark_test:rexor('lark/prelude/boolean/False')),
                    ?test('lark/prelude/boolean/False', lark_test:rexor('lark/prelude/boolean/True'))]
           end).

pattern_match_multivariate_test_() ->
    ?setup("module lark/test (export { rexor }
                              def rexor
                                True False -> True,
                                False True -> True,
                                _ _ -> False)",
           fun({ok, _}) ->
                   [?test('lark/prelude/boolean/True', lark_test:rexor('lark/prelude/boolean/True', 'lark/prelude/boolean/False')),
                    ?test('lark/prelude/boolean/False', lark_test:rexor('lark/prelude/boolean/True', 'lark/prelude/boolean/True'))]
           end).

pattern_match3_test_() ->
    ?setup("module lark/test (export { blah }
                              def blah
                                b -> b)",
           fun({ok, _}) ->
                   ?test('lark/prelude/boolean/True', lark_test:blah('lark/prelude/boolean/True'))
           end).

pattern_match_dict_test_() ->
    ?setup("module lark/test (
              export { blah }
              def blah
                {key1: True, key2: k2} -> k2,
                {key1: False, key2: Nil} -> Nil)",
           fun({ok, _}) ->
                   [?test('lark/prelude/boolean/False', lark_test:blah(#{key1 => 'lark/prelude/boolean/True', key2 => 'lark/prelude/boolean/False'})),
                    ?test('lark/prelude/option/Nil', lark_test:blah(#{key1 => 'lark/prelude/boolean/False', key2 => 'lark/prelude/option/Nil'}))]
           end).

underscore_arg_test_() ->
    ?setup("module lark/test (export { blip }
                              def blip _ _ c -> c)",
           fun({ok, _}) -> ?test(blop, lark_test:blip(blip, blab, blop)) end).

first_order_function_test_() ->
    TestFunction = fun('lark/prelude/boolean/True') -> 'lark/prelude/boolean/False' end,
    ?setup("module lark/test (
              export { blip }
              def blip f -> f(True))",
           fun({ok, _}) -> ?test('lark/prelude/boolean/False', lark_test:blip(TestFunction)) end).

erlang_module_call_test_() ->
    ?setup("module lark/test (export { get_last }
                              import beam/lists/last
                              def get_last l -> l.last)",
           fun({ok, _}) -> ?test('last_item', lark_test:get_last(['first_item', 'last_item'])) end).

erlang_module_call_erlang_test_() ->
    ?setup("module lark/test (export { test }\n"
           "import beam/erlang/atom_to_list\n"
           "def test a -> a.atom_to_list)",
           fun({ok, _}) -> ?test("an_atom", lark_test:test('an_atom')) end).

erlang_module_call_no_dot_notation_test_() ->
    ?setup("module lark/test (export { test }\n"
           "import beam/erlang\n"
           "def test a -> erlang/atom_to_list(a))",
           fun({ok, _}) -> ?test("another_atom", lark_test:test('another_atom')) end).

top_level_import_test_() ->
    {"Test if we can use lark/prelude/boolean as defined in the prelude",
     ?setup("module lark/test (export { test }\n"
            "def test -> (boolean() | Maybe))",
            fun({ok, _}) -> 
                    ?testEqual({sum, ordsets:from_list(['lark/test/test/Maybe', 'lark/prelude/boolean/True', 'lark/prelude/boolean/False'])}, lark_test:'test'())
            end)}.

sup_level_import_test_() ->
    {"Test if we can use lark/prelude/boolean as defined in the prelude",
     ?setup("module lark/test (export { test }
                               def test -> (True | Maybe))",
            fun({ok, _}) -> 
                    ?testEqual({sum, ordsets:from_list(['lark/test/test/Maybe', 'lark/prelude/boolean/True'])}, lark_test:'test'())
            end)}.

unicode_string_test_() ->
    {"Support for string expressions in utf8",
     ?setup("module lark/test (export { main }
                          def main -> \"strings! ❤️\")",
            fun({ok, _}) ->
                    ?testEqual(<<"strings! ❤️"/utf8>>, lark_test:main())
            end)}.

unicode_string_pattern_test_() ->
    {"Support for string patterns in utf8",
     ?setup("module lark/test (export { main }
                               def main
                                 \"❤️\" -> True,
                                 _ -> False)",
            fun({ok, _}) ->
                    ?test('lark/prelude/boolean/True', lark_test:main(<<"❤️"/utf8>>))
            end)}.

number_test_() ->
    {"Support for number expressions",
     ?setup("module lark/test (export { main }
                               def main -> 3.14)",
            fun({ok, _}) ->
                    ?test(3.14, lark_test:main())
            end)}.

number_pattern_test_() ->
    {"Support for numbers in patterns",
     ?setup("module lark/test (export { main }
                          def main
                            3.14 -> True,
                            _ -> False)",
            fun({ok, _}) ->
                    ?test('lark/prelude/boolean/True', lark_test:main(3.14))
            end)}.

atom_test_() ->
    {"Support for atoms",
     ?setup("module lark/test (export { main }
                               def main -> 'atom'.match(atom -> atom))",
            fun({ok, _}) ->
                    ?test(atom, lark_test:main())
            end)}.

atom_pattern_test_() ->
    {"Support for atoms in patterns",
     ?setup("module lark/test (export { main }
                               def main
                                 'atom' -> True,
                                 _ -> False)",
            fun({ok, _}) ->
                    ?test('lark/prelude/boolean/True', lark_test:main(atom))
            end)}.

list_test_() ->
    {"Support for lists",
     ?setup("module lark/test (export { main }
                               def main -> [1, 2, 3])",
            fun({ok, _}) ->
                    ?test([1, 2, 3], lark_test:main())
            end)}.

list_pattern_test_() ->
    {"Support for lists in patterns",
     ?setup("module lark/test (export { main }
                               def main
                                 [1, 2, b] -> b,
                                 a         -> a)",
            fun({ok, _}) ->
                    ?test(3, lark_test:main([1, 2, 3]))
            end)}.

unexported_type_test_() ->
    {"A def within a local file can call all types defined in the module",
     ?setup("module lark/test (export { main }
                               def test -> {a: A, b: B}
                               def main -> test)",
            fun({ok, _}) ->
                    [?testEqual(#{a => 'lark/test/test/A', b => 'lark/test/test/B'}, lark_test:main())]
            end)}.
             
type_parameter_pattern_test_() ->
    {"A def within a local file can call all types defined in the module",
     ?setup("module lark/test (export { main }\n"
            "def test -> option(boolean)\n"
            "def main\n"
            "   test -> True,\n"
            "   _ -> False)",
            fun({ok, _}) ->
                    [?test('lark/prelude/boolean/True', lark_test:main('lark/prelude/option/Nil')),
                     ?test('lark/prelude/boolean/True', lark_test:main('lark/prelude/boolean/True')),
                     ?test('lark/prelude/boolean/True', lark_test:main('lark/prelude/boolean/False')),
                     ?test('lark/prelude/boolean/False', lark_test:main('_'))]
            end)}.
             
unexported_type_availability_test_() ->
    {"A type which isn't exported in a module shouldn't be callable from
      outside the module. Same is true for the domain module.",
     ?setup("module lark/test (export { exported }\n"
            "def exported -> True\n"
            "def unExported -> False)",
            fun({ok, _}) ->
                    [?test('lark/prelude/boolean/True', lark_test:'exported'()),
                     ?test(false, lists:member({'unExported', 0}, proplists:get_value(exports, erlang:get_module_info(lark_test))))]
            end)}.

tagged_constructor_test_() ->
    {"To construct tagged values we can call a tagged type constructor",
     ?setup("module lark/test (export { test/T, main }
             def test -> (T: ({a: boolean, b: 4} | {c: boolean, d: boolean}))
             def main -> test/T({c: False, d: True}))",
            fun({ok, _}) ->
                    [?test(#{c := 'lark/prelude/boolean/False', d := 'lark/prelude/boolean/True'},
                           lark_test:main())]
            end)}.

assignment_test_() ->
    {"Assigning to variable pattern should let those variables be accessible in subsequent expressions",
     ?setup("module lark/test (export { main }
             def main a -> (val [1,2,b] = a
                            [b, b, b]))",
            fun({ok, _}) ->
                    [?test([3, 3, 3], lark_test:main([1, 2, 3]))]
            end)}.

sum_assignment_test_() ->
    {"Assigning to variable pattern should let those variables be accessible in subsequent expressions",
     ?setup("module lark/test (export { main }
             def main a -> (val (b: boolean) = a
                            b))",
            fun({ok, _}) ->
                    [?test('lark/prelude/boolean/True', lark_test:main('lark/prelude/boolean/True')),
                     ?test('lark/prelude/boolean/False', lark_test:main('lark/prelude/boolean/False')),
                     ?testError({no_matching_pattern, [not_a_boolean]}, lark_test:main('not_a_boolean'))]
            end)}.

seq_test_() ->
    {"Two expressions in successions should be evaluated in order",
     ?setup("module lark/test (export { main }
             def main a -> (val f = (fn b -> [b, b])
                            f(a)
                            [a, a, a]))",
            fun({ok, _}) ->
                    [?test([1, 1, 1], lark_test:main(1))]
            end)}.

param_pattern_test_() ->
    {"Defs accept patterns for parameters",
     ?setup("module lark/test (export { main }
             import beam/erlang/+
             def main {key1: a, key2: b} -> a + b)",
            fun({ok, _}) ->
                    [?test(3, lark_test:main(#{key1 => 1, key2 => 2}))]
            end)}.

tuple_test_() ->
    {"tuple support in lark",
     ?setup("module lark/test (export { main }
                 def main #(a, b) -> #(b, a))",
            fun({ok, _}) ->
                    [?test({2, 1}, lark_test:main({1, 2}))]
            end)}.
