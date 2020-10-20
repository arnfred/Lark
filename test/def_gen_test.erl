-module(def_gen_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(setup(Code, Tests), {setup, fun() -> kind:load(Code, #{sandboxed => false}) end, fun clean/1, Tests}).

clean({error, _}) -> noop;
clean({ok, Modules}) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].


identity_run_test_() ->
    {"define and call the identity function",
     ?setup("module kind/test { id }\n"
            "def id a -> a",
            fun({ok, _}) ->
                    [?test(2, kind_test:id(2)),
                     ?test(1.3, kind_test:id(1.3)),
                     ?test("string", kind_test:id("string")),
                     ?test(atom, kind_test:id(atom))]
            end)}.

function_call_test_() ->
        ?setup("module kind/test { callId }\n"
               "def id a -> a\n"
               "def callId b -> b.id",
               fun({ok, _}) -> ?test(2, kind_test:callId(2)) end).

function_def_newline_test_() ->
        ?setup("module kind/test { id }\n"
               "def id b ->\n"
               "    b",
               fun({ok, _}) -> ?test(2, kind_test:id(2)) end).

function_call_multiple_args_test_() ->
    ?setup("module kind/test { callId }\n"
           "def firstId a b c -> a\n"
           "def callId a b -> b.firstId(b, a)",
           fun({ok, _}) -> ?test(3, kind_test:callId(2, 3)) end).

always_true_test_() ->
    ?setup("module kind/test { alwaysTrue }\n"
           "def alwaysTrue a -> True",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:alwaysTrue(2)) end).

pattern_match_test_() ->
    ?setup("module kind/test { rexor }\n"
           "def rexor a\n"
           " | True -> False\n"
           " | False -> True",
           fun({ok, _}) ->
                   [?test('Boolean/True', kind_test:rexor('Boolean/False')),
                   ?test('Boolean/False', kind_test:rexor('Boolean/True'))]
           end).

pattern_match_multivariate_test_() ->
    ?setup("module kind/test { rexor }\n"
           "def rexor a b\n"
           " | True False -> True\n"
           " | False True -> True\n"
           " | _ _ -> False",
           fun({ok, _}) ->
                   [?test('Boolean/True', kind_test:rexor('Boolean/True', 'Boolean/False')),
                    ?test('Boolean/False', kind_test:rexor('Boolean/True', 'Boolean/True'))]
           end).

pattern_match_expr_syntax1_test_() ->
    ?setup("module kind/test { test2 }\n"
           "def test2 a -> a.match(False -> True, True -> False)",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:test2('Boolean/False'))
           end).

pattern_match_expr_syntax2_test_() ->
    ?setup("module kind/test { test3 }\n"
           "def test3 a -> a.match(False -> True\n"
           "                       True -> False)",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:test3('Boolean/False'))
           end).

pattern_match_expr_syntax3_test_() ->
    ?setup("module kind/test { test4 }\n"
           "def test4 a -> a.match(\n"
           "    False -> True\n"
           "    True -> False)",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:test4('Boolean/False'))
           end).

pattern_match3_test_() ->
    ?setup("module kind/test { blah }\n"
           "def blah a\n"
           " | b -> b",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:blah('Boolean/True'))
           end).

pattern_match_type_test_() ->
    ?setup("module kind/test{ blah }\n"
           "type T -> {key1: Boolean, key2: Option(Boolean)}\n"
           "def blah a\n"
           " | {key1: True, key2: k2} -> k2\n"
           " | {key1: False, key2: Nil} -> Nil\n",
           fun({ok, _}) ->
                   [?test('Boolean/False', kind_test:blah(#{key1 => 'Boolean/True', key2 => 'Boolean/False'})),
                    ?test('Option/Nil', kind_test:blah(#{key1 => 'Boolean/False', key2 => 'Option/Nil'}))]
           end).


underscore_arg_test_() ->
    ?setup("module kind/test { blip }\n"
           "def blip _ _ c -> c",
           fun({ok, _}) -> ?test(blop, kind_test:blip(blip, blab, blop)) end).

anonymous_function1_test_() ->
    TestFunction = fun('Boolean/True') -> 'Boolean/False' end,
    ?setup("module kind/test { blip }\n"
           "def blip f -> f(True)",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blip(TestFunction)) end).

anonymous_function2_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a)\n"
           "def blap a -> a.blip(False -> True\n"
           "                      True -> False)",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blap('Boolean/True')) end).

anonymous_function3_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a)\n"
           "def blap a -> a.blip((arg -> arg))",
           fun({ok, _}) -> ?test(whatevs, kind_test:blap(whatevs)) end).

anonymous_function4_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a, a)\n"
           "def blap a -> a.blip(arg1 False -> False\n"
           "                     arg1 True  -> arg1)",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:blap('Boolean/True')) end).

multiple_anonymous_functions1_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f g -> f(g(a))\n"
           "def blap a -> a.blip((_ -> False),\n"
           "                     (False -> True\n"
           "                      True -> False))",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blap('Boolean/True')) end).

multiple_anonymous_functions2_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f g -> f(g(a))\n"
           "def blap a -> a.blip((True -> False\n"
           "                      False -> True),\n"
           "                     (_ -> False))",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:blap('whatevs')) end).

erlang_module_call_test_() ->
    ?setup("module kind/test { get_last }\n"
           "import lists/last\n"
           "def get_last l -> l.last",
           fun({ok, _}) -> ?test('last_item', kind_test:get_last(['first_item', 'last_item'])) end).

erlang_module_call_erlang_test_() ->
    ?setup("module kind/test { test }\n"
           "import erlang/atom_to_list\n"
           "def test a -> a.atom_to_list",
           fun({ok, _}) -> ?test("an_atom", kind_test:test('an_atom')) end).

erlang_module_call_no_dot_notation_test_() ->
    ?setup("module kind/test { test }\n"
           "import erlang\n"
           "def test a -> erlang/atom_to_list(a)",
           fun({ok, _}) -> ?test("another_atom", kind_test:test('another_atom')) end).

shadow_variable_test_() ->
    {"Test how we handle variable reuse",
     ?setup("module kind/test { test }\n"
            "def test a\n"
            " | a -> a",
            fun(Error) -> 
                    [?testError({symbol_in_pattern_already_defined, a}, Error)]
            end)}.

top_level_type_import_test_() ->
    {"Test if we can use Boolean as defined in the prelude",
     ?setup("module kind/test { Test }\n"
            "type Test -> Boolean | Maybe",
            fun({ok, _}) -> 
                    ?testEqual({sum, ordsets:from_list(['Test/Maybe', 'Boolean/True', 'Boolean/False'])}, kind_test:'Test'())
            end)}.

sup_level_type_import_test_() ->
    {"Test if we can use Boolean as defined in the prelude",
     ?setup("module kind/test { Test }\n"
            "type Test -> True | Maybe",
            fun({ok, _}) -> 
                    ?testEqual({sum, ordsets:from_list(['Test/Maybe', 'Boolean/True'])}, kind_test:'Test'())
            end)}.

unicode_string_test_() ->
    {"Support for string expressions in utf8",
     ?setup("def main _ -> \"strings! ❤️\"",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?testEqual(<<"strings! ❤️"/utf8>>, Mod:main("_"))
            end)}.

unicode_string_pattern_test_() ->
    {"Support for string patterns in utf8",
     ?setup("def main _\n"
            " | \"❤️\" -> True\n"
            " | _ -> False",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?test('Boolean/True', Mod:main(<<"❤️"/utf8>>))
            end)}.

number_test_() ->
    {"Support for number expressions",
     ?setup("def main _ -> 3.14",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?test(3.14, Mod:main("_"))
            end)}.

number_pattern_test_() ->
    {"Support for numbers in patterns",
     ?setup("def main _\n"
            " | 3.14 -> True\n"
            " | _ -> False",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?test('Boolean/True', Mod:main(3.14))
            end)}.

atom_test_() ->
    {"Support for atoms",
     ?setup("def main _ -> 'atom'.match(\n"
            "    atom -> atom)",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?test(atom, Mod:main("_"))
            end)}.

atom_pattern_test_() ->
    {"Support for atoms in patterns",
     ?setup("def main _\n"
            " | 'atom' -> True\n"
            " | _ -> False",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?test('Boolean/True', Mod:main(atom))
            end)}.

list_test_() ->
    {"Support for lists",
     ?setup("def main _ -> [1, 2, 3]",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?test([1, 2, 3], Mod:main('_'))
            end)}.

list_pattern_test_() ->
    {"Support for lists in patterns",
     ?setup("def main a\n"
            " | [1, 2, b] -> b\n"
            " | _         -> a",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    ?test(3, Mod:main([1, 2, 3]))
            end)}.

unexported_type_test_() ->
    {"A def within a local file can call all types defined in the module",
     ?setup("type Test -> {a: A, b: B}\n"
            "def main _ -> Test\n",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    [?testEqual(#{a => 'Test/A', b => 'Test/B'}, Mod:main('_'))]
            end)}.
             
type_parameter_pattern_test_() ->
    {"A def within a local file can call all types defined in the module",
     ?setup("type Test -> Option(Boolean)\n"
            "def main _\n"
            "  | Test -> True\n"
            "  | _ -> False",
            fun({ok, Modules}) ->
                    Mod = lists:last(Modules),
                    [?test('Boolean/True', Mod:main('Option/Nil')),
                     ?test('Boolean/True', Mod:main('Boolean/True')),
                     ?test('Boolean/True', Mod:main('Boolean/False')),
                     ?test('Boolean/False', Mod:main('_'))]
            end)}.
             
unexported_type_availability_test_() ->
    {"A type which isn't exported in a module shouldn't be callable from
      outside the module. All types should be part of the domain function
      though",
     ?setup("module test { Exported }\n"
            "type Exported -> True\n"
            "type UnExported -> False",
            fun({ok, _}) ->
                    [?test('Boolean/True', test:'Exported'()),
                     ?test('Boolean/False', test:domain('UnExported')),
                     ?test(false, lists:member({'Test', 0}, proplists:get_value(exports, erlang:get_module_info(test))))]
            end)}.

tagged_constructor_test_() ->
    {"To construct tagged values we can call a tagged type constructor",
     ?setup("module test { Test/T, main }
             type Test -> T: ({a: Boolean, b: 4} | {c: Boolean, d: Boolean})
             def main _ -> Test/T(False, True)",
            fun({ok, _}) ->
                    [?test({tagged, 'Test/T', #{a := 'Boolean/True', b := 4}},
                           test:'T'('Boolean/True')),
                     ?test({tagged, 'Test/T', #{c := 'Boolean/True', d := 'Boolean/False'}},
                            test:'T'('Boolean/True', 'Boolean/False')),
                     ?test({tagged, 'Test/T', #{c := 'Boolean/False', d := 'Boolean/True'}},
                            test:main('_'))]
            end)}.

assignment_test_() ->
    {"Assigning to variable pattern should let those variables be accessible in subsequent expressions",
     ?setup("module test { main }
             def main a -> (val [1,2,b] = a
                            [b, b, b])",
            fun({ok, _}) ->
                    [?test([3, 3, 3], test:main([1, 2, 3]))]
            end)}.

seq_test_() ->
    {"Two expressions in successions should be evaluated in order",
     ?setup("module test { main }
             def main a -> (val f = (b -> [b, b])
                            f(a)
                            [a, a, a])",
            fun({ok, _}) ->
                    [?test([1, 1, 1], test:main(1))]
            end)}.
