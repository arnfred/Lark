-module(typecheck_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

-define(setup(Code, Tests), {setup, fun() -> kind:load(Code, #{import_kind_libraries => false}) end, fun clean/1, Tests}).

clean({error, _}) -> noop;
clean({ok, Modules}) -> clean(Modules);
clean(Modules) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].

simple_def_test_() ->
    ?setup("module test { x }
            import erlang/+
            def x (n: 5) -> n + 3",
           fun({ok, _}) ->
                   [?test(none, test_domain:x(lenient, 4)),
                    ?testError({domains_do_not_intersect, 4, 5}, test_domain:x(normal, 4)),
                    ?testError({domain_not_subset, 4, 5}, test_domain:x(strict, 4)),
                    ?test(8, test_domain:x(any)),
                    ?testError({domain_not_subset, any, 5}, test_domain:x(strict, any)),
                    ?test(8, test_domain:x(5))]
           end).

application_test_() ->
    ?setup("module test { app }
            import erlang/+
            def f n -> 4 + n
            def app m -> m.f",
           fun({ok, _}) ->
                   [?test(9, test_domain:app(strict, 5))]
           end).
                   

first_order_function_test_() ->
    ?setup("module test { first-order }
            def first-order f -> f(3)",
           fun({ok, _}) ->
                   [?test(any, test_domain:'first-order'(any)),
                    ?test(4, test_domain:'first-order'(fun(N) -> N + 1 end))]
           end).

run_xor_test_() ->
    ?setup("module test { xor }
            type Boolean -> (True | False)
            def xor
              | Boolean/True Boolean/False -> Boolean/True
              | Boolean/False (b: Boolean/True) -> b
              | _ _ -> Boolean/False",
    fun({ok, _}) ->
            Boolean = {sum, ['Boolean/False', 'Boolean/True']},
            [?test(Boolean, test_domain:'xor'(any, any)),
             ?test('Boolean/True', test_domain:'xor'('Boolean/True', 'Boolean/False')),
             ?test(Boolean, test_domain:'xor'(Boolean, Boolean)),
             ?test('Boolean/False', test_domain:'xor'('Boolean/True', 'Boolean/True'))]
    end).

direct_recursion_test_() ->
    ?setup("module test { g }
            type State -> (Start | Continue | Stop)
            def g
             | State/Start -> g(State/Continue)
             | State/Continue -> g(State/Stop)
             | (state: State/Stop) -> state",
    fun({ok, _}) ->
            [?test('State/Stop', test_domain:g('State/Stop')),
             ?test('State/Stop', test_domain:g('State/Start'))]
    end).

infinite_recursion_test_() ->
    ?setup("module test { recurse }
            def recurse a -> recurse(a)",
           fun({ok, _}) ->
                   [?test({recur, _}, test_domain:recurse(3))]
           end).

infinite_co_recursion_test_() ->
    ?setup("module test { f }
            def f a -> g(a)
            def g a -> h(a)
            def h a -> f(a)",
           fun({ok, _}) ->
                   ?test({recur, _}, test_domain:f('_'))
           end).

application_error_test_() ->
    ?setup("module test { f }
            def f a -> a(f)",
           fun({ok, _}) ->
                   [?test(none, test_domain:f(lenient, {sum, [1,2]})),
                    ?testError({expected_function_domain, {sum, [1,2]}}, test_domain:f(strict, {sum, [1,2]})),
                    ?testError({expected_function_domain, {sum, [1,2]}}, test_domain:f(normal, {sum, [1,2]}))]
           end).

pair_values_refinement_test_() ->
    ?setup("module test { f }
            type Boolean -> (True | False)
            def f t -> t: Boolean",
           fun({ok, _}) ->
                   [?test('Boolean/True', test_domain:f('Boolean/True')),
                    ?test({sum, ['Boolean/False', 'Boolean/True']}, test_domain:f(lenient, 'any')),
                    ?test({sum, ['Boolean/False', 'Boolean/True']}, test_domain:f(normal, 'any')),
                    ?testError({pair_not_subset, any, {sum, ['Boolean/False', 'Boolean/True']}}, test_domain:f(strict, 'any'))]
           end).


pair_expressions_refinement_test_() ->
    ?setup("module test { f }
            type Boolean -> (True | False)
            type Option a -> (a | None)
            def id a -> a
            def f t -> id(t): Option(Boolean)",
           fun({ok, _}) ->
                   Boolean = {sum, ['Boolean/False', 'Boolean/True']},
                   Constraint = {sum, ordsets:from_list(['Boolean/True',
                                                         'Boolean/False',
                                                         'Option/None'])},
                   [?test(Boolean, test_domain:f('Boolean')),
                    ?testError({pair_not_subset, _, Constraint}, test_domain:f(normal, 'Option')),
                    ?test(Constraint, test_domain:f(any)),
                    ?testError({pair_not_subset, any, Constraint}, test_domain:f(strict, any))]
           end).

pass_type_function_as_atom_test_() ->
    ?setup("module test { f }
            type Boolean -> (True | False)
            type Option a -> (a | None)
            def f g -> g(Boolean)",
           fun({ok, _}) ->
                   Expected = {sum, ordsets:from_list(['Boolean/True',
                                                       'Boolean/False',
                                                       'Option/None'])},
                   ?test(Expected, test_domain:f('Option'))
           end).

fun_clause_test_() ->
    ?setup("module test { test }
            type Args -> (A | B | C)
            def ap f a -> f(a)
            def test a -> ap(| Args/A -> Args/C
                             | Args/B -> Args/C,
                             a)",
           fun({ok, _}) ->
               [?test('Args/C', test_domain:test('Args/A')),
                ?test('Args/C', test_domain:test(lenient, 'Args')),
                ?test('Args/C', test_domain:test(normal, 'Args')),
                ?testError({domain_not_subset, {sum, ['Args/A', 'Args/B', 'Args/C']},
                                                   {sum, ['Args/A', 'Args/B']}},
                           test_domain:test(strict, 'Args')),
                ?test(none, test_domain:test(lenient, 'Args/C')),
                ?testError({domains_do_not_intersect, 'Args/A', 'Args/C'},
                           {domains_do_not_intersect, 'Args/B', 'Args/C'},
                           test_domain:test(normal, 'Args/C')),
                ?testError({domain_not_subset, 'Args/A', 'Args/C'},
                           {domain_not_subset, 'Args/B', 'Args/C'},
                           test_domain:test(strict, 'Args/C'))]
           end).


assignment_variable_test_() ->
    ?setup("module test { test }
            type Args -> (A | B | C)
            def test a -> (val f = (| Args/A -> Args/B
                                    | Args/B -> Args/C)
                           f(a))",
           fun({ok, _}) ->
                   [?test('Args/B', test_domain:test('Args/A')),
                    ?test({sum, ['Args/B', 'Args/C']}, test_domain:test({sum, ['Args/A', 'Args/B']}))]
           end).

assignment_pattern_test_() ->
    ?setup("module test { test }
            type Dict -> {a: (Ah | Oh), b: Buh}
            def test input -> (val {a: (out: Dict/Ah)} = input, out)",
           fun({ok, _}) ->
                   [?test('Dict/Ah', test_domain:test('Dict')),
                    ?test('Dict/Ah', test_domain:test(any)),
                    ?testError({domains_do_not_intersect, 'Dict/Ah', #{a := 'Dict/Ah'}}, test_domain:test('Dict/Ah')),
                    ?testError({domains_do_not_intersect, 'Dict/Ah', 'Dict/Oh'}, test_domain:test(#{a => 'Dict/Oh'}))]
           end).

assignment_nonexistent_key_test_() ->
    ?setup("module test { test }
           type Sum -> ({a: A} | {b: B})
           def test input -> (val {a, b} = input, b)",
           fun({ok, _}) ->
                   [?testError({nonexistent_key, a, #{}},
                               {nonexistent_key, b, #{}}, test_domain:test('Sum')),
                    ?testError({nonexistent_key, b, #{}}, test_domain:test(#{a => 'Sum/A'})),
                    ?test('Sum/B', test_domain:test(#{a => 'Sum/A', b => 'Sum/B'}))]
           end).

assignment_tagged_product_test_() ->
    ?setup("module test { test }
            type Tagged -> T: {a: A, b: B}
            def test input -> (val (Tagged/T: {a: out}) = input, out)",
           fun({ok, _}) ->
                   [?test('Tagged/A', test_domain:test('Tagged')),
                    ?testError({domains_do_not_intersect, {tagged, 'S', _}, {tagged, 'Tagged/T', _}},
                               test_domain:test({tagged, 'S', #{a => 'Tagged/A'}}))]
           end).

tagged_application_test_() ->
    ?setup("module test { f, g }
            type Args -> (A | B | C)
            type Tagged1 -> T: {a: Args, b: Args}
            type Tagged2 -> T: {a: 4, b: Args}
            def f a b -> Tagged1/T(a, b)
            def g a -> Tagged2/T(a)",
           fun({ok, _}) ->
                   [?test({tagged, 'Tagged1/T', #{a := 'Args/A',
                                                  b := 'Args/B'}}, test_domain:f('Args/A', 'Args/B')),
                    ?testError({domains_do_not_intersect, {sum, ['Args/A', 'Args/B', 'Args/C']}, 5},
                               test_domain:f('Args/C', 5)),
                    ?test({tagged, 'Tagged2/T', #{a := 4,
                                                  b := 'Args/B'}}, test_domain:g('Args/B')),
                    ?testError({domains_do_not_intersect, {sum, ['Args/A', 'Args/B', 'Args/C']}, 4},
                               test_domain:g(4))]
           end).

tagged_sum_application_test_() ->
    ?setup("module test { f }
            type Args -> (A | B | C)
            type Pargs -> (A | B | C)
            type Tagged -> T: ({k1: Args, k2: Args} | {k3: Pargs, k4: Pargs})
            def f a -> Tagged/T(a)",
           fun({ok, _}) ->
                   [?test({tagged, 'Tagged/T', #{k1 := 'Args/A',
                                                 k2 := 'Args/B'}}, test_domain:f(#{k1 => 'Args/A', k2 => 'Args/B'})),
                    ?test({tagged, 'Tagged/T', #{k3 := 'Pargs/B',
                                                 k4 := 'Pargs/C'}}, test_domain:f(#{k3 => 'Pargs/B', k4 => 'Pargs/C'}))]
           end).

wrong_arity_test_() ->
    ?setup("module test { f }
            def id a -> a
            def f a -> id()",
           fun(Err) ->
                   [?testError({wrong_arity, {id, 1}, 0, 1}, Err)]
           end).

dict_duplicate_keys_test_() ->
    ?setup("module test { f }
            def f -> {k: 1, k: 2}",
           fun(Err) ->
                   [?testError({duplicate_keys, [k]}, Err)]
           end).

list_test_() ->
    ?setup("module test { f }
            type Args -> (A | B | C)
            def f [a: Args, b: Args] -> [b, a]",
           fun({ok, _}) ->
                   AnyExpected = [{sum, ['Args/A', 'Args/B', 'Args/C']}, {sum, ['Args/A', 'Args/B', 'Args/C']}],
                   [?test(AnyExpected, test_domain:f(any)),
                    ?test(['Args/C', 'Args/A'], test_domain:f(['Args/A', 'Args/C'])),
                    ?testError({domains_do_not_intersect, ['Args/A'], AnyExpected}, test_domain:f(['Args/A']))]
           end).

assignment_intersection_no_subset_test_() ->
    ?setup("module test { test }
            type Args -> (A | B | C)
            def test input -> (val (a: (Args/A | Args/B)) = input, a)",
           fun({ok, _}) ->
                   [?test('Args/A', test_domain:test('Args/A')),
                    ?test({sum, ['Args/A', 'Args/B']}, test_domain:test('Args'))]
           end).


assignment_narrowing_of_expression_domain_by_pattern_test_() ->
    ?setup("module test { test }
            type Args -> (A | B | C)
            def test input -> (val (a: Args/A) = input, input)",
           fun({ok, _}) ->
                   [?test({sum, ['Args/A', 'Args/B', 'Args/C']}, test_domain:test(normal, 'Args')),
                    ?testError({domain_not_subset, {sum, ['Args/A', 'Args/B', 'Args/C']}, 'Args/A'},
                               test_domain:test(strict, 'Args'))]
           end).

match_single_arg_test_() ->
    ?setup("module test { test }
            type Args -> (A | B | C)
            def match a f -> f(a)
            def test input -> input.match(| Args/A -> Args/B
                                          | Args/B -> Args/C)",
           fun({ok, _}) ->
                   [?test('Args/B', test_domain:test('Args/A')),
                    ?testError({domains_do_not_intersect, 'Args/A', 'Args/C'},
                               {domains_do_not_intersect, 'Args/B', 'Args/C'},
                               test_domain:test('Args/C')),
                    ?testError({domain_not_subset,
                                {sum, ['Args/A', 'Args/B', 'Args/C']},
                                {sum, ['Args/A', 'Args/B']}},
                               test_domain:test('strict', 'Args'))]
           end).

pattern_recursive_lookup_test_() ->
    ?setup("module test { last }
            type Args -> (A | B | C)
            type List -> (Nil | Cons: {elem: Args, tail: List})
            def last
             | (List/Cons: {elem, tail: List/Nil}) -> elem
             | (List/Cons: {tail}) -> tail.last()",
           fun({ok, _}) ->
                   ListDomain = {tagged, 'List/Cons',
                                 #{elem => 'Args/A',
                                   tail => {tagged, 'List/Cons',
                                            #{elem => 'Args/A',
                                              tail => {tagged, 'List/Cons',
                                                       #{elem => 'Args/A',
                                                         tail => 'List/Nil'}}}}}},
                   [?test('Args/A', test_domain:last(ListDomain))]
           end).
