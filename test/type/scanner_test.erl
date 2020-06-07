-module(scanner_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

run(Code, RunAsserts) ->
    {ok, _, {TypeAST, AST}} = kind:get_AST(Code),
    io:format("DefAST: ~p~n", [AST]),
    io:format("TypeAST: ~p~n", [TypeAST]),
    {ok, TypeMod} = typer:load("test", TypeAST),
    Env = scanner:scan(TypeMod, AST),
    RunAsserts(Env),
    true = code:soft_purge(TypeMod),
    true = code:delete(TypeMod).


env_gen_test() ->
    AST = [{def, 1, function1, [], []}, {def, 2, function2, [], []}],
    Output = scanner:scan({}, AST),
    ExpectedDomains = [none, none],
    ActualDomains = [DomainFun([]) || {_, {f, _, DomainFun}} <- maps:to_list(Output)],
    ExpectedKeys = [{function1, 0}, {function2, 0}],
    ActualKeys = [Key || {Key, _} <- maps:to_list(Output)],
    ?assertEqual(ExpectedDomains, ActualDomains),
    ?assertEqual(ExpectedKeys, ActualKeys).

run_xor_test() ->
    Code = "type Boolean -> True | False\n"
           "def xor a b\n"
           " | True, False -> True\n"
           " | False, True -> b\n"
           " | _, _ -> False",
    RunAsserts = fun(Env) ->
                         {f, 'xor', DomainFun} = maps:get({'xor', 2}, Env),

                         Actual1 = DomainFun(any, any),
                         Expected1 = {sum, #{'Boolean/True' => true, 'Boolean/False' => true}},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('Boolean/True', 'Boolean/False'),
                         Expected2 = 'Boolean/True',
                         ?assertEqual(none, domain:diff(Expected2, Actual2)),

                         Boolean = {sum, #{'Boolean/True' => true, 'Boolean/False' => true}},
                         Actual3 = DomainFun(Boolean, Boolean),
                         Expected3 = Boolean,
                         ?assertEqual(none, domain:diff(Expected3, Actual3)),

                         Actual4 = DomainFun('Boolean/True', 'Boolean/True'),
                         Expected4 = 'Boolean/False',
                         ?assertEqual(none, domain:diff(Expected4, Actual4))
                 end,
    run(Code, RunAsserts).

direct_recursion_test() ->
    Code = "type State -> Start | Continue | Stop\n"
           "def g state\n"
           " | Start -> g(Continue)\n"
           " | Continue -> g(Stop)\n"
           " | Stop -> state",

    RunAsserts = fun(Env) ->
                         {f, g, DomainFun} = maps:get({g, 1}, Env),

                         Actual1 = DomainFun('State/Stop'),
                         Expected1 = 'State/Stop',
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('State/Start'),
                         Expected2 = 'State/Stop',
                         ?assertEqual(none, domain:diff(Expected2, Actual2))
                 end,
    run(Code, RunAsserts).

infinite_recursion_test() ->
    Code = "def recurse a -> recurse(a)",
    RunAsserts = fun(Env) ->
                         {f, recurse, DomainFun} = maps:get({recurse, 1}, Env),
                         Expected = none,
                         Actual = DomainFun(['_']),
                         ?assertEqual(Expected, Actual)
                 end,
    run(Code, RunAsserts).
                    
infinite_co_recursion_test() ->
    Code = "def f a -> g(a)\n"
           "def g a -> h(a)\n"
           "def h a -> f(a)",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Expected = none,
                         Actual = DomainFun(['_']),
                         ?assertEqual(Expected, Actual)
                 end,
    run(Code, RunAsserts).

application_error_test() ->
    Code = "def f a -> a(f)",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         {error, Error} = DomainFun(any),
                         ExpectedError = expected_function_domain,
                         ExpectedDomain = any,
                         [{_, {ActualError, ActualDomain}, _}] = Error,
                         ?assertEqual(ExpectedError, ActualError),
                         ?assertEqual(ExpectedDomain, ActualDomain)
                 end,
    run(Code, RunAsserts).

lookup_expr_product_test() ->
    Code ="def f t -> t { a, b }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Expected1 = {product, #{a => 'A', b => 'B'}},
                         Actual1 = DomainFun({product, #{a => 'A', b => 'B', c => 'C'}}),
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Expected2 = {product, #{a => 'A', b => 'B'}},
                         Actual2 = DomainFun({tagged, tag, {product, #{a => 'A', b => 'B'}}}),
                         ?assertEqual(none, domain:diff(Expected2, Actual2)),

                         Expected3 = none,
                         Actual3 = DomainFun({product, #{c => 'C', d => 'D'}}),
                         ?assertEqual(none, domain:diff(Expected3, Actual3))
                 end,
    run(Code, RunAsserts).

lookup_expr_sum_test() ->
    Code = "type T -> T: {blip: (Blip | Blop)\n"
           "              blup: (Blup | Blap)}\n"
           "def f a\n"
           " | (t: T) -> t { blup }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Actual = DomainFun({tagged, 'T', {product, #{blup => 'T/Blup', blip => 'T/Blop'}}}),
                         Expected = {product, #{blup => 'T/Blup'}},
                         ?assertEqual(none, domain:diff(Expected, Actual)),

                         Input = {product, #{blup => 'T/Blup', blip => 'T/Blop'}},
                         Error = DomainFun(Input),
                         ?errorMatch({no_intersection, Input, {tagged, _, _}}, Error)
                 end,
    run(Code, RunAsserts).
           
lookup_error_propagation_test() ->
    Code = "def f t -> t { a }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         StackError = {error, [some_error]},
                         Actual1 = DomainFun(StackError),
                         Expected1 = StackError,
                         ?assertEqual(none, domain:diff(Expected1, Actual1))
                 end,
    run(Code, RunAsserts).

lookup_non_product_or_tagged_domain_test() ->
    Code = "def f t -> t { a }",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         ?errorMatch({expected_product_domain, any}, DomainFun(any))
                 end,
    run(Code, RunAsserts).

pair_values_refinement_test() ->
    Code = "type Boolean -> True | False\n"
           "def f t -> t: Boolean",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Actual1 = DomainFun('Boolean/True'),
                         Expected1 = 'Boolean/True',
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('any'),
                         Constraint = {sum, #{'Boolean/True' => true, 'Boolean/False' => true}},
                         ?errorMatch({pair_not_subset, any, Constraint}, Actual2)
                 end,
    run(Code, RunAsserts).

pair_expressions_refinement_test() ->
    Code = "type Boolean -> True | False\n"
           "type Option a -> a | None\n"
           "def id a -> a\n"
           "def f t -> id(t): Option(Boolean)",
    RunAsserts = fun(Env) ->
                         {f, f, DomainFun} = maps:get({f, 1}, Env),
                         Actual1 = DomainFun('Boolean'),
                         Expected1 = {sum, #{'Boolean/True' => true, 'Boolean/False' => true}},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('any'),
                         Constraint = {sum, #{'Boolean/True' => true,
                                              'Boolean/False' => true,
                                              'Option/None' => true}},
                         ?errorMatch({pair_not_subset, any, Constraint}, Actual2)
                 end,
    run(Code, RunAsserts).

lambda_clause_test() ->
    Code = "type Args -> A | B | C\n"
           "def ap f a -> f(a)\n"
           "def test a -> ap(A -> C\n"
           "                 B -> C,\n"
           "                 a)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Args/A'),
                         ?assertEqual('Args/C', Actual1),

                         Actual2 = DomainFun('Args'),
                         LambdaDomain = {sum, #{'Args/A' => true, 'Args/B' => true}},
                         InputDomain = {sum, #{'Args/A' => true,'Args/B' => true,'Args/C' => true}}, 
                         ?errorMatch({arguments_not_subsets, [InputDomain], [LambdaDomain]}, Actual2),

                         Actual3 = DomainFun('Args/C'),
                         LambdaDomain = {sum, #{'Args/A' => true, 'Args/B' => true}},
                         ?errorMatch({no_intersection, 'Args/C', 'Args/B'}, 
                                     {no_intersection, 'Args/C', 'Args/A'},
                                     Actual3)

                 end,
    run(Code, RunAsserts).

assignment_variable_test() ->
    Code = "type Args -> A | B | C\n"
           "def test a -> (val f = (A -> B\n"
           "                        B -> C)\n"
           "               f(a))",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Args/A'),
                         ?assertEqual('Args/B', Actual1),

                         Actual2 = DomainFun({sum, #{'Args/A' => true, 'Args/B' => true}}),
                         Expected2 = {sum, #{'Args/B' => true, 'Args/C' => true}},
                         ?assertEqual(none, domain:diff(Expected2, Actual2))
                 end,
    run(Code, RunAsserts).

assignment_pattern_test() ->
    Code = "type Dict -> {a: (Ah | Oh), b: Buh}\n"
           "def test input -> (val {a: Ah} = input, a)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Dict'),
                         ?assertEqual('Dict/Ah', Actual1),

                         Actual2 = DomainFun('Dict/Ah'),
                         ?errorMatch({non_dict_lookup, 'Dict/Ah'}, Actual2),

                         Actual3 = DomainFun({product, #{a => 'Dict/Oh'}}),
                         ?errorMatch({no_intersection, 'Dict/Oh', 'Dict/Ah'}, Actual3)
                 end,
    run(Code, RunAsserts).

assignment_nonexistent_key_test() ->
    Code = "type Sum -> {a: A} | {b: B}\n"
           "def test input -> (val {a, b} = input, b)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Sum'),
                         ?errorMatch({nonexistent_key, b, {product, _}},
                                     {nonexistent_key, a, {product, _}}, Actual1),

                         Actual2 = DomainFun({product, #{a => 'Sum/A'}}),
                         ?errorMatch({nonexistent_key, b, {product, _}}, Actual2),

                         Actual3 = DomainFun({product, #{a => 'Sum/A', b => 'Sum/B'}}),
                         ?assertEqual('Sum/B', Actual3)
                 end,
    run(Code, RunAsserts).

assignment_tagged_product_test() ->
    Code = "type Tagged -> T: {a: A, b: B}\n"
           "def test input -> (val T{a} = input, a)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Tagged'),
                         ?assertEqual('Tagged/A', Actual1),

                         Actual2 = DomainFun({tagged, 'S', {product, #{a => 'Tagged/A'}}}),
                         ?errorMatch({no_intersection, {tagged, 'S', _}, {tagged, 'Tagged/T', _}}, Actual2)
                 end,
    run(Code, RunAsserts).

assignment_intersection_no_subset_test() ->
    Code = "type Args -> A | B | C\n"
           "def test input -> (val a: (A | B) = input, a)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Args/A'),
                         ?assertEqual('Args/A', Actual1),

                         Actual2 = DomainFun('Args'),
                         Expected2 = {sum, #{'Args/A' => true, 'Args/B' => true}},
                         ?assertEqual(none, domain:diff(Expected2, Actual2))
                 end,
    run(Code, RunAsserts).

% I'd like for input to reflect that it can only have the value `Args/A` if it
% matches the pattern for `a`. That would mean returning `Args/A`.
%
% This is really hard to do for a generelised expression though. Imagine if we
% had assigned `a` to `b` which had been assigned to the input. Then we would
% have to run backwards through the expression tree to map all assignments
% retroactively with the new narrower definitions.
%
% I'm trying to think if there's a way to achieve this, but in the meantime,
% I'll let `input` keep it's original domain.
assignment_narrowing_of_expression_domain_by_pattern_test() ->
    Code = "type Args -> A | B | C\n"
           "def test input -> (val a: A = input, input)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Args'),
                         Expected1 = {sum, #{'Args/A' => true, 'Args/B' => true, 'Args/C' => true}},
                         ?assertEqual(none, domain:diff(Expected1, Actual1))
                 end,
    run(Code, RunAsserts).

match_single_arg_test() ->
    Code = "type Args -> A | B | C\n"
           "def test input -> input.match(A -> B\n"
           "                              B -> C)",
    RunAsserts = fun(Env) ->
                         {f, test, DomainFun} = maps:get({test, 1}, Env),
                         Actual1 = DomainFun('Args/A'),
                         ?assertEqual('Args/B', Actual1),

                         Actual2 = DomainFun('Args/C'),
                         ?errorMatch({no_intersection, 'Args/C', 'Args/B'},
                                     {no_intersection, 'Args/C', 'Args/A'},
                                     Actual2),

                         Actual3 = DomainFun('Args'),
                         ?errorMatch({arguments_not_subsets, [{sum, _}], [{sum, _}]}, Actual3)
                 end,
    run(Code, RunAsserts).

pattern_recursive_lookup_test() ->
    Code = "type Args -> A | B | C\n"
           "type List -> Nil | Cons: {elem: Args, tail: List}\n"
           "def last input\n"
           " | Cons {elem, tail: Nil} -> elem\n"
           " | Cons {tail} -> tail.last()",
    RunAsserts = fun(Env) ->
                         {f, last, DomainFun} = maps:get({last, 1}, Env),
                         ListDomain = {tagged, 'List/Cons',
                                       {product, #{elem => 'Args/A',
                                                   tail => {tagged, 'List/Cons',
                                                            {product, #{elem => 'Args/A',
                                                                       tail => {tagged, 'List/Cons',
                                                                                {product, #{elem => 'Args/A',
                                                                                            tail => 'List/Nil'}}}}}}}}},
                         Actual1 = DomainFun(ListDomain),
                         ?assertEqual('Args/A', Actual1)
                 end,
    run(Code, RunAsserts).

