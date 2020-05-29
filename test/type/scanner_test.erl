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
                         Expected1 = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('Boolean/True', 'Boolean/False'),
                         Expected2 = 'Boolean/True',
                         ?assertEqual(none, domain:diff(Expected2, Actual2)),

                         Boolean = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
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
                         Constraint = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?errorMatch({not_a_subset, any, Constraint}, Actual2)
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
                         Expected1 = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Actual2 = DomainFun('any'),
                         Constraint = {sum, ordsets:from_list(['Boolean/True',
                                                               'Boolean/False',
                                                               'Option/None'])},
                         ?errorMatch({not_a_subset, any, Constraint}, Actual2)
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
                         Expected = 'Args/C',
                         ?assertEqual(none, domain:diff(Expected, Actual1)),

                         Actual2 = DomainFun('Args'),
                         LambdaDomain = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                         InputDomain = {sum, ordsets:from_list(['Args/A','Args/B','Args/C'])}, 
                         ?errorMatch({arguments_not_subsets, [InputDomain], [LambdaDomain]}, Actual2),

                         Actual3 = DomainFun('Args/C'),
                         LambdaDomain = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                         ?errorMatch({no_intersection, 'Args/C', 'Args/B'}, 
                                     {no_intersection, 'Args/C', 'Args/A'},
                                     Actual3)
                 end,
    run(Code, RunAsserts).
