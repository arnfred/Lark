-module(typegen_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(setup(Code, Tests), {setup, loadFun(Code), fun unload/1, Tests}).

loadFun(Code) ->
    {ok, [AST]} = parser:parse([{text, Code}], #{add_kind_libraries => false}),
    fun() -> case typer:load(AST) of
                 {error, Errs}                      -> {error, Errs};
                 {ok, {_, ScannerMod, TypeMods, _}} -> 
                     {ok, [ScannerMod | TypeMods]}
             end
    end.

unload({error, _}) -> noop;
unload({ok, ModuleNames}) ->
    Remove = fun(TypeMod) ->
                     true = code:soft_purge(TypeMod),
                     true = code:delete(TypeMod) end,
    [Remove(ModuleName) || ModuleName <- ModuleNames].

run(Code, RunAsserts) ->
    {ok, [AST]} = parser:parse([{text, Code}], #{add_kind_libraries => false}),
    case typer:load(AST) of
        {error, Errs}                       -> RunAsserts({error, Errs});
        {ok, {_, ScannerMod, TypeMods, _}}  ->
            RunAsserts(ScannerMod),
            Remove = fun(TypeMod) ->
                             true = code:soft_purge(TypeMod),
                             true = code:delete(TypeMod) end,
            [Remove(ModuleName) || ModuleName <- [ScannerMod | TypeMods]]
    end.

sum_type_boolean_test() ->
    Code = "type Boolean -> True | False",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         Actual = Mod:domain('Boolean'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'Boolean'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

product_type_test() ->
    Code = "type P -> {a: A, b: B}",
    RunAsserts = fun(Mod) ->
                         Expected = {product, #{a => 'P/A', b => 'P/B'}},
                         Actual = Mod:domain('P'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'P'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

tagged_type_test() ->
    Code = "type P -> K: {a: A, b: B}",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'P/K', {product, #{a => 'P/A', b => 'P/B'}}},
                         Actual = Mod:domain('P'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'P'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

type_parameter_test() ->
    Code = "type Id a -> a\n"
           "type T -> A | B",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('T/A', Mod:'Id'('T/A')),
                         {f, 'Id', DomainFun} = Mod:domain('Id'),
                         ?assertEqual('T/B', DomainFun('T/B'))
                 end,
    run(Code, RunAsserts).

tagged_type_reuse_name_test() ->
    Code = "type P -> P: Int",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'P', 'P/Int'},
                         Actual = Mod:domain('P'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'P'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

tagged_subtype_test() ->
    Code = "type TimeUnit -> Hour: Int | Minute: Int | Second: Int",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'TimeUnit/Minute', 'TimeUnit/Int'},
                         Actual = Mod:domain('TimeUnit/Minute'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

tagged_product_subset_test() ->
    Code = "type Time -> {hour: (Hour: Int), minute: (Minute: Int), second: (Second: Int)}",
    RunAsserts = fun(Mod) ->
                         Actual = Mod:domain('Time/Minute'),
                         Expected = {tagged, 'Time/Minute', 'Time/Int'},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

sum_var_test() ->
    Code = "type Boolean -> True | False\n"
           "type Option a -> a | None",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, ordsets:from_list(['Boolean/True',
                                                          'Boolean/False',
                                                          'Option/None'])},
                         Actual = Mod:'Option'({sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])}),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

product_sum_test() ->
    Code = "type Args -> A | B | C\n"
           "type Elems -> {elem: Args}",
    RunAsserts = fun(Mod) ->
                         Actual = Mod:domain('Elems'),
                         Expected = {product, #{elem => {sum, ['Args/A', 'Args/B', 'Args/C']}}},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

buried_var_test() ->
    Code = "type Buried a -> Surface | Bottom: { var: a }\n"
           "type Hidden -> Treasure",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Buried/Bottom', 
                                     {product, #{var => 'Hidden/Treasure'}}},
                         {f, 'Buried/Bottom', DomainFun} = Mod:domain('Buried/Bottom'),
                         Actual = DomainFun('Hidden/Treasure'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

var_order_test() ->
    Code = "type Order a b c -> T: (C: c | B: b | A: a)\n"
           "type Args -> A | B | C",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Order/T', 
                                     {sum, ordsets:from_list([
                                                           {tagged, 'Order/C', 'Args/C'},
                                                           {tagged, 'Order/A', 'Args/A'},
                                                           {tagged, 'Order/B', 'Args/B'}])}},
                         {f, 'Order/T', DomainFun} = Mod:domain('Order/T'),
                         Actual = DomainFun('Args/C', 'Args/B', 'Args/A'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

application_top_level_f_test_() ->
    {"Call type function",
     ?setup("type Option a -> a | None\n"
            "type BlahOption -> Option(Blah)",
            fun({ok, [Mod | _]}) ->
                    Expected = {sum, ordsets:from_list(['BlahOption/Blah', 'Option/None'])},
                    Actual = Mod:domain('BlahOption'),
                    ?test(none, domain:diff(Expected, Actual))
            end)}.

application_wrong_number_of_args_test() ->
    Code = "type Option a -> a | None\n"
           "type BlahOption -> Option(Option/None, Option/None)",
    RunAsserts = fun(Error) ->
                         ?errorMatch({wrong_number_of_arguments, 'Option', _, _}, Error)
                 end,
    run(Code, RunAsserts).

application_inner_level_f_test() ->
    Code = "type Option a -> P: {a: a} | None | O: P(a)\n",
    RunAsserts = fun(Mod) ->
                         {f, 'Option', DomainFun} = Mod:domain('Option'),
                         Actual = DomainFun('Option/None'),
                         Expected = {sum, ordsets:from_list(['Option/None',
                                                             {tagged, 'Option/P',
                                                              {product, #{a => 'Option/None'}}},
                                                             {tagged, 'Option/O',
                                                              {tagged, 'Option/P',
                                                              {product, #{a => 'Option/None'}}}}])},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

application_first_order_type_test() ->
    Code = "type Args -> Arg1 | Arg2\n"
           "type Option a -> None | a\n"
           "type AnyOption f a -> f(a)",
    RunAsserts = fun(Mod) ->
                         {f, 'AnyOption', DomainFun} = Mod:domain('AnyOption'),
                         Actual = DomainFun('Option', 'Args/Arg1'),
                         Expected = {sum, ordsets:from_list(['Option/None', 'Args/Arg1'])},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

application_product_test() ->
    Code = "type P a b -> {a: a, b: b}\n"
           "type Test -> P(A, B)",
    RunAsserts = fun(Mod) ->
                         Expected = {product, #{a => 'Test/A', b => 'Test/B'}},
                         ?assertEqual(Expected, Mod:domain('Test'))
                 end,
    run(Code, RunAsserts).


recursion_top_level_f_test() ->
    Code = "type List a -> Nil | Cons: {head: a, tail: List(a)}",
    RunAsserts = fun(Mod) ->
                         {f, 'List', DomainFun} = Mod:domain('List'),
                         Actual = DomainFun('List/Nil'),
                         ?assertMatch({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
                         {_, [_, {_, _, {recur, RecurFun}}]} = Actual,
                         ?assertMatch({product, _}, RecurFun()),
                         {product, ProductMap} = RecurFun(),
                         ?assertEqual('List/Nil', maps:get(head, ProductMap)),
                         ?assertMatch({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap))
                 end,
    run(Code, RunAsserts).

recursion_top_level_non_function_test() ->
    Code = "type Args -> A | B | C\n"
           "type List -> Nil | Cons: {elem: Args, tail: List}",
    RunAsserts = fun(Mod) ->
                         Actual = Mod:domain('List'),
                         ?assertMatch({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
                         {_, [_, {_, _, {recur, RecurFun}}]} = Actual,
                         ?assertMatch({product, _}, RecurFun()),
                         {product, ProductMap} = RecurFun(),
                         ?assertMatch({sum, ['Args/A', 'Args/B', 'Args/C']}, maps:get(elem, ProductMap)),
                         ?assertMatch({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap))
                 end,
    run(Code, RunAsserts).

pattern_then_type_parse_test() ->
    Code = "type T a\n"
           " | A -> A\n"
           "type Test -> T(T/A)",
    RunAsserts = fun(Mod) ->
                         ?assertMatch('T/A', Mod:domain('Test'))
                 end,
    run(Code, RunAsserts).

pattern_type_test() ->
    Code = "type F a\n"
           " | A -> B\n"
           " | B -> C\n"
           " | C -> A",
    RunAsserts = fun(Mod) ->
                         {f, 'F', DomainFun} = Mod:domain('F'),
                         Actual = DomainFun('F/A'),
                         ?assertMatch('F/B', Actual)
                 end,
    run(Code, RunAsserts).

pattern_variable1_test() ->
    Code = "type X a\n"
           " | T -> T\n"
           " | t -> {t: t}",
    RunAsserts = fun(Mod) ->
                         {f, 'X', DomainFun} = Mod:domain('X'),
                         ?assertEqual('X/T', DomainFun('X/T')),
                         ?assertEqual(none, domain:diff({product, #{t => 'S'}}, DomainFun('S')))
                 end,
    run(Code, RunAsserts).

pattern_variable2_test() ->
    Code = "type Args -> A | B | C\n"
           "type X a\n"
           " | Args/A -> Args/B\n"
           " | t -> t",
    RunAsserts = fun(Mod) ->
                         {f, 'X', DomainFun} = Mod:domain('X'),
                         ?assertMatch('Args/B', DomainFun('Args/A')),
                         ?assertMatch('Args/C', DomainFun('Args/C'))
                 end,
    run(Code, RunAsserts).

pattern_dict_test() ->
    Code = "type Args -> A | B | C\n"
           "type Test t\n"
           " | {a, b} -> a | b",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         Input = {product, #{a => 'Args/A', b => 'Args/B'}},
                         Actual = DomainFun(Input),
                         Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

pattern_dict_unused_key_test() ->
    Code = "type Args -> A | B | C\n"
           "type Test t\n"
           " | {a, b} -> a | b",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         Input = {product, #{a => 'Args/A', b => 'Args/B', c => 'Args/C'}},
                         Actual = DomainFun(Input),
                         Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

pattern_dict_pair_test() ->
    Code = "type Args -> A | B | C\n"
           "type Test t\n"
           " | {a: s, b} -> s | b",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         Input = {product, #{a => 'Args/A', b => 'Args/B', c => 'Args/C'}},
                         Actual = DomainFun(Input),
                         Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

pattern_dict_dict_test() ->
    Code = "type Args -> A | B | C\n"
           "type Test t\n"
           " | {a, b: Args/C} -> a\n"
           " | {a: {a}, b} -> a | b",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         Input1 = {product, #{a => {product, #{a => 'Args/A'}}, 
                                              b => 'Args/B', 
                                              c => 'Args/C'}},
                         Actual1 = DomainFun(Input1),
                         Expected1 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                         ?assertEqual(none, domain:diff(Expected1, Actual1)),

                         Input2 = {product, #{a => 'Args/A', 
                                              b => 'Args/C'}},
                         ?assertEqual('Args/A', DomainFun(Input2))
                 end,
    run(Code, RunAsserts).

pattern_dict_sum_test_() ->
    {"'Args' in second line of pattern match should be expanded to "
     "all members ('A', 'B', 'C') of the Args type and a clause "
     "should be generated for each",
     ?setup("type Args -> A | B | C\n"
            "type Test t\n"
            " | {a, b: (Args/A | Args/B)} -> a\n"
            " | {a, b: Args} -> t",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    Input1 = {product, #{a => 'Args/A', 
                                         b => 'Args/B'}},
                    Actual1 = DomainFun(Input1),
                    Expected1 = 'Args/A',

                    Input2 = {product, #{a => 'Args/A', 
                                         b => 'Args/C'}},
                    Expected2 = Input2,%{sum, ordsets:from_list(['Args/A', 'Args/C'])},
                    [?test(none, domain:diff(Expected1, Actual1)),
                     ?test(none, domain:diff(Expected2, DomainFun(Input2)))]
            end)}.

pattern_tagged_test() ->
    Code = "type Args -> T: {a: A, b: B}\n"
           "type Test t\n"
           " | (Args/T: {a: s, b}) -> s | b",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         Actual = DomainFun(Mod:domain('Args')),
                         Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

pattern_no_matching_test() ->
    Code = "type Test t\n"
           " | T -> T | S",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         Actual = DomainFun('Test/S'),
                         ?errorMatch({no_matching_pattern, ['Test/S']}, Actual)
                 end,
    run(Code, RunAsserts).

pattern_sum_test() ->
    Code =
           "type Args -> A | B | C\n"
           "type Test t\n"
           " | Args -> Matched\n"
           " | _ -> Unmatched",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         ?assertEqual('Test/Matched', DomainFun('Args/A')),
                         ?assertEqual('Test/Matched', DomainFun('Args/B')),
                         ?assertEqual('Test/Matched', DomainFun('Args/C')),
                         ?assertEqual('Test/Unmatched', DomainFun('Test/Matched'))
                 end,
    run(Code, RunAsserts).

pattern_sum_tagged_test() ->
    Code =
           "type Args -> A: ({b: B} | {c: C})\n"
           "type Test t\n"
           " | (Args/A: {b}) -> b\n"
           " | (Args/A: {c}) -> c\n"
           " | _ -> Unmatched",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         ?assertEqual('Args/B', DomainFun({tagged, 'Args/A', {product, #{b => 'Args/B'}}})),
                         ?assertEqual('Args/C', DomainFun({tagged, 'Args/A', {product, #{c => 'Args/C'}}})),
                         ?assertEqual('Test/Unmatched', DomainFun({tagged, 'Args/A', {product, #{a => 'Args/A'}}})),
                         ?assertEqual('Test/Unmatched', DomainFun({tagged, 'Args/B', {product, #{b => 'Args/B'}}}))
                 end,
    run(Code, RunAsserts).

pattern_product_sum_test_() ->
    {"Pattern matching should match all members ('Y', 'Z') of type 'X' and generate a clause for each",
     ?setup("type X -> Y | Z\n"
            "type P -> {x: X, xx: X}\n"
            "type Test t\n"
            " | P -> Matched\n"
            " | _ -> Unmatched",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    [?test('Test/Matched', DomainFun({product, #{x => 'X/Y', xx => 'X/Y'}})),
                     ?test('Test/Matched', DomainFun({product, #{x => 'X/Z', xx => 'X/Y'}})),
                     ?test('Test/Matched', DomainFun({product, #{x => 'X/Y', xx => 'X/Z'}})),
                     ?test('Test/Matched', DomainFun({product, #{x => 'X/Z', xx => 'X/Z'}})),
                     ?test('Test/Unmatched', DomainFun('Test/Matched'))]
            end)}.

pattern_tagged_pair_test() ->
    Code = "type Args -> A | B | C\n"
           "type Test t\n"
           " | (a: Args/A) -> a\n"
           " | (a: Args/B) -> Args/C",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         ?assertEqual('Args/A', DomainFun('Args/A')),
                         ?assertEqual('Args/C', DomainFun('Args/B'))
                 end,
    run(Code, RunAsserts).

pattern_tagged_sum_list_test() ->
    Code = "type Args -> A | B | C\n"
           "type Test t\n"
           " | (a: (Args/A | Args/B)) -> a\n"
           " | (a: Args/C) -> Args/B",
    RunAsserts = fun(Mod) ->
                         {f, 'Test', DomainFun} = Mod:domain('Test'),
                         ?assertEqual('Args/A', DomainFun('Args/A')),
                         ?assertEqual('Args/B', DomainFun('Args/C'))
                 end,
    run(Code, RunAsserts).

tagged_pair_in_pattern_test() ->
     Code = "type Test -> Blip/Blop(T)\n"
            "type Blip a\n"
            " | value -> Blop: {key: value}",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Blip/Blop',
                                     {product, #{key => 'Test/T'}}},
                         ?assertEqual(none, domain:diff(Expected, Mod:domain('Test')))
                 end,
    run(Code, RunAsserts).

module_type_test_() ->
    {"A type module should be created that can be called for any type member",
     ?setup("module test { T }\n"
            "type T -> {a: A, b: B}",
            fun({ok, _}) ->
                    [?test('T/A', 'test_T':'A'()),
                     ?test('T/B', 'test_T':'B'())]
            end)}.

module_tagged_type_test_() ->
    {"The created type module for `T` needs to contain a type function for the tagged type `F`",
     ?setup("module test { T }\n"
            "type T a -> F: {a: a}",
            fun({ok, _}) ->
                    [?test({tagged, 'T/F', {product, #{a := 'Input'}}}, 'test_T':'F'('Input'))]
            end)}.

unused_type_parameter_test_() ->
    {"In a type application, the number of arguments should be compared with the number of type "
     "arguments. We normally count type arguments as the number of variables present in the type "
     "body, but for type definitions, we want to make sure we count the number of type parameters"
     "instead",
     ?setup("module test { BlahOption }\n"
            "type Option a b -> a | None\n"
            "type BlahOption -> Option(Option/None, Option/None)",
            fun({ok, [Mod | _]}) ->
                    [?test('Option/None', Mod:'BlahOption'())]
            end)}.

type_redefinition_test_() ->
    {"When we redefine one type as another type, the new type returns the function domain: "
     "`{f, Option, F(args)}` where `F(args)` is the function of the original type", 
     ?setup("module test { RedefOption }\n"
            "type Option a -> a | None\n"
            "type RedefOption -> Option",
            fun({ok, [Mod | _]}) ->
                    {f, _, RedefOption} = Mod:'RedefOption'(),
                    [?test('Option/None', RedefOption('Option/None'))]
            end)}.

type_redefinition_args_test_() ->
    {"If we redefine a type and then later call the original type, we want to check that the "
     "number of arguments it expects hasn't changed because of the redefinition",
     ?setup("module test { BlahOption }\n"
            "type BlipOption -> Option\n"
            "type Option a -> a | None\n"
            "type BlupOption -> Option\n"
            "type BlahOption -> Option(Option/None)",
            fun({ok, [Mod | _]}) ->
                    [?test('Option/None', Mod:'BlahOption'())]
            end)}.


%multiple_tagged_pair_in_pattern_test() ->
%     Code = "type Test a\n"
%            " | {value} -> Blop: {key: value}\n"
%            " | value -> Blop: {key: value}",
%    RunAsserts = fun(Err) -> ?errorMatch({multiple_definitions, 'Test/Blop'}, Err) end,
%    run(Code, RunAsserts).
