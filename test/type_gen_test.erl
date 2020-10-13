-module(type_gen_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(setup(Code, Tests), {setup, loadFun(Code, #{add_kind_libraries => false}), fun unload/1, Tests}).
-define(setup(Code, Options, Tests), {setup, loadFun(Code, Options), fun unload/1, Tests}).

loadFun(Code, Options) -> 
    TestOptions = maps:merge(#{purge_scanner_module => false}, Options),
    fun() -> case kind:load(Code, TestOptions) of 
                 {error, Errs}  -> {error, Errs};
                 {ok, Modules}  ->
                     % Only return scanner modules (e.g. modules starting with `types`)
                     {ok, [Mod || Mod <- Modules,
                                  Name <- [atom_to_list(Mod)],
                                  string:find(Name, "types") =:= Name]}
             end
    end.

unload({error, _}) -> noop;
unload({ok, ModuleNames}) ->
    Remove = fun(TypeMod) ->
                     true = code:soft_purge(TypeMod),
                     true = code:delete(TypeMod) end,
    [Remove(ModuleName) || ModuleName <- ModuleNames].

sum_type_boolean_test_() ->
    {"The domain of a sum type is the same as the type module function for
     said type",
    ?setup("type Boolean -> True | False",
           fun({ok, [Mod | _]}) ->
                   Expected = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                   Actual = Mod:domain('Boolean'),
                   Actual2 = Mod:'Boolean'(),
                   [?test(none, domain:diff(Expected, Actual)),
                    ?test(none, domain:diff(Expected, Actual2))]
           end)}.

product_type_test_() ->
    {"The domain of a product type is the same as the type module function for
     said type",
    ?setup("type P -> {a: A, b: B}",
           fun({ok, [Mod | _]}) ->
                   Expected = #{a => 'P/A', b => 'P/B'},
                   Actual = Mod:domain('P'),
                   Actual2 = Mod:'P'(),
                   [?test(none, domain:diff(Expected, Actual)),
                    ?test(none, domain:diff(Expected, Actual2))]
           end)}.

tagged_type_test_() ->
    {"The domain of a taggged type is the same as the type module function for"
     "said type",
     ?setup("type P -> K: {a: A, b: B}",
            fun({ok, [Mod | _]}) ->
                    Expected = {tagged, 'P/K', #{a => 'P/A', b => 'P/B'}},
                    Actual = Mod:domain('P'),
                    Actual2 = Mod:'P'(),
                    [?test(none, domain:diff(Expected, Actual)),
                     ?test(none, domain:diff(Expected, Actual2))]
            end)}.

type_parameter_test_() ->
    {"a type function with a parameter should generate a function in the type"
     "module which can be called with an argument",
     ?setup("type Id a -> a\n"
            "type T -> A | B",
            fun({ok, [Mod | _]}) -> 
                    {f, 'Id', DomainFun} = Mod:domain('Id'),
                    [?test('T/A', Mod:'Id'('T/A')),
                     ?test('T/B', DomainFun('T/B'))]
            end)}.

tagged_type_reuse_name_test_() ->
    {"When a type is tagged with its own name, it becomes an opaque new type"
     "this means that while `P` is an int, it won't be accepted by functions"
     "that accept Int parameters. This is useful for example if a function"
     "accepts two parameters that are both Ints because it prevents the caller"
     "from accidentially swapping them",
     ?setup("type P -> P: Int",
            fun({ok, [Mod | _]}) ->
                    Expected = {tagged, 'P', 'P/Int'},
                    Actual = Mod:domain('P'),
                    Actual2 = Mod:'P'(),
                    [?test(none, domain:diff(Expected, Actual)),
                     ?test(none, domain:diff(Expected, Actual2))]
            end)}.

tagged_subtype_test_() ->
    {"Tags are useful for reusing existing data-types in new situations where
      functions that work on the underlying data type (in this case `Int`)
      aren't useful for the new domain.",
     ?setup("type TimeUnit -> Hour: Int | Minute: Int | Second: Int",
            fun({ok, [Mod | _]}) ->
                    Expected = {tagged, 'TimeUnit/Minute', 'TimeUnit/Int'},
                    Actual = Mod:domain('TimeUnit/Minute'),
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

tagged_product_subset_test_() ->
    {"Tags can also be used for types in a product. In this test the type
      constructor `Time` generates the following types: `Time/Hour`,
      `Time/Minute` and `Time/Second`",
     ?setup("type Time -> {hour: (Hour: Int), minute: (Minute: Int), second: (Second: Int)}",
            fun({ok, [Mod | _]}) ->
                    Actual = Mod:domain('Time/Minute'),
                    Expected = {tagged, 'Time/Minute', 'Time/Int'},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

sum_var_test_() ->
    {"We can define new types based on existing types arbitrarily so.",
     ?setup("type Boolean -> True | False\n"
            "type Option a -> a | None",
            fun({ok, [Mod | _]}) ->
                    Expected = {sum, ordsets:from_list(['Boolean/True',
                                                        'Boolean/False',
                                                        'Option/None'])},
                    Actual = Mod:'Option'({sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])}),
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

product_sum_test_() ->
    {"Type constructors without arguments can be used in place of a specific
      type to cover over the entire domain",
     ?setup("type Args -> A | B | C\n"
            "type Elems -> {elem: Args}",
            fun({ok, [Mod | _]}) ->
                    Actual = Mod:domain('Elems'),
                    Expected = #{elem => {sum, ['Args/A', 'Args/B', 'Args/C']}},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

buried_var_test_() ->
    {"When a type declared in a type constructor (here `Buried/Bottom`)
      contains a variable, it is itself a type constructor, and can be called.",
     ?setup("type Buried a -> Surface | Bottom: { var: a }\n"
            "type Hidden -> Treasure",
            fun({ok, [Mod | _]}) ->
                    Expected = {tagged, 'Buried/Bottom', 
                                #{var => 'Hidden/Treasure'}},
                    {f, 'Buried/Bottom', DomainFun} = Mod:domain('Buried/Bottom'),
                    Actual = DomainFun('Hidden/Treasure'),
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

var_order_test_() ->
    {"The parameter order in a subtype constructor is ambigious. In kind it's
      defined as the order the type variables appear in in a left-biased depth
      first traversel (e.g. the order that you would read them in)",
     ?setup("type Order a b c -> T: (C: c | B: b | A: a)\n"
            "type Args -> A | B | C",
            fun({ok, [Mod | _]}) ->
                    Expected = {tagged, 'Order/T', 
                                {sum, ordsets:from_list([
                                                         {tagged, 'Order/C', 'Args/C'},
                                                         {tagged, 'Order/A', 'Args/A'},
                                                         {tagged, 'Order/B', 'Args/B'}])}},
                    {f, 'Order/T', DomainFun} = Mod:domain('Order/T'),
                    Actual = DomainFun('Args/C', 'Args/B', 'Args/A'),
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

application_top_level_f_test_() ->
    {"Call type function using domain function",
     ?setup("type Option a -> a | None\n"
            "type BlahOption -> Option(Blah)",
            fun({ok, [Mod | _]}) ->
                    Expected = {sum, ordsets:from_list(['BlahOption/Blah', 'Option/None'])},
                    Actual = Mod:domain('BlahOption'),
                    ?test(none, domain:diff(Expected, Actual))
            end)}.

application_wrong_number_of_args_test_() ->
    {"Check that we see an error when the wrong number of arguments is given to a type constructor",
     ?setup("type Option a -> a | None\n"
            "type BlahOption -> Option(Option/None, Option/None)",
            fun(Error) ->
                    [?testError({wrong_number_of_arguments, 'Option', _, _}, Error)]
            end)}.

application_inner_level_f_test_() ->
    {"Same as above for subtype constructor",
     ?setup("type Option a -> P: {a: a} | None | O: P(a)\n",
            fun({ok, [Mod | _]}) ->
                    {f, 'Option', DomainFun} = Mod:domain('Option'),
                    Actual = DomainFun('Option/None'),
                    Expected = {sum, ordsets:from_list(['Option/None',
                                                        {tagged, 'Option/P',
                                                         #{a => 'Option/None'}},
                                                        {tagged, 'Option/O',
                                                         {tagged, 'Option/P',
                                                          #{a => 'Option/None'}}}])},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

application_first_order_type_test_() ->
    {"Similar to generics we can call a type constructor from another type constructor",
     ?setup("type Args -> Arg1 | Arg2\n"
            "type Option a -> None | a\n"
            "type AnyOption f a -> f(a)",
            fun({ok, [Mod | _]}) ->
                    Actual = Mod:'AnyOption'(Mod:domain('Option'), 'Args/Arg1'),
                    Expected = {sum, ordsets:from_list(['Option/None', 'Args/Arg1'])},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

application_first_order_called_by_type_test_() ->
    {"Call type with another type constructor as function and have that applied to third type"
     "Mostly applications to do with Types are of term type
     'type_application', but when a type variable containing a type
     constructor is used, it's a normal application with the domain of a type
     function, namely `{f, Tag, Function}` and so I want to make sure that it
     works",
     ?setup("type Args -> Arg1 | Arg2\n"
    	    "type Switch a\n"
    	    " | Args/Arg1 -> Args/Arg2\n"
    	    " | Args/Arg2 -> Args/Arg1\n"
    	    "type Apply f a -> f(a)\n"
    	    "type Test -> Apply(Switch, Args/Arg1)",
    	    fun({ok, [Mod | _]}) ->
                [?test('Args/Arg2', Mod:domain('Test'))]
            end)}.

application_product_test_() ->
    {"Subtype constructors make a nice equivalent to case classes with the same parent in Scala",
     ?setup("type P a b -> {a: a, b: b}\n"
            "type Test -> P(A, B)",
            fun({ok, [Mod | _]}) ->
                    Expected = #{a => 'Test/A', b => 'Test/B'},
                    [?test(Expected, Mod:domain('Test'))]
            end)}.


recursion_top_level_f_test_() ->
    {"Recursive types are essential for data structures like linked lists and
      trees. Non-recursive domains are strictly evaluated, but recursive domains
      replace the recursion with a `Recur` domain type containing a
      function which can be evaluated to traverse the structure.",
     ?setup("type List a -> Nil | Cons: {head: a, tail: List(a)}",
            fun({ok, [Mod | _]}) ->
                    {f, 'List', DomainFun} = Mod:domain('List'),
                    Actual = DomainFun('List/Nil'),
                    {_, [_, {_, _, {recur, RecurFun}}]} = Actual,
                    ProductMap = RecurFun(),
                    [?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
                     ?test(_, RecurFun()),
                     ?test('List/Nil', maps:get(head, ProductMap)),
                     ?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap))]
            end)}.

recursion_top_level_non_function_test_() ->
    {"Test that recursion works for a type constructor that doesn't take any arguments",
    ?setup("type Args -> A | B | C\n"
           "type List -> Nil | Cons: {elem: Args, tail: List}",
           fun({ok, [Mod | _]}) ->
                   Actual = Mod:domain('List'),
                   {_, [_, {_, _, {recur, RecurFun}}]} = Actual,
                   ProductMap = RecurFun(),
                   [?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
                    ?test(_, RecurFun()),
                    ?test({sum, ['Args/A', 'Args/B', 'Args/C']}, maps:get(elem, ProductMap)),
                    ?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap))]
           end)}.

pattern_then_type_parse_test_() ->
    {"A type can be pattern matched against types it creates",
     ?setup("type T a\n"
            " | A -> A\n"
            "type Test -> T(T/A)",
            fun({ok, [Mod | _]}) ->
                    [?test('T/A', Mod:domain('Test'))]
            end)}.

pattern_type_test_() ->
    {"The domain of the type constructor `F` should depend entirely on it's input `a`",
     ?setup("type F a\n"
            " | A -> B\n"
            " | B -> C\n"
            " | C -> A",
            fun({ok, [Mod | _]}) ->
                    {f, 'F', DomainFun} = Mod:domain('F'),
                    Actual = DomainFun('F/A'),
                    [?test('F/B', Actual)]
            end)}.

pattern_variable1_test_() ->
    {"pattern matching with types allows variables",
     ?setup("type X a\n"
            " | T -> T\n"
            " | t -> {t: t}",
            fun({ok, [Mod | _]}) ->
                    {f, 'X', DomainFun} = Mod:domain('X'),
                    [?test('X/T', DomainFun('X/T')),
                     ?test(none, domain:diff(#{t => 'S'}, DomainFun('S')))]
            end)}.

pattern_variable2_test_() ->
    {"a pattern variable can be constrained to a type",
     ?setup("type Args -> A | B | C\n"
            "type X a\n"
            " | Args/A -> Args/B\n"
            " | (t: Args) -> t",
            fun({ok, [Mod | _]}) ->
                    {f, 'X', DomainFun} = Mod:domain('X'),
                    [?test('Args/B', DomainFun('Args/A')),
                     ?test('Args/C', DomainFun('Args/C'))]
            end)}.

pattern_dict_test_() ->
    {"When using a dict pattern, the variables are mapped on to the values associated with the product keys",
     ?setup("type Args -> A | B | C\n"
            "type Test t\n"
            " | {a, b} -> a | b",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    Input1 = #{a => 'Args/A', b => 'Args/B'},
                    Actual1 = DomainFun(Input1),
                    Expected1 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},

                    % Test behavior with unused key
                    Input2 = #{a => 'Args/A', b => 'Args/B', c => 'Args/C'},
                    Actual2 = DomainFun(Input2),
                    Expected2 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                    [?test(none, domain:diff(Expected1, Actual1)),
                     ?test(none, domain:diff(Expected2, Actual2))]
            end)}.

pattern_dict_pair_test_() ->
    {"When a dict pattern contains a key value pair, the domain under the key
     is matched against the value pattern. In this test the value pattern is a
                                           variable so the domain under key `a`
                                           should be assigned to type variable
                                           `s`",
     ?setup("type Args -> A | B | C\n"
            "type Test t\n"
            " | {a: s, b} -> s | b",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    Input = #{a => 'Args/A', b => 'Args/B', c => 'Args/C'},
                    Actual = DomainFun(Input),
                    Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

pattern_dict_dict_test_() ->
    {"Same as above, but this time the key value pair contains a pattern",
     ?setup("type Args -> A | B | C\n"
            "type Test t\n"
            " | {a, b: Args/C} -> a\n"
            " | {a: {a}, b} -> a | b",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    Input1 = #{a => #{a => 'Args/A'}, 
                                        b => 'Args/B', 
                                        c => 'Args/C'},
                    Actual1 = DomainFun(Input1),
                    Expected1 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},

                    Input2 = #{a => 'Args/A', 
                                        b => 'Args/C'},
                    [?test(none, domain:diff(Expected1, Actual1)),
                     ?test('Args/A', DomainFun(Input2))]
            end)}.

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
                    Input1 = #{a => 'Args/A', 
                                        b => 'Args/B'},
                    Actual1 = DomainFun(Input1),
                    Expected1 = 'Args/A',

                    Input2 = #{a => 'Args/A', 
                                        b => 'Args/C'},
                    Expected2 = Input2,%{sum, ordsets:from_list(['Args/A', 'Args/C'])},
                    [?test(none, domain:diff(Expected1, Actual1)),
                     ?test(none, domain:diff(Expected2, DomainFun(Input2)))]
            end)}.

pattern_tagged_test_() ->
    {"A tagged value can be pattern matched by prefacing the pattern with the tag and a colon",
     ?setup("type Args -> T: {a: A, b: B}\n"
            "type Test t\n"
            " | (Args/T: {a: s, b}) -> s | b",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    Actual = DomainFun(Mod:domain('Args')),
                    Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

pattern_no_matching_test_() ->
    {"when no matching pattern is found an error should be returned at compile time",
     ?setup("type Test t\n"
            " | T -> T | S",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    Actual = DomainFun('Test/S'),
                    [?testError({no_matching_pattern, ['Test/S']}, Actual)]
            end)}.

pattern_sum_test_() ->
    {"A pattern with sums should match against all members of said sum",
     ?setup("type Args -> A | B | C\n"
            "type Test t\n"
            " | Args -> Matched\n"
            " | _ -> Unmatched",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    [?test('Test/Matched', DomainFun('Args/A')),
                     ?test('Test/Matched', DomainFun('Args/B')),
                     ?test('Test/Matched', DomainFun('Args/C')),
                     ?test('Test/Unmatched', DomainFun('Test/Matched'))]
            end)}.

pattern_sum_tagged_test_() ->
    {"A pattern with a sum of product should expand to match all products",
     ?setup("type Args -> A: ({b: B} | {c: C})\n"
            "type Test t\n"
            " | (Args/A: {b}) -> b\n"
            " | (Args/A: {c}) -> c\n"
            " | _ -> Unmatched",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    [?test('Args/B', DomainFun({tagged, 'Args/A', #{b => 'Args/B'}})),
                     ?test('Args/C', DomainFun({tagged, 'Args/A', #{c => 'Args/C'}})),
                     ?test('Test/Unmatched', DomainFun({tagged, 'Args/A', #{a => 'Args/A'}})),
                     ?test('Test/Unmatched', DomainFun({tagged, 'Args/B', #{b => 'Args/B'}}))]
            end)}.

pattern_product_sum_test_() ->
    {"Pattern matching should match all members ('Y', 'Z') of type 'X' and generate a clause for each",
     ?setup("type X -> Y | Z\n"
            "type P -> {x: X, xx: X}\n"
            "type Test t\n"
            " | P -> Matched\n"
            " | _ -> Unmatched",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    [?test('Test/Matched', DomainFun(#{x => 'X/Y', xx => 'X/Y'})),
                     ?test('Test/Matched', DomainFun(#{x => 'X/Z', xx => 'X/Y'})),
                     ?test('Test/Matched', DomainFun(#{x => 'X/Y', xx => 'X/Z'})),
                     ?test('Test/Matched', DomainFun(#{x => 'X/Z', xx => 'X/Z'})),
                     ?test('Test/Unmatched', DomainFun('Test/Matched'))]
            end)}.

pattern_tagged_pair_test_() ->
    {"A key value pair outside a dict pattern constraints the variable (key
     part) to a member of the domain of the value part",
     ?setup("type Args -> A | B | C\n"
            "type Test t\n"
            " | (a: Args/A) -> a\n"
            " | (a: Args/B) -> Args/C",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    [?test('Args/A', DomainFun('Args/A')),
                     ?test('Args/C', DomainFun('Args/B'))]
            end)}.

pattern_tagged_sum_list_test_() ->
    {"Instead of multiple patterns, a sum can also be expressed within a"
     "pattern to cover several valid values",
     ?setup("type Args -> A | B | C\n"
            "type Test t\n"
            " | (a: (Args/A | Args/B)) -> a\n"
            " | (a: Args/C) -> Args/B",
            fun({ok, [Mod | _]}) ->
                    {f, 'Test', DomainFun} = Mod:domain('Test'),
                    [?test('Args/A', DomainFun('Args/A')),
                     ?test('Args/B', DomainFun('Args/C'))]
            end)}.

tagged_pair_in_pattern_test_() ->
    {"When a subtype is defined after a pattern it can still be called independently",
     ?setup("type Test -> Blip/Blop(T)\n"
            "type Blip a\n"
            " | value -> Blop: {key: value}",
            fun({ok, [Mod | _]}) ->
                    Expected = {tagged, 'Blip/Blop',
                                #{key => 'Test/T'}},
                    [?test(none, domain:diff(Expected, Mod:domain('Test')))]
            end)}.

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
                    [?test({tagged, 'T/F', #{a := 'Input'}}, 'test_T':'F'('Input'))]
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

qualified_type_pattern_atom_test_() ->
    {"When a pattern contains a qualified type constant (e.g. a type from a
     different module) it should match against it",
     ?setup("type T a\n"
            " | Boolean/False -> Falsy\n"
            " | Boolean/True -> Truthy",
            #{add_kind_libraries => true},
            fun({ok, Mods}) ->
                    Mod = lists:last(Mods),
                    [?test('T/Falsy', Mod:'T'('Boolean/False'))]
            end)}.

qualified_type_pattern_sum_test_() ->
    {"When a pattern contains a qualified type constant (e.g. a type from a
     different module) it should match against it",
     ?setup("type T a\n"
            " | Boolean -> Falsy\n"
            " | _ -> Truthy",
            #{add_kind_libraries => true},
            fun({ok, Mods}) ->
                    Mod = lists:last(Mods),
                    [?test('T/Falsy', Mod:'T'('Boolean/False')),
                     ?test('T/Falsy', Mod:'T'('Boolean/True')),
                     ?test('T/Truthy', Mod:'T'('T/Falsy'))]
            end)}.

qualified_type_undefined_arity_pattern_test_() ->
    {"type constructors that take one or more arguments can't be used as patterns on their own",
     ?setup("type T a\n"
            " | Option -> Falsy\n"
            " | _ -> Truthy",
            #{add_kind_libraries => true},
            fun(Err) -> [?testError({undefined_type_in_pattern, 'Option'}, Err)] end)}.

call_qualified_type_with_args_test_() ->
    {"A qualified type can be evaluated alone and as part of a type
     application. When evaluated as part of a type application, the underlying
     domain function of the types should be called with the arguments",
     ?setup("import kind/prelude\n"
            "type Test -> kind/prelude/Option(Boolean/True)",
            #{add_kind_libraries => true},
            fun({ok, Mods}) ->
                    Mod = lists:last(Mods),
                    Actual = Mod:'Test'(),
                    [?test(none, domain:diff({sum, ordsets:from_list(['Boolean/True', 'Option/Nil'])}, Actual))]
            end)}.

var_application_in_type_def_test_() ->
    {"A type constructor can call non-type functions to construct types",
     ?setup("import erlang\n"
            "type Test a -> a.match(True -> False\n"
            "                       False -> True)",
            #{add_kind_libraries => true},
            fun({ok, Mods}) ->
                    Mod = lists:last(Mods),
                    [?test('Boolean/False', Mod:'Test'('Boolean/True'))]
            end)}.
             

%multiple_tagged_pair_in_pattern_test_() ->
%     Code = "type Test a\n"
%            " | {value} -> Blop: {key: value}\n"
%            " | value -> Blop: {key: value}",
%    RunAsserts = fun(Err) -> ?errorMatch({multiple_definitions, 'Test/Blop'}, Err) end,
%    run(Code, RunAsserts).
