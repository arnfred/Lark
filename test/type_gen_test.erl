-module(type_gen_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(setup(Code, Tests), {setup, loadFun(Code, #{import_kind_libraries => false}), fun unload/1, Tests}).
-define(setup(Code, Options, Tests), {setup, loadFun(Code, Options), fun unload/1, Tests}).

loadFun(Code, Options) -> 
    TestOptions = maps:merge(#{purge_scanner_module => false}, Options),
    fun() -> case kind:load(Code, TestOptions) of 
                 {error, Errs}  -> {error, Errs};
                 {ok, Modules}  ->
                     % Only return scanner modules (e.g. modules starting with `types`)
                     {ok, [Mod || Mod <- Modules,
                                  Name <- [atom_to_list(Mod)],
                                  string:find(Name, "domain") =:= Name]}
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
    ?setup("type Boolean -> (True | False)",
           fun({ok, [Mod | _]}) ->
                   [?test({sum, ['Boolean/False', 'Boolean/True']}, Mod:'Boolean'())]
           end)}.

product_type_test_() ->
    {"The domain of a product type is the same as the type module function for
     said type",
    ?setup("type P -> {a: A, b: B}",
           fun({ok, [Mod | _]}) ->
                   [?test(#{a := 'P/A', b := 'P/B'}, Mod:'P'())]
           end)}.

tagged_type_test_() ->
    {"The domain of a taggged type is the same as the type module function for"
     "said type",
     ?setup("type P -> K: {a: A, b: B}",
            fun({ok, [Mod | _]}) ->
                    [?test({tagged, 'P/K', #{a := 'P/A', b := 'P/B'}}, Mod:'P'())]
            end)}.

type_parameter_test_() ->
    {"a type function with a parameter should generate a function in the type"
     "module which can be called with an argument",
     ?setup("type Id a -> a\n"
            "type T -> (A | B)",
            fun({ok, [Mod | _]}) -> 
                    [?test('T/A', Mod:'Id'('T/A'))]
            end)}.

tagged_type_reuse_name_test_() ->
    {"When a type is tagged with its own name, it becomes an opaque new type"
     "this means that while `P` is an int, it won't be accepted by functions"
     "that accept Int parameters. This is useful for example if a function"
     "accepts two parameters that are both Ints because it prevents the caller"
     "from accidentially swapping them",
     ?setup("type P -> P: Int",
            fun({ok, [Mod | _]}) ->
                    [?test({tagged, 'P', 'P/Int'}, Mod:'P'())]
            end)}.

tagged_subtype_test_() ->
    {"Tags are useful for reusing existing data-types in new situations where
      functions that work on the underlying data type (in this case `Int`)
      aren't useful for the new domain.",
     ?setup("type TimeUnit -> (Hour: Int | Minute: Int | Second: Int)",
            fun({ok, [Mod | _]}) ->
                    [?test({tagged, 'TimeUnit/Minute', 'TimeUnit/Int'},
                           Mod:'TimeUnit/Minute'('TimeUnit/Int'))]
            end)}.

tagged_product_subset_test_() ->
    {"Tags can also be used for types in a product. In this test the type
      constructor `Time` generates the following types: `Time/Hour`,
      `Time/Minute` and `Time/Second`",
     ?setup("type Time -> {hour: (Hour: Int), minute: (Minute: Int), second: (Second: Int)}",
            fun({ok, [Mod | _]}) ->
                    [?test({tagged, 'Time/Minute', 'Time/Int'},
                           Mod:'Time/Minute'('Time/Int'))]
            end)}.

sum_var_test_() ->
    {"We can define new types based on existing types arbitrarily so.",
     ?setup("type Boolean -> (True | False)\n"
            "type Option a -> (a | None)",
            fun({ok, [Mod | _]}) ->
                    [?test({sum, ['Boolean/False', 'Boolean/True', 'Option/None']},
                           Mod:'Option'('Boolean'))]
            end)}.

product_sum_test_() ->
    {"Type constructors without arguments can be used in place of a specific
      type to cover over the entire domain",
     ?setup("type Args -> (A | B | C)\n"
            "type Elems -> {elem: Args}",
            fun({ok, [Mod | _]}) ->
                    [?test(#{elem := {sum, ['Args/A', 'Args/B', 'Args/C']}},
                           Mod:'Elems'())]
            end)}.

buried_var_test_() ->
    {"When a type declared in a type constructor (here `Buried/Bottom`)
      contains a variable, it is itself a type constructor, and can be called.",
     ?setup("type Buried a -> (Surface | Bottom: { var: a })\n"
            "type Hidden -> Treasure",
            fun({ok, [Mod | _]}) ->
                    [?test({tagged, 'Buried/Bottom', #{var := 'Hidden/Treasure'}},
                           Mod:'Buried/Bottom'('Hidden/Treasure'))]
            end)}.

var_order_test_() ->
    {"The parameter order in a subtype constructor is ambigious. In kind it's
      defined as the order the type variables appear in in a left-biased depth
      first traversel (e.g. the order that you would read them in)",
     ?setup("type Order a b c -> T: [c, b, a]\n"
            "type Args -> (A | B | C)",
            fun({ok, [Mod | _]}) ->
                    [?test({tagged, 'Order/T', ['Args/C', 'Args/B', 'Args/A']},
                           Mod:'Order/T'('Args/C', 'Args/B', 'Args/A'))]
            end)}.

application_top_level_f_test_() ->
    {"Call type function using domain function",
     ?setup("type Option a -> (a | None)\n"
            "type BlahOption -> Option(Blah)",
            fun({ok, [Mod | _]}) ->
                    [?test({sum, ['BlahOption/Blah', 'Option/None']},
                           Mod:'BlahOption'())]
            end)}.

application_wrong_number_of_args_test_() ->
    {"Check that we see an error when the wrong number of arguments is given to a type constructor",
     ?setup("module blah { BlahOption }
             type Option a -> (a | None)
             type BlahOption -> Option(Option/None, Option/None)",
            fun(Error) ->
                    [?testError({wrong_arity, 'Option', 2, 1}, Error)]
            end)}.

application_inner_level_f_test_() ->
    {"Same as above for subtype constructor",
     ?setup("type Option a -> (P: {a: a} | None | O: P(a))\n",
            fun({ok, [Mod | _]}) ->
                    [?test({sum, ['Option/None',
                                  {tagged, 'Option/O',
                                   {tagged, 'Option/P',
                                    #{a := 'Option/None'}}},
                                  {tagged, 'Option/P',
                                   #{a := 'Option/None'}}]},
                           Mod:'Option'('Option/None'))]
            end)}.

application_first_order_type_test_() ->
    {"Similar to generics we can call a type constructor from another type constructor",
     ?setup("type Args -> (Arg1 | Arg2)\n"
            "type Option a -> (None | a)\n"
            "type AnyOption f a -> f(a)",
            fun({ok, [Mod | _]}) ->
                    [?test({sum, ['Args/Arg1', 'Option/None']},
                           % The anonymous function takes two arguments because
                           % it's given the current domain stack as the first
                           % parameter
                           Mod:'AnyOption'(fun Mod:'Option'/1, 'Args/Arg1'))]
            end)}.

application_first_order_called_by_type_test_() ->
    {"Call type with another type constructor as function and have that applied
     to third type" "Mostly applications to do with Types are clearly a type
     constructor application, but when a type variable containing a type
     constructor is used instead, it's indistinguishable from a normal
     application.",
     ?setup("type Args -> (Arg1 | Arg2)\n"
    	    "type Switch\n"
    	    " | Args/Arg1 -> Args/Arg2\n"
    	    " | Args/Arg2 -> Args/Arg1\n"
    	    "type Apply f a -> f(a)\n"
    	    "type Test -> Apply(Switch, Args/Arg1)",
    	    fun({ok, [Mod | _]}) ->
                [?test('Args/Arg2', Mod:'Test'())]
            end)}.

application_product_test_() ->
    {"Subtype constructors make a nice equivalent to case classes with the same parent in Scala",
     ?setup("type P a b -> {a: a, b: b}\n"
            "type Test -> P(A, B)",
            fun({ok, [Mod | _]}) ->
                    Expected = #{a => 'Test/A', b => 'Test/B'},
                    [?test(Expected, Mod:'Test'())]
            end)}.


recursion_top_level_f_test_() ->
    {"Recursive types are essential for data structures like linked lists and
      trees. Non-recursive domains are strictly evaluated, but recursive domains
      replace the recursion with a `Recur` domain type containing a
      function which can be evaluated to traverse the structure.",
     ?setup("type List a -> (Nil | Cons: {head: a, tail: List(a)})",
            fun({ok, [Mod | _]}) ->
                    Actual = Mod:'List'('List/Nil'),
                    {_, [_, {_, _, {recur, ProductMap}}]} = Actual,
                    [?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
                     ?test('List/Nil', maps:get(head, ProductMap())),
                     ?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap()))]
            end)}.

recursion_top_level_non_function_test_() ->
    {"Test that recursion works for a type constructor that doesn't take any arguments",
    ?setup("type Args -> (A | B | C)\n"
           "type List -> (Nil | Cons: {elem: Args, tail: List})",
           fun({ok, [Mod | _]}) ->
                   Actual = Mod:'List'(),
                   {_, [_, {_, _, {recur, ProductMap}}]} = Actual,
                   [?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
                    ?test({sum, ['Args/A', 'Args/B', 'Args/C']}, maps:get(elem, ProductMap())),
                    ?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap()))]
           end)}.

pattern_then_type_parse_test_() ->
    {"A type can be pattern matched against types it creates",
     ?setup("type T\n"
            " | A -> A\n"
            "type Test -> T(T/A)",
            fun({ok, [Mod | _]}) ->
                    [?test('T/A', Mod:'Test'())]
            end)}.

pattern_type_test_() ->
    {"The domain of the type constructor `F` should depend entirely on it's input `a`",
     ?setup("type F\n"
            " | A -> B\n"
            " | B -> C\n"
            " | C -> A",
            fun({ok, [Mod | _]}) ->
                    [?test('F/B', Mod:'F'('F/A')),
                     ?test({sum, ['F/A', 'F/B', 'F/C']}, Mod:'F'('any'))]
            end)}.

pattern_variable1_test_() ->
    {"pattern matching with types allows variables",
     ?setup("type X\n"
            " | T -> T\n"
            " | t -> {t: t}",
            fun({ok, [Mod | _]}) ->
                    [?test('X/T', Mod:'X'('X/T')),
                     ?test(#{t := 'S'}, Mod:'X'('S'))]
            end)}.

pattern_variable2_test_() ->
    {"a pattern variable can be constrained to a type",
     ?setup("type Args -> (A | B | C)\n"
            "type X\n"
            " | Args/A -> Args/B\n"
            " | (t: Args) -> t",
            fun({ok, [Mod | _]}) ->
                    [?test('Args/B', Mod:'X'('Args/A')),
                     ?test('Args/C', Mod:'X'('Args/C'))]
            end)}.

pattern_dict_test_() ->
    {"When using a dict pattern, the variables are mapped on to the values associated with the product keys",
     ?setup("type Args -> (A | B | C)\n"
            "type Test\n"
            " | {a, b} -> (a | b)",
            fun({ok, [Mod | _]}) ->
                    Input1 = #{a => 'Args/A', b => 'Args/B'},
                    Actual1 = Mod:'Test'(Input1),
                    Expected1 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},

                    % Test behavior with unused key
                    Input2 = #{a => 'Args/A', b => 'Args/B', c => 'Args/C'},
                    Actual2 = Mod:'Test'(Input2),
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
     ?setup("type Args -> (A | B | C)\n"
            "type Test\n"
            " | {a: s, b} -> (s | b)",
            fun({ok, [Mod | _]}) ->
                    Input = #{a => 'Args/A', b => 'Args/B', c => 'Args/C'},
                    Actual = Mod:'Test'(Input),
                    Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

pattern_dict_dict_test_() ->
    {"Same as above, but this time the key value pair contains a pattern",
     ?setup("type Args -> (A | B | C)\n"
            "type Test\n"
            " | {a, b: Args/C} -> a\n"
            " | {a: {a}, b} -> (a | b)",
            fun({ok, [Mod | _]}) ->
                    Input1 = #{a => #{a => 'Args/A'}, 
                                        b => 'Args/B', 
                                        c => 'Args/C'},
                    Actual1 = Mod:'Test'(Input1),
                    Expected1 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},

                    Input2 = #{a => 'Args/A', 
                                        b => 'Args/C'},
                    [?test(none, domain:diff(Expected1, Actual1)),
                     ?test('Args/A', Mod:'Test'(Input2))]
            end)}.

pattern_dict_sum_test_() ->
    {"'Args' in second line of pattern match should be expanded to "
     "all members ('A', 'B', 'C') of the Args type and a clause "
     "should be generated for each",
     ?setup("type Args -> (A | B | C)\n"
            "type Test\n"
            " | {a, b: (Args/A | Args/B)} -> a\n"
            " | (t: {a, b: Args}) -> t",
            fun({ok, [Mod | _]}) ->
                    Input1 = #{a => 'Args/A', 
                                        b => 'Args/B'},
                    Actual1 = Mod:'Test'(Input1),
                    Expected1 = 'Args/A',

                    Input2 = #{a => 'Args/A', 
                                        b => 'Args/C'},
                    Expected2 = Input2,%{sum, ordsets:from_list(['Args/A', 'Args/C'])},
                    [?test(none, domain:diff(Expected1, Actual1)),
                     ?test(none, domain:diff(Expected2, Mod:'Test'(Input2)))]
            end)}.

pattern_tagged_test_() ->
    {"A tagged value can be pattern matched by prefacing the pattern with the tag and a colon",
     ?setup("type Args -> T: {a: A, b: B}\n"
            "type Test\n"
            " | (Args/T: {a: s, b}) -> (s | b)",
            fun({ok, [Mod | _]}) ->
                    Actual = Mod:'Test'('Args'),
                    Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
                    [?test(none, domain:diff(Expected, Actual))]
            end)}.

pattern_no_matching_test_() ->
    {"when no matching pattern is found an error should be returned at compile time",
     ?setup("type Test\n"
            " | T -> (T | S)",
            fun({ok, [Mod | _]}) ->
                    [?testError({domains_do_not_intersect, 'Test/T', 'Test/S'}, Mod:'Test'('Test/S'))]
            end)}.

pattern_sum_test_() ->
    {"A pattern with sums should match against all members of said sum",
     ?setup("type Args -> (A | B | C)\n"
            "type Test\n"
            " | Args -> Matched\n"
            " | _ -> Unmatched",
            fun({ok, [Mod | _]}) ->
                    [?test('Test/Matched', Mod:'Test'('Args/A')),
                     ?test('Test/Matched', Mod:'Test'('Args/B')),
                     ?test('Test/Matched', Mod:'Test'('Args/C')),
                     ?test('Test/Unmatched', Mod:'Test'('Test/Matched'))]
            end)}.

pattern_sum_tagged_test_() ->
    {"A pattern with a sum of product should expand to match all products",
     ?setup("type Args -> A: ({b: B} | {c: C})\n"
            "type Test\n"
            " | (Args/A: {b}) -> b\n"
            " | (Args/A: {c}) -> c\n"
            " | _ -> Unmatched",
            fun({ok, [Mod | _]}) ->
                    [?test('Args/B', Mod:'Test'({tagged, 'Args/A', #{b => 'Args/B'}})),
                     ?test('Args/C', Mod:'Test'({tagged, 'Args/A', #{c => 'Args/C'}})),
                     ?test('Test/Unmatched', Mod:'Test'({tagged, 'Args/A', #{a => 'Args/A'}})),
                     ?test('Test/Unmatched', Mod:'Test'({tagged, 'Args/B', #{b => 'Args/B'}}))]
            end)}.

pattern_product_sum_test_() ->
    {"Pattern matching should match all members ('Y', 'Z') of type 'X' and generate a clause for each",
     ?setup("type X -> (Y | Z)\n"
            "type P -> {x: X, xx: X}\n"
            "type Test\n"
            " | P -> Matched\n"
            " | _ -> Unmatched",
            fun({ok, [Mod | _]}) ->
                    [?test('Test/Matched', Mod:'Test'(#{x => 'X/Y', xx => 'X/Y'})),
                     ?test('Test/Matched', Mod:'Test'(#{x => 'X/Z', xx => 'X/Y'})),
                     ?test('Test/Matched', Mod:'Test'(#{x => 'X/Y', xx => 'X/Z'})),
                     ?test('Test/Matched', Mod:'Test'(#{x => 'X/Z', xx => 'X/Z'})),
                     ?test('Test/Unmatched', Mod:'Test'('Test/Matched'))]
            end)}.

pattern_tagged_pair_test_() ->
    {"A key value pair outside a dict pattern constraints the variable (key
     part) to a member of the domain of the value part",
     ?setup("type Args -> (A | B | C)\n"
            "type Test\n"
            " | (a: Args/A) -> a\n"
            " | (a: Args/B) -> Args/C",
            fun({ok, [Mod | _]}) ->
                    [?test('Args/A', Mod:'Test'('Args/A')),
                     ?test('Args/C', Mod:'Test'('Args/B'))]
            end)}.

pattern_tagged_sum_list_test_() ->
    {"Instead of multiple patterns, a sum can also be expressed within a"
     "pattern to cover several valid values",
     ?setup("type Args -> (A | B | C)\n"
            "type Test\n"
            " | (a: (Args/A | Args/B)) -> a\n"
            " | (a: Args/C) -> Args/B",
            fun({ok, [Mod | _]}) ->
                    [?test('Args/A', Mod:'Test'('Args/A')),
                     ?test('Args/B', Mod:'Test'('Args/C'))]
            end)}.

% Disabling: I'm not sure I see a need to support subtype application
tagged_pair_in_pattern_test_() ->
    {"When a subtype is defined after a pattern it can still be called independently",
     ?setup("type Test -> Blip/Blop(T)\n"
            "type Blip\n"
            " | value -> Blop: {key: value}",
            fun({ok, [Mod | _]}) ->
                    Expected = {tagged, 'Blip/Blop',
                                #{key => 'Test/T'}},
                    [?test(none, domain:diff(Expected, Mod:'Test'()))]
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
    {"In a type application, the number of arguments should be compared with
      the number of type " "arguments. We normally count type arguments as the
      number of variables present in the type " "body, but for type definitions,
      we want to make sure we count the number of function parameters instead",
     ?setup("module test { BlahOption }\n"
            "type Option a b -> (a | None)\n"
            "type BlahOption -> Option(Option/None, Option/None)",
            fun({ok, [Mod | _]}) ->
                    [?test('Option/None', Mod:'BlahOption'())]
            end)}.

type_redefinition_test_() ->
    {"When we redefine one type as another type, the new type returns the function domain: "
     "`F(args)` where `F(args)` is the function of the original type", 
     ?setup("module test { Test }
            type Option a -> (a | None)
            type RedefOption -> Option
            type Test -> RedefOption(T)",
            fun({ok, [Mod | _]}) ->
                    [?test({sum, ['Option/None', 'Test/T']}, Mod:'Test'())]
            end)}.

type_redefinition_args_test_() ->
    {"If we redefine a type and then later call the original type, we want to check that the "
     "number of arguments it expects hasn't changed because of the redefinition",
     ?setup("module test { BlahOption }\n"
            "type BlipOption -> Option\n"
            "type Option a -> (a | None)\n"
            "type BlupOption -> Option\n"
            "type BlahOption -> Option(Option/None)",
            fun({ok, [Mod | _]}) ->
                    [?test('Option/None', Mod:'BlahOption'())]
            end)}.

qualified_symbol_pattern_atom_test_() ->
    {"When a pattern contains a qualified type constant (e.g. a type from a
     different module) it should match against it",
     ?setup("module test { T }\n"
            "type T\n"
            " | Boolean/False -> Falsy\n"
            " | Boolean/True -> Truthy",
            #{import_kind_libraries => true},
            fun({ok, _}) ->
                    [?test('T/Falsy', test:'T'('Boolean/False'))]
            end)}.

qualified_symbol_pattern_sum_test_() ->
    {"When a pattern contains a qualified type constant (e.g. a type from a
     different module) it should match against it",
     ?setup("module test { T }
             type T
               | Boolean -> Falsy
               | _ -> Truthy",
            #{import_kind_libraries => true},
            fun({ok, _}) ->
                    [?test('T/Falsy', test:'T'('Boolean/False')),
                     ?test('T/Falsy', test:'T'('Boolean/True')),
                     ?test('T/Truthy', test:'T'('T/Falsy'))]
            end)}.

qualified_symbol_undefined_arity_pattern_test_() ->
    {"type constructors that take one or more arguments can't be used as patterns on their own",
     ?setup("module test { T }
             type T
              | Option -> Falsy
              | Truthy -> Truthy",
            #{import_kind_libraries => true},
            fun({ok, _}) ->
                    [?testError({wrong_arity, 'kind/prelude', 'Option', 0, 1}, test_domain:'T'('T/Falsy'))]
            end)}.

call_qualified_symbol_with_args_test_() ->
    {"A qualified type can be evaluated alone and as part of a type
     application. When evaluated as part of a type application, the underlying
     domain function of the types should be called with the arguments",
     ?setup("import kind/prelude\n"
            "module test { Test }\n"
            "type Test -> kind/prelude/Option(Boolean/True)",
            #{import_kind_libraries => true},
            fun({ok, _}) ->
                    Actual = test:'Test'(),
                    [?test(none, domain:diff({sum, ordsets:from_list(['Boolean/True', 'Option/Nil'])}, Actual))]
            end)}.

var_application_in_type_def_test_() ->
    {"A type constructor can call non-type functions to construct types",
     ?setup("import erlang\n"
            "module test { Test }\n"
            "type Test a -> a.match(| True -> False\n"
            "                       | False -> True)",
            #{import_kind_libraries => true},
            fun({ok, _}) ->
                    [?test('Boolean/False', test:'Test'('Boolean/True'))]
            end)}.

pattern_type_application_test_() ->
    {"A type application inside a pattern is evaluated against the resulting domain",
     ?setup("module test { Test }\n"
            "type Test\n"
            "  | Boolean.Option  -> True\n"
            "  | _               -> False",
            #{import_kind_libraries => true},
            fun({ok, _}) ->
                    [?test('Boolean/True', test:'Test'('Option/Nil')),
                     ?test('Boolean/True', test:'Test'('Boolean/True')),
                     ?test('Boolean/True', test:'Test'('Boolean/False')),
                     ?test('Boolean/False', test:'Test'('_'))]
            end)}.

pattern_local_application_test_() ->
    {"Apply a local type constructor in a pattern should pattern match against the result",
     ?setup("module test { Test }
             type F a -> a
             type Test
               | True.F -> True",
            #{import_kind_libraries => true},
            fun({ok, [Mod | _]}) ->
                    [?test('Boolean/True', test_domain:'Test'('Boolean/True'))]
            end)}.

recursive_wrong_number_of_arguments_1_test_() ->
    {"A recursive type should be checked for number of arguments",
     ?setup("module test { Tree }
             import Tree/_
             type Tree a -> (Leaf | Node: {left: Tree(a), value: a, right: Tree(a)})",
            fun({ok, _}) ->
                    [?test({sum,['Tree/Leaf', {tagged,'Tree/Node',{recur,_}}]}, test:'Tree'('Int'))]
            end)}.

recursive_wrong_number_of_arguments_2_test_() ->
    {"A recursive type should be checked for number of arguments",
     ?setup("module test { Tree }
             type Tree a -> (Leaf | Node: {left: Tree(a), value: a, right: Tree(a)})",
            fun({ok, _}) ->
                    [?test({sum,['Tree/Leaf', {tagged,'Tree/Node',{recur,_}}]}, test:'Tree'('Int'))]
            end)}.

param_pattern_test_() ->
    {"Type constructors accept patterns for parameters",
     ?setup("module test { T }
             import erlang/+
             type T {key1: a, key2: b} -> (a | b)",
            fun({ok, _}) ->
                    [?test({sum, [1, 2]}, test:'T'(#{key1 => 1, key2 => 2}))]
            end)}.

top_level_function_call_in_type_test_() ->
    {"A variable referencing a top-level function should not be treated as a free variable",
     ?setup("module test { T}
             def t -> 'literal'
             type T -> t",
            fun({ok, _}) ->
                    [?test('literal', test:'T'())]
            end)}.


fake_pair_test_() ->
    {"It should be possible to construct the tagged type using a type function",
     ?setup("module test { main }
             import symbol/{tag, ctx}
             import erlang/{list_to_tuple: tuple}

             macro :- t1 t2 -> ['tagged', ctx(t1), [tag(t1)], t2].tuple
             type T -> Tag :- {key1: T1, key2: T2}

             def main -> T",
            fun({ok, _}) ->
                    [?test({tagged, 'T/Tag', #{key1 := 'T/T1', key2 := 'T/T2'}}, test:main())]
            end)}.

%multiple_tagged_pair_in_pattern_test_() ->
%     Code = "type Test a\n"
%            " | {value} -> Blop: {key: value}\n"
%            " | value -> Blop: {key: value}",
%    RunAsserts = fun(Err) -> ?errorMatch({multiple_definitions, 'Test/Blop'}, Err) end,
%    run(Code, RunAsserts).
