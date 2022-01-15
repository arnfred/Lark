-module(end_to_end_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(default_options, #{target => [lark, test], include_lark_libraries => false}).

-define(setup(Code, Tests), {setup, fun() -> lark:load(Code, ?default_options) end, fun clean/1, Tests}).
-define(setup(Code, Options, Tests), {setup, fun() -> lark:load(Code, maps:merge(?default_options, Options)) end, fun clean/1, Tests}).

clean({error, _}) -> noop;
clean({ok, Module}) -> true = code:soft_purge(Module),
                       true = code:delete(Module).

sum_type_boolean_test_() ->
    {"sum domain",
    ?setup("module lark/test (export {boolean}
                      def boolean -> (True | False))",
           fun({ok, Mod}) ->
                   [?test({sum, ['lark/test/boolean/False', 'lark/test/boolean/True']}, Mod:'boolean'())]
           end)}.

product_test_() ->
    {"product domain",
    ?setup("module lark/test (export {p}
                def p -> {a: A, b: B})",
           fun({ok, Mod}) ->
                   [?test(#{a := 'lark/test/p/A', b := 'lark/test/p/B'}, Mod:p())]
           end)}.

tagged_test_() ->
    {"tagged domains are value domains. This means the when compiled `K` is
     stripped and only the value `{a: A, b: B}` remains",
     ?setup("module lark/test (
                 export {p}
                 def p -> (K: {a: A, b: B}))",
            fun({ok, Mod}) ->
                    [?test(#{a := 'lark/test/p/A', b := 'lark/test/p/B'}, Mod:p())]
            end)}.

keyword_test_() ->
    {"When a definition creates keywords, they are usable outside the definition",
     ?setup("module lark/test (
               export {id}
               def id a -> a
               def t -> (A | B))",
            fun({ok, Mod}) ->
                    [?test('lark/test/t/A', Mod:'id'('lark/test/t/A'))]
            end)}.

sum_var_test_() ->
    {"We can define new domain keywords based on existing types arbitrarily so.",
     ?setup("module lark/test (
                 export {f}
                 def boolean -> (True | False)
                 def option a -> (a | None)
                 def f -> option(boolean))",
            fun({ok, Mod}) ->
                    [?test({sum, ['lark/test/boolean/False', 'lark/test/boolean/True', 'lark/test/option/None']},
                           Mod:'f'())]
            end)}.

product_sum_test_() ->
    {"domain constructors without arguments can be used in place of a specific
      values to cover over the entire domain",
     ?setup("module lark/test (
                 export {elems}
                 def args -> (A | B | C)
                 def elems -> {elem: args})",
            fun({ok, Mod}) ->
                    [?test(#{elem := {sum, ['lark/test/args/A',
                                            'lark/test/args/B',
                                            'lark/test/args/C']}},
                           Mod:elems())]
            end)}.

tagged_constructor_test_() ->
    {"The parameter order in a subtype constructor is ambigious. In lark it's
      defined as the order the type variables appear in in a left-biased depth
      first traversel (e.g. the order that you would read them in)",
     ?setup("module lark/test (
                 export {order, order/T}
                 def order a b c -> (T: [c, b, a])
                 def args -> (A | B | C))",
            fun({ok, Mod}) ->
                    [?test(['lark/test/args/C', 'lark/test/args/B', 'lark/test/args/A'],
                           Mod:'order/T'(['lark/test/args/C', 'lark/test/args/B', 'lark/test/args/A']))]
            end)}.



%application_top_level_f_test_() ->
%    {"Call type function using domain function",
%     ?setup("type Option a -> (a | None)\n"
%            "type BlahOption -> Option(Blah)",
%            fun({ok, [Mod | _]}) ->
%                    [?test({sum, ['BlahOption/Blah', 'Option/None']},
%                           Mod:'BlahOption'())]
%            end)}.
%
%application_wrong_number_of_args_test_() ->
%    {"Check that we see an error when the wrong number of arguments is given to a type constructor",
%     ?setup("module blah { BlahOption }
%             type Option a -> (a | None)
%             type BlahOption -> Option(Option/None, Option/None)",
%            fun(Error) ->
%                    [?testError({wrong_arity, 'Option', 2, 1}, Error)]
%            end)}.
%
%application_inner_level_f_test_() ->
%    {"Same as above for subtype constructor",
%     ?setup("type Option a -> (P: {a: a} | None | O: P(a))\n",
%            fun({ok, [Mod | _]}) ->
%                    [?test({sum, ['Option/None',
%                                  {tagged, 'Option/O',
%                                   {tagged, 'Option/P',
%                                    #{a := 'Option/None'}}},
%                                  {tagged, 'Option/P',
%                                   #{a := 'Option/None'}}]},
%                           Mod:'Option'('Option/None'))]
%            end)}.
%
%application_first_order_type_test_() ->
%    {"Similar to generics we can call a type constructor from another type constructor",
%     ?setup("type Args -> (Arg1 | Arg2)\n"
%            "type Option a -> (None | a)\n"
%            "type AnyOption f a -> f(a)",
%            fun({ok, [Mod | _]}) ->
%                    [?test({sum, ['Args/Arg1', 'Option/None']},
%                           % The anonymous function takes two arguments because
%                           % it's given the current domain stack as the first
%                           % parameter
%                           Mod:'AnyOption'(fun Mod:'Option'/1, 'Args/Arg1'))]
%            end)}.
%
%application_first_order_called_by_type_test_() ->
%    {"Call type with another type constructor as function and have that applied
%     to third type" "Mostly applications to do with Types are clearly a type
%     constructor application, but when a type variable containing a type
%     constructor is used instead, it's indistinguishable from a normal
%     application.",
%     ?setup("type Args -> (Arg1 | Arg2)\n"
%    	    "type Switch\n"
%    	    " | Args/Arg1 -> Args/Arg2\n"
%    	    " | Args/Arg2 -> Args/Arg1\n"
%    	    "type Apply f a -> f(a)\n"
%    	    "type Test -> Apply(Switch, Args/Arg1)",
%    	    fun({ok, [Mod | _]}) ->
%                [?test('Args/Arg2', Mod:'Test'())]
%            end)}.
%
%application_product_test_() ->
%    {"Subtype constructors make a nice equivalent to case classes with the same parent in Scala",
%     ?setup("type P a b -> {a: a, b: b}\n"
%            "type Test -> P(A, B)",
%            fun({ok, [Mod | _]}) ->
%                    Expected = #{a => 'Test/A', b => 'Test/B'},
%                    [?test(Expected, Mod:'Test'())]
%            end)}.
%
%
%recursion_top_level_f_test_() ->
%    {"Recursive types are essential for data structures like linked lists and
%      trees. Non-recursive domains are strictly evaluated, but recursive domains
%      replace the recursion with a `Recur` domain type containing a
%      function which can be evaluated to traverse the structure.",
%     ?setup("type List a -> (Nil | Cons: {head: a, tail: List(a)})",
%            fun({ok, [Mod | _]}) ->
%                    Actual = Mod:'List'('List/Nil'),
%                    {_, [_, {_, _, {recur, ProductMap}}]} = Actual,
%                    [?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
%                     ?test('List/Nil', maps:get(head, ProductMap())),
%                     ?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap()))]
%            end)}.
%
%recursion_top_level_non_function_test_() ->
%    {"Test that recursion works for a type constructor that doesn't take any arguments",
%    ?setup("type Args -> (A | B | C)\n"
%           "type List -> (Nil | Cons: {elem: Args, tail: List})",
%           fun({ok, [Mod | _]}) ->
%                   Actual = Mod:'List'(),
%                   {_, [_, {_, _, {recur, ProductMap}}]} = Actual,
%                   [?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
%                    ?test({sum, ['Args/A', 'Args/B', 'Args/C']}, maps:get(elem, ProductMap())),
%                    ?test({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap()))]
%           end)}.
%
%pattern_then_type_parse_test_() ->
%    {"A type can be pattern matched against types it creates",
%     ?setup("type T\n"
%            " | A -> A\n"
%            "type Test -> T(T/A)",
%            fun({ok, [Mod | _]}) ->
%                    [?test('T/A', Mod:'Test'())]
%            end)}.
%
%pattern_type_test_() ->
%    {"The domain of the type constructor `F` should depend entirely on it's input `a`",
%     ?setup("type F\n"
%            " | A -> B\n"
%            " | B -> C\n"
%            " | C -> A",
%            fun({ok, [Mod | _]}) ->
%                    [?test('F/B', Mod:'F'('F/A')),
%                     ?test({sum, ['F/A', 'F/B', 'F/C']}, Mod:'F'('any'))]
%            end)}.
%
%pattern_variable1_test_() ->
%    {"pattern matching with types allows variables",
%     ?setup("type X\n"
%            " | T -> T\n"
%            " | t -> {t: t}",
%            fun({ok, [Mod | _]}) ->
%                    [?test('X/T', Mod:'X'('X/T')),
%                     ?test(#{t := 'S'}, Mod:'X'('S'))]
%            end)}.
%
%pattern_variable2_test_() ->
%    {"a pattern variable can be constrained to a type",
%     ?setup("type Args -> (A | B | C)\n"
%            "type X\n"
%            " | Args/A -> Args/B\n"
%            " | (t: Args) -> t",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Args/B', Mod:'X'('Args/A')),
%                     ?test('Args/C', Mod:'X'('Args/C'))]
%            end)}.
%
%pattern_dict_test_() ->
%    {"When using a dict pattern, the variables are mapped on to the values associated with the product keys",
%     ?setup("type Args -> (A | B | C)\n"
%            "type Test\n"
%            " | {a, b} -> (a | b)",
%            fun({ok, [Mod | _]}) ->
%                    Input1 = #{a => 'Args/A', b => 'Args/B'},
%                    Actual1 = Mod:'Test'(Input1),
%                    Expected1 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
%
%                    % Test behavior with unused key
%                    Input2 = #{a => 'Args/A', b => 'Args/B', c => 'Args/C'},
%                    Actual2 = Mod:'Test'(Input2),
%                    Expected2 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
%                    [?test(none, domain:diff(Expected1, Actual1)),
%                     ?test(none, domain:diff(Expected2, Actual2))]
%            end)}.
%
%pattern_dict_pair_test_() ->
%    {"When a dict pattern contains a key value pair, the domain under the key
%     is matched against the value pattern. In this test the value pattern is a
%                                           variable so the domain under key `a`
%                                           should be assigned to type variable
%                                           `s`",
%     ?setup("type Args -> (A | B | C)\n"
%            "type Test\n"
%            " | {a: s, b} -> (s | b)",
%            fun({ok, [Mod | _]}) ->
%                    Input = #{a => 'Args/A', b => 'Args/B', c => 'Args/C'},
%                    Actual = Mod:'Test'(Input),
%                    Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
%                    [?test(none, domain:diff(Expected, Actual))]
%            end)}.
%
%pattern_dict_dict_test_() ->
%    {"Same as above, but this time the key value pair contains a pattern",
%     ?setup("type Args -> (A | B | C)\n"
%            "type Test\n"
%            " | {a, b: Args/C} -> a\n"
%            " | {a: {a}, b} -> (a | b)",
%            fun({ok, [Mod | _]}) ->
%                    Input1 = #{a => #{a => 'Args/A'},
%                                        b => 'Args/B',
%                                        c => 'Args/C'},
%                    Actual1 = Mod:'Test'(Input1),
%                    Expected1 = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
%
%                    Input2 = #{a => 'Args/A',
%                                        b => 'Args/C'},
%                    [?test(none, domain:diff(Expected1, Actual1)),
%                     ?test('Args/A', Mod:'Test'(Input2))]
%            end)}.
%
%pattern_dict_sum_test_() ->
%    {"'Args' in second line of pattern match should be expanded to "
%     "all members ('A', 'B', 'C') of the Args type and a clause "
%     "should be generated for each",
%     ?setup("type Args -> (A | B | C)\n"
%            "type Test\n"
%            " | {a, b: (Args/A | Args/B)} -> a\n"
%            " | (t: {a, b: Args}) -> t",
%            fun({ok, [Mod | _]}) ->
%                    Input1 = #{a => 'Args/A',
%                                        b => 'Args/B'},
%                    Actual1 = Mod:'Test'(Input1),
%                    Expected1 = 'Args/A',
%
%                    Input2 = #{a => 'Args/A',
%                                        b => 'Args/C'},
%                    Expected2 = Input2,%{sum, ordsets:from_list(['Args/A', 'Args/C'])},
%                    [?test(none, domain:diff(Expected1, Actual1)),
%                     ?test(none, domain:diff(Expected2, Mod:'Test'(Input2)))]
%            end)}.
%
%pattern_tagged_test_() ->
%    {"A tagged value can be pattern matched by prefacing the pattern with the tag and a colon",
%     ?setup("type Args -> T: {a: A, b: B}\n"
%            "type Test\n"
%            " | (Args/T: {a: s, b}) -> (s | b)",
%            fun({ok, [Mod | _]}) ->
%                    Actual = Mod:'Test'('Args'),
%                    Expected = {sum, ordsets:from_list(['Args/A', 'Args/B'])},
%                    [?test(none, domain:diff(Expected, Actual))]
%            end)}.
%
%pattern_no_matching_test_() ->
%    {"when no matching pattern is found an error should be returned at compile time",
%     ?setup("type Test\n"
%            " | T -> (T | S)",
%            fun({ok, [Mod | _]}) ->
%                    [?testError({domains_do_not_intersect, 'Test/T', 'Test/S'}, Mod:'Test'('Test/S'))]
%            end)}.
%
%pattern_sum_test_() ->
%    {"A pattern with sums should match against all members of said sum",
%     ?setup("type Args -> (A | B | C)\n"
%            "type Test\n"
%            " | Args -> Matched\n"
%            " | _ -> Unmatched",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Test/Matched', Mod:'Test'('Args/A')),
%                     ?test('Test/Matched', Mod:'Test'('Args/B')),
%                     ?test('Test/Matched', Mod:'Test'('Args/C')),
%                     ?test('Test/Unmatched', Mod:'Test'('Test/Matched'))]
%            end)}.
%
%pattern_sum_tagged_test_() ->
%    {"A pattern with a sum of product should expand to match all products",
%     ?setup("type Args -> A: ({b: B} | {c: C})\n"
%            "type Test\n"
%            " | (Args/A: {b}) -> b\n"
%            " | (Args/A: {c}) -> c\n"
%            " | _ -> Unmatched",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Args/B', Mod:'Test'({tagged, 'Args/A', #{b => 'Args/B'}})),
%                     ?test('Args/C', Mod:'Test'({tagged, 'Args/A', #{c => 'Args/C'}})),
%                     ?test('Test/Unmatched', Mod:'Test'({tagged, 'Args/A', #{a => 'Args/A'}})),
%                     ?test('Test/Unmatched', Mod:'Test'({tagged, 'Args/B', #{b => 'Args/B'}}))]
%            end)}.
%
%pattern_product_sum_test_() ->
%    {"Pattern matching should match all members ('Y', 'Z') of type 'X' and generate a clause for each",
%     ?setup("type X -> (Y | Z)\n"
%            "type P -> {x: X, xx: X}\n"
%            "type Test\n"
%            " | P -> Matched\n"
%            " | _ -> Unmatched",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Test/Matched', Mod:'Test'(#{x => 'X/Y', xx => 'X/Y'})),
%                     ?test('Test/Matched', Mod:'Test'(#{x => 'X/Z', xx => 'X/Y'})),
%                     ?test('Test/Matched', Mod:'Test'(#{x => 'X/Y', xx => 'X/Z'})),
%                     ?test('Test/Matched', Mod:'Test'(#{x => 'X/Z', xx => 'X/Z'})),
%                     ?test('Test/Unmatched', Mod:'Test'('Test/Matched'))]
%            end)}.
%
%pattern_tagged_pair_test_() ->
%    {"A key value pair outside a dict pattern constraints the variable (key
%     part) to a member of the domain of the value part",
%     ?setup("type Args -> (A | B | C)\n"
%            "type Test\n"
%            " | (a: Args/A) -> a\n"
%            " | (a: Args/B) -> Args/C",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Args/A', Mod:'Test'('Args/A')),
%                     ?test('Args/C', Mod:'Test'('Args/B'))]
%            end)}.
%
%pattern_tagged_sum_list_test_() ->
%    {"Instead of multiple patterns, a sum can also be expressed within a"
%     "pattern to cover several valid values",
%     ?setup("type Args -> (A | B | C)\n"
%            "type Test\n"
%            " | (a: (Args/A | Args/B)) -> a\n"
%            " | (a: Args/C) -> Args/B",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Args/A', Mod:'Test'('Args/A')),
%                     ?test('Args/B', Mod:'Test'('Args/C'))]
%            end)}.
%
%% Disabling: I'm not sure I see a need to support subtype application
%tagged_pair_in_pattern_test_() ->
%    {"When a subtype is defined after a pattern it can still be called independently",
%     ?setup("type Test -> Blip/Blop(T)\n"
%            "type Blip\n"
%            " | value -> Blop: {key: value}",
%            fun({ok, [Mod | _]}) ->
%                    Expected = {tagged, 'Blip/Blop',
%                                #{key => 'Test/T'}},
%                    [?test(none, domain:diff(Expected, Mod:'Test'()))]
%            end)}.
%
%module_type_test_() ->
%    {"A type module should be created that can be called for any type member",
%     ?setup("module test { T }\n"
%            "type T -> {a: A, b: B}",
%            fun({ok, _}) ->
%                    [?test('T/A', 'test_T':'A'()),
%                     ?test('T/B', 'test_T':'B'())]
%            end)}.
%
%module_tagged_type_test_() ->
%    {"The created type module for `T` needs to contain a type function for the tagged type `F`",
%     ?setup("module test { T }\n"
%            "type T a -> F: {a: a}",
%            fun({ok, _}) ->
%                    [?test({tagged, 'T/F', #{a := 'Input'}}, 'test_T':'F'('Input'))]
%            end)}.
%
%unused_type_parameter_test_() ->
%    {"In a type application, the number of arguments should be compared with
%      the number of type " "arguments. We normally count type arguments as the
%      number of variables present in the type " "body, but for type definitions,
%      we want to make sure we count the number of function parameters instead",
%     ?setup("module test { BlahOption }\n"
%            "type Option a b -> (a | None)\n"
%            "type BlahOption -> Option(Option/None, Option/None)",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Option/None', Mod:'BlahOption'())]
%            end)}.
%
%type_redefinition_test_() ->
%    {"When we redefine one type as another type, the new type returns the function domain: "
%     "`F(args)` where `F(args)` is the function of the original type",
%     ?setup("module test { Test }
%            type Option a -> (a | None)
%            type RedefOption -> Option
%            type Test -> RedefOption(T)",
%            fun({ok, [Mod | _]}) ->
%                    [?test({sum, ['Option/None', 'Test/T']}, Mod:'Test'())]
%            end)}.
%
%type_redefinition_args_test_() ->
%    {"If we redefine a type and then later call the original type, we want to check that the "
%     "number of arguments it expects hasn't changed because of the redefinition",
%     ?setup("module test { BlahOption }\n"
%            "type BlipOption -> Option\n"
%            "type Option a -> (a | None)\n"
%            "type BlupOption -> Option\n"
%            "type BlahOption -> Option(Option/None)",
%            fun({ok, [Mod | _]}) ->
%                    [?test('Option/None', Mod:'BlahOption'())]
%            end)}.
%
%qualified_symbol_pattern_atom_test_() ->
%    {"When a pattern contains a qualified type constant (e.g. a type from a
%     different module) it should match against it",
%     ?setup("module test { T }\n"
%            "type T\n"
%            " | Boolean/False -> Falsy\n"
%            " | Boolean/True -> Truthy",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    [?test('T/Falsy', test:'T'('Boolean/False'))]
%            end)}.
%
%qualified_symbol_pattern_sum_test_() ->
%    {"When a pattern contains a qualified type constant (e.g. a type from a
%     different module) it should match against it",
%     ?setup("module test { T }
%             type T
%               | Boolean -> Falsy
%               | _ -> Truthy",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    [?test('T/Falsy', test:'T'('Boolean/False')),
%                     ?test('T/Falsy', test:'T'('Boolean/True')),
%                     ?test('T/Truthy', test:'T'('T/Falsy'))]
%            end)}.
%
%qualified_symbol_undefined_arity_pattern_test_() ->
%    {"type constructors that take one or more arguments can't be used as patterns on their own",
%     ?setup("module test { T }
%             type T
%              | Option -> Falsy
%              | Truthy -> Truthy",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    [?testError({wrong_arity, 'lark/prelude', 'Option', 0, 1}, test_domain:'T'('T/Falsy'))]
%            end)}.
%
%call_qualified_symbol_with_args_test_() ->
%    {"A qualified type can be evaluated alone and as part of a type
%     application. When evaluated as part of a type application, the underlying
%     domain function of the types should be called with the arguments",
%     ?setup("import lark/prelude\n"
%            "module test { Test }\n"
%            "type Test -> lark/prelude/Option(Boolean/True)",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    Actual = test:'Test'(),
%                    [?test(none, domain:diff({sum, ordsets:from_list(['Boolean/True', 'Option/Nil'])}, Actual))]
%            end)}.
%
%var_application_in_type_def_test_() ->
%    {"A type constructor can call non-type functions to construct types",
%     ?setup("import erlang\n"
%            "module test { Test }\n"
%            "type Test a -> a.match(| True -> False\n"
%            "                       | False -> True)",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    [?test('Boolean/False', test:'Test'('Boolean/True'))]
%            end)}.
%
%pattern_type_application_test_() ->
%    {"A type application inside a pattern is evaluated against the resulting domain",
%     ?setup("module test { Test }\n"
%            "type Test\n"
%            "  | Option(Boolean) -> True\n"
%            "  | _               -> False",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    [?test('Boolean/True', test:'Test'('Option/Nil')),
%                     ?test('Boolean/True', test:'Test'('Boolean/True')),
%                     ?test('Boolean/True', test:'Test'('Boolean/False')),
%                     ?test('Boolean/False', test:'Test'('_'))]
%            end)}.
%
%pattern_local_application_test_() ->
%    {"Apply a local type constructor in a pattern should pattern match against the result",
%     ?setup("module test { Test }
%             type F a -> a
%             type Test
%               | True.F -> True",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    [?test('Boolean/True', test_domain:'Test'('Boolean/True'))]
%            end)}.
%
%pattern_variable_application_test_() ->
%    {"Apply a type constructor to a variable should create a pattern based on this variable",
%     ?setup("module test { Test }
%             type Test a b -> a.match(
%               | Option(b)  -> True
%               | False      -> False)",
%            #{include_lark_libraries => true},
%            fun({ok, _}) ->
%                    [?test('Boolean/True', test_domain:'Test'({sum, ['Boolean/True', 'Option/Nil']}, 'Boolean/True')),
%                     ?test('Boolean/True', test_domain:'Test'({sum, ['Boolean/False', 'Option/Nil']}, 'Boolean/False')),
%                     ?test('Boolean/False', test_domain:'Test'('Boolean/False', 'Boolean/True'))]
%            end)}.
%
%recursive_wrong_number_of_arguments_1_test_() ->
%    {"A recursive type should be checked for number of arguments",
%     ?setup("module test { Tree }
%             import Tree/_
%             type Tree a -> (Leaf | Node: {left: Tree(a), value: a, right: Tree(a)})",
%            fun({ok, _}) ->
%                    [?test({sum,['Tree/Leaf', {tagged,'Tree/Node',{recur,_}}]}, test:'Tree'('Int'))]
%            end)}.
%
%recursive_wrong_number_of_arguments_2_test_() ->
%    {"A recursive type should be checked for number of arguments",
%     ?setup("module test { Tree }
%             type Tree a -> (Leaf | Node: {left: Tree(a), value: a, right: Tree(a)})",
%            fun({ok, _}) ->
%                    [?test({sum,['Tree/Leaf', {tagged,'Tree/Node',{recur,_}}]}, test:'Tree'('Int'))]
%            end)}.
%
%param_pattern_test_() ->
%    {"Type constructors accept patterns for parameters",
%     ?setup("module test { T }
%             import erlang/+
%             type T {key1: a, key2: b} -> (a | b)",
%            fun({ok, _}) ->
%                    [?test({sum, [1, 2]}, test:'T'(#{key1 => 1, key2 => 2}))]
%            end)}.
%
%top_level_function_call_in_type_test_() ->
%    {"A variable referencing a top-level function should not be treated as a free variable",
%     ?setup("module test { T}
%             def t -> 'literal'
%             type T -> t",
%            fun({ok, _}) ->
%                    [?test('literal', test:'T'())]
%            end)}.
%
%
%fake_pair_test_() ->
%    {"It should be possible to construct the tagged type using a type function",
%     ?setup("module test { main }
%             import symbol/{tag, ctx}
%             import erlang/{list_to_tuple: tuple}
%
%             macro :- t1 t2 -> ['tagged', ctx(t1), [tag(t1)], t2].tuple
%             type T -> Tag :- {key1: T1, key2: T2}
%
%             def main -> T",
%            fun({ok, _}) ->
%                    [?test({tagged, 'T/Tag', #{key1 := 'T/T1', key2 := 'T/T2'}}, test:main())]
%            end)}.
%
%%multiple_tagged_pair_in_pattern_test_() ->
%%     Code = "type Test a\n"
%%            " | {value} -> Blop: {key: value}\n"
%%            " | value -> Blop: {key: value}",
%%    RunAsserts = fun(Err) -> ?errorMatch({multiple_definitions, 'Test/Blop'}, Err) end,
%%    run(Code, RunAsserts).




%-module(typecheck_test).
%
%-include_lib("eunit/include/eunit.hrl").
%-include_lib("test/macros.hrl").
%
%-define(setup(Code, Tests), {setup, fun() -> lark:load(Code, #{include_lark_libraries => false}) end, fun clean/1, Tests}).
%
%clean({error, _}) -> noop;
%clean({ok, Modules}) -> clean(Modules);
%clean(Modules) ->
%    F = fun(M) -> true = code:soft_purge(M),
%                  true = code:delete(M)
%        end,
%    [F(M) || M <- Modules].
%
%simple_def_test_() ->
%    ?setup("module test { x }
%            import beam/erlang/+
%            def x (n: 5) -> n + 3",
%           fun({ok, _}) ->
%                   [?test(none, test_domain:x(lenient, 4)),
%                    ?testError({domains_do_not_intersect, 4, 5}, test_domain:x(normal, 4)),
%                    ?test(8, test_domain:x(any)),
%                    ?testError({domain_not_subset, any, 5}, test_domain:x(strict, any)),
%                    ?test(8, test_domain:x(5))]
%           end).
%
%application_test_() ->
%    ?setup("module test { app }
%            import beam/erlang/+
%            def f n -> 4 + n
%            def app m -> m.f",
%           fun({ok, _}) ->
%                   [?test(9, test_domain:app(strict, 5))]
%           end).
%                   
%
%first_order_function_test_() ->
%    ?setup("module test { first-order }
%            def first-order f -> f(3)",
%           fun({ok, _}) ->
%                   [?test(any, test_domain:'first-order'(any)),
%                    ?test(4, test_domain:'first-order'(fun(N) -> N + 1 end))]
%           end).
%
%run_xor_test_() ->
%    ?setup("module test { xor }
%            type Boolean -> (True | False)
%            def xor
%              | Boolean/True Boolean/False -> Boolean/True
%              | Boolean/False (b: Boolean/True) -> b
%              | _ _ -> Boolean/False",
%    fun({ok, _}) ->
%            Boolean = {sum, ['Boolean/False', 'Boolean/True']},
%            [?test(Boolean, test_domain:'xor'(any, any)),
%             ?test('Boolean/True', test_domain:'xor'('Boolean/True', 'Boolean/False')),
%             ?test(Boolean, test_domain:'xor'(Boolean, Boolean)),
%             ?test('Boolean/False', test_domain:'xor'('Boolean/True', 'Boolean/True'))]
%    end).
%
%direct_recursion_test_() ->
%    ?setup("module test { g }
%            type State -> (Start | Continue | Stop)
%            def g
%             | State/Start -> g(State/Continue)
%             | State/Continue -> g(State/Stop)
%             | (state: State/Stop) -> state",
%    fun({ok, _}) ->
%            [?test('State/Stop', test_domain:g('State/Stop')),
%             ?test('State/Stop', test_domain:g('State/Start'))]
%    end).
%
%infinite_recursion_test_() ->
%    ?setup("module test { recurse }
%            def recurse a -> recurse(a)",
%           fun({ok, _}) ->
%                   [?test({recur, _}, test_domain:recurse(3))]
%           end).
%
%infinite_co_recursion_test_() ->
%    ?setup("module test { f }
%            def f a -> g(a)
%            def g a -> h(a)
%            def h a -> f(a)",
%           fun({ok, _}) ->
%                   ?test({recur, _}, test_domain:f('_'))
%           end).
%
%application_error_test_() ->
%    ?setup("module test { f }
%            def f a -> a(f)",
%           fun({ok, _}) ->
%                   [?test(none, test_domain:f(lenient, {sum, [1,2]})),
%                    ?testError({expected_function_domain, {sum, [1,2]}}, test_domain:f(strict, {sum, [1,2]})),
%                    ?testError({expected_function_domain, {sum, [1,2]}}, test_domain:f(normal, {sum, [1,2]}))]
%           end).
%
%pair_values_refinement_test_() ->
%    ?setup("module test { f }
%            type Boolean -> (True | False)
%            def f t -> t: Boolean",
%           fun({ok, _}) ->
%                   [?test('Boolean/True', test_domain:f('Boolean/True')),
%                    ?test({sum, ['Boolean/False', 'Boolean/True']}, test_domain:f(lenient, 'any')),
%                    ?test({sum, ['Boolean/False', 'Boolean/True']}, test_domain:f(normal, 'any')),
%                    ?testError({pair_not_subset, any, {sum, ['Boolean/False', 'Boolean/True']}}, test_domain:f(strict, 'any'))]
%           end).
%
%
%pair_expressions_refinement_test_() ->
%    ?setup("module test { f }
%            type Boolean -> (True | False)
%            type Option a -> (a | None)
%            def id a -> a
%            def f t -> id(t): Option(Boolean)",
%           fun({ok, _}) ->
%                   Boolean = {sum, ['Boolean/False', 'Boolean/True']},
%                   Constraint = {sum, ordsets:from_list(['Boolean/True',
%                                                         'Boolean/False',
%                                                         'Option/None'])},
%                   [?test(Boolean, test_domain:f('Boolean')),
%                    ?testError({pair_not_subset, _, Constraint}, test_domain:f(normal, 'Option')),
%                    ?test(Constraint, test_domain:f(any)),
%                    ?testError({pair_not_subset, any, Constraint}, test_domain:f(strict, any))]
%           end).
%
%pass_type_function_as_atom_test_() ->
%    ?setup("module test { f }
%            type Boolean -> (True | False)
%            type Option a -> (a | None)
%            def f g -> g(Boolean)",
%           fun({ok, _}) ->
%                   Expected = {sum, ordsets:from_list(['Boolean/True',
%                                                       'Boolean/False',
%                                                       'Option/None'])},
%                   ?test(Expected, test_domain:f('Option'))
%           end).
%
%fun_clause_test_() ->
%    ?setup("module test { test }
%            type Args -> (A | B | C)
%            def ap f a -> f(a)
%            def test a -> ap(| Args/A -> Args/C
%                             | Args/B -> Args/C,
%                             a)",
%           fun({ok, _}) ->
%               [?test('Args/C', test_domain:test('Args/A')),
%                ?test('Args/C', test_domain:test(lenient, 'Args')),
%                ?test('Args/C', test_domain:test(normal, 'Args')),
%                ?testError({domain_not_subset, {sum, ['Args/A', 'Args/B', 'Args/C']},
%                                                   {sum, ['Args/A', 'Args/B']}},
%                           test_domain:test(strict, 'Args')),
%                ?test(none, test_domain:test(lenient, 'Args/C')),
%                ?testError({domains_do_not_intersect, 'Args/A', 'Args/C'},
%                           {domains_do_not_intersect, 'Args/B', 'Args/C'},
%                           test_domain:test(normal, 'Args/C')),
%                ?testError({domain_not_subset, 'Args/A', 'Args/C'},
%                           {domain_not_subset, 'Args/B', 'Args/C'},
%                           test_domain:test(strict, 'Args/C'))]
%           end).
%
%
%assignment_variable_test_() ->
%    ?setup("module test { test }
%            type Args -> (A | B | C)
%            def test a -> (val f = (| Args/A -> Args/B
%                                    | Args/B -> Args/C)
%                           f(a))",
%           fun({ok, _}) ->
%                   [?test('Args/B', test_domain:test('Args/A')),
%                    ?test({sum, ['Args/B', 'Args/C']}, test_domain:test({sum, ['Args/A', 'Args/B']}))]
%           end).
%
%assignment_pattern_test_() ->
%    ?setup("module test { test }
%            type Dict -> {a: (Ah | Oh), b: Buh}
%            def test input -> (val {a: (out: Dict/Ah)} = input, out)",
%           fun({ok, _}) ->
%                   [?test('Dict/Ah', test_domain:test('Dict')),
%                    ?test('Dict/Ah', test_domain:test(any)),
%                    ?testError({domains_do_not_intersect, 'Dict/Ah', #{a := 'Dict/Ah'}}, test_domain:test('Dict/Ah')),
%                    ?testError({domains_do_not_intersect, 'Dict/Ah', 'Dict/Oh'}, test_domain:test(#{a => 'Dict/Oh'}))]
%           end).
%
%assignment_nonexistent_key_test_() ->
%    ?setup("module test { test }
%           type Sum -> ({a: A} | {b: B})
%           def test input -> (val {a, b} = input, b)",
%           fun({ok, _}) ->
%                   [?testError({nonexistent_key, a, #{}},
%                               {nonexistent_key, b, #{}}, test_domain:test('Sum')),
%                    ?testError({nonexistent_key, b, #{}}, test_domain:test(#{a => 'Sum/A'})),
%                    ?test('Sum/B', test_domain:test(#{a => 'Sum/A', b => 'Sum/B'}))]
%           end).
%
%assignment_tagged_product_test_() ->
%    ?setup("module test { test }
%            type Tagged -> T: {a: A, b: B}
%            def test input -> (val (Tagged/T: {a: out}) = input, out)",
%           fun({ok, _}) ->
%                   [?test('Tagged/A', test_domain:test('Tagged')),
%                    ?testError({domains_do_not_intersect, {tagged, 'S', _}, {tagged, 'Tagged/T', _}},
%                               test_domain:test({tagged, 'S', #{a => 'Tagged/A'}}))]
%           end).
%
%tagged_application_test_() ->
%    ?setup("module test { f, g }
%            type Args -> (A | B | C)
%            type Tagged1 -> T: {a: Args, b: Args}
%            type Tagged2 -> T: {a: 4, b: Args}
%            def f a b -> Tagged1/T(a, b)
%            def g a -> Tagged2/T(a)",
%           fun({ok, _}) ->
%                   [?test({tagged, 'Tagged1/T', #{a := 'Args/A',
%                                                  b := 'Args/B'}}, test_domain:f('Args/A', 'Args/B')),
%                    ?testError({domains_do_not_intersect, {sum, ['Args/A', 'Args/B', 'Args/C']}, 5},
%                               test_domain:f('Args/C', 5)),
%                    ?test({tagged, 'Tagged2/T', #{a := 4,
%                                                  b := 'Args/B'}}, test_domain:g('Args/B')),
%                    ?testError({domains_do_not_intersect, {sum, ['Args/A', 'Args/B', 'Args/C']}, 4},
%                               test_domain:g(4))]
%           end).
%
%tagged_sum_application_test_() ->
%    ?setup("module test { f }
%            type Args -> (A | B | C)
%            type Pargs -> (A | B | C)
%            type Tagged -> T: ({k1: Args, k2: Args} | {k3: Pargs, k4: Pargs})
%            def f a -> Tagged/T(a)",
%           fun({ok, _}) ->
%                   [?test({tagged, 'Tagged/T', #{k1 := 'Args/A',
%                                                 k2 := 'Args/B'}}, test_domain:f(#{k1 => 'Args/A', k2 => 'Args/B'})),
%                    ?test({tagged, 'Tagged/T', #{k3 := 'Pargs/B',
%                                                 k4 := 'Pargs/C'}}, test_domain:f(#{k3 => 'Pargs/B', k4 => 'Pargs/C'}))]
%           end).
%
%wrong_arity_test_() ->
%    ?setup("module test { f }
%            def id a -> a
%            def f a -> id()",
%           fun(Err) ->
%                   [?testError({wrong_arity, {id, 1}, 0, 1}, Err)]
%           end).
%
%dict_duplicate_keys_test_() ->
%    ?setup("module test { f }
%            def f -> {k: 1, k: 2}",
%           fun(Err) ->
%                   [?testError({duplicate_keys, [k]}, Err)]
%           end).
%
%list_test_() ->
%    ?setup("module test { f }
%            type Args -> (A | B | C)
%            def f [a: Args, b: Args] -> [b, a]",
%           fun({ok, _}) ->
%                   AnyExpected = [{sum, ['Args/A', 'Args/B', 'Args/C']}, {sum, ['Args/A', 'Args/B', 'Args/C']}],
%                   [?test(AnyExpected, test_domain:f(any)),
%                    ?test(['Args/C', 'Args/A'], test_domain:f(['Args/A', 'Args/C'])),
%                    ?testError({domains_do_not_intersect, ['Args/A'], AnyExpected}, test_domain:f(['Args/A']))]
%           end).
%
%assignment_intersection_no_subset_test_() ->
%    ?setup("module test { test }
%            type Args -> (A | B | C)
%            def test input -> (val (a: (Args/A | Args/B)) = input, a)",
%           fun({ok, _}) ->
%                   [?test('Args/A', test_domain:test('Args/A')),
%                    ?test({sum, ['Args/A', 'Args/B']}, test_domain:test('Args'))]
%           end).
%
%
%assignment_narrowing_of_expression_domain_by_pattern_test_() ->
%    ?setup("module test { test }
%            type Args -> (A | B | C)
%            def test input -> (val (a: Args/A) = input, input)",
%           fun({ok, _}) ->
%                   [?test({sum, ['Args/A', 'Args/B', 'Args/C']}, test_domain:test(normal, 'Args')),
%                    ?testError({domain_not_subset, {sum, ['Args/A', 'Args/B', 'Args/C']}, 'Args/A'},
%                               test_domain:test(strict, 'Args'))]
%           end).
%
%match_single_arg_test_() ->
%    ?setup("module test { test }
%            type Args -> (A | B | C)
%            def match a f -> f(a)
%            def test input -> input.match(| Args/A -> Args/B
%                                          | Args/B -> Args/C)",
%           fun({ok, _}) ->
%                   [?test('Args/B', test_domain:test('Args/A')),
%                    ?testError({domains_do_not_intersect, 'Args/A', 'Args/C'},
%                               {domains_do_not_intersect, 'Args/B', 'Args/C'},
%                               test_domain:test('Args/C')),
%                    ?testError({domain_not_subset,
%                                {sum, ['Args/A', 'Args/B', 'Args/C']},
%                                {sum, ['Args/A', 'Args/B']}},
%                               test_domain:test('strict', 'Args'))]
%           end).
%
%pattern_recursive_lookup_test_() ->
%    ?setup("module test { last }
%            type Args -> (A | B | C)
%            type List -> (Nil | Cons: {elem: Args, tail: List})
%            def last
%             | (List/Cons: {elem, tail: List/Nil}) -> elem
%             | (List/Cons: {tail}) -> tail.last()",
%           fun({ok, _}) ->
%                   ListDomain = {tagged, 'List/Cons',
%                                 #{elem => 'Args/A',
%                                   tail => {tagged, 'List/Cons',
%                                            #{elem => 'Args/A',
%                                              tail => {tagged, 'List/Cons',
%                                                       #{elem => 'Args/A',
%                                                         tail => 'List/Nil'}}}}}},
%                   [?test('Args/A', test_domain:last(ListDomain))]
%           end).
%
%
