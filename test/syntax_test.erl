-module(syntax_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(setup(Code, Tests), {setup, fun() -> kind:load(Code, #{sandboxed => false}) end, fun clean/1, Tests}).

clean({error, _}) -> noop;
clean({ok, Modules}) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].

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

pattern_match_expr_syntax1_test_() ->
    ?setup("module kind/test { test3 }\n"
           "def test3 a -> a.match( | False -> True\n"
           "                        | True -> False)",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:test3('Boolean/False'))
           end).

pattern_match_expr_syntax2_test_() ->
    ?setup("module kind/test { test4 }\n"
           "def test4 a -> a.match(\n"
           "  | False -> True\n"
           "  | True -> False)",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:test4('Boolean/False'))
           end).

anonymous_function1_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a)\n"
           "def blap a -> a.blip( | False -> True\n"
           "                      | True -> False)",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blap('Boolean/True')) end).

anonymous_function2_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a)\n"
           "def blap a -> a.blip(( | arg -> arg))",
           fun({ok, _}) -> ?test(whatevs, kind_test:blap(whatevs)) end).

anonymous_function3_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a, a)\n"
           "def blap a -> a.blip(| arg1 False -> False\n"
           "                     | arg1 True  -> arg1)",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:blap('Boolean/True')) end).

multiple_anonymous_functions1_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f g -> f(g(a))\n"
           "def blap a -> a.blip((| _ -> False),\n"
           "                     (| False -> True\n"
           "                      | True -> False))",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blap('Boolean/True')) end).

multiple_anonymous_functions2_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f g -> f(g(a))\n"
           "def blap a -> a.blip((| True -> False\n"
           "                      | False -> True),\n"
           "                     (| _ -> False))",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:blap('whatevs')) end).

infix_2_test_() ->
    ?setup("module test { main }
            import erlang/<
            def main a b -> a < b",
           fun({ok, _}) -> ?test('true', test:main(4, 5)) end).

infix_3_test_() ->
    ?setup("module test { main }
            import erlang/{<, +}
            def main a b -> (val c = a
                             val d = b
                             (c < d))",
           fun({ok, _}) -> ?test('true', test:main(4, 5)) end).

infix_4_test_() ->
    ?setup("module test { main }
            import erlang/{<}
            def main a b -> (a < b)",
           fun({ok, _}) -> ?test('true', test:main(4, 5)) end).

infix_5_test_() ->
    ?setup("module test { main }
            import erlang/{<,+}
            def f
              | 'true' -> True
              | 'false' -> False
            def main a b -> (a < b).f",
           fun({ok, _}) -> ?test('Boolean/True', test:main(4, 5)) end).

infix_6_test_() ->
    ?setup("module test { main }
            import erlang/{<, +}
            def f
              | 'true' _ -> True
              | 'false' q -> q
            def main a b -> (a < b).f(a + b)",
           fun({ok, _}) -> ?test(9, test:main(5, 4)) end).

infix_7_test_() ->
    ?setup("module test { main }
            import erlang/{<}
            def main a b -> (a < b).match(|'true' -> True
                                          |'false' -> False)",
           fun({ok, _}) -> ?test('Boolean/True', test:main(4, 5)) end).

operator_precedence_1_test_() ->
    ?setup("module test { main }
            import erlang/{+,*}
            def main -> 1 + 2 * 3",
           fun({ok, _}) -> ?test(7, test:main()) end).

operator_precedence_2_test_() ->
    ?setup("module test { main }
            import erlang/+
            import math/pow
            def ^ a b -> a.pow(b)
            def main -> 1 + 2 ^ 3",
           fun({ok, _}) -> ?test(9.0, test:main()) end).


operator_precedence_3_test_() ->
    ?setup("module test { main }
            import erlang/{<, +}
            def main a b -> a < (b + 3)",
           fun({ok, _}) -> ?test('true', test:main(4, 3)) end).

operator_precedence_4_test_() ->
    ?setup("module test { main }
            import erlang/{<,>,==}
            def main -> 1 < 2 == 3 > 2",
           fun({ok, _}) -> ?test('true', test:main()) end).

operator_precedence_5_test_() ->
    ?setup("module test { main }
            import erlang/{apply}
            def // n m -> apply('erlang', '/', [n, m])
            def main -> (12 // 2) // 6",
           fun({ok, _}) -> ?test(1.0, test:main()) end).

operator_precedence_6_test_() ->
    ?setup("module test { main }
            import erlang/{-,apply}
            def // n m -> apply('erlang', '/', [n, m])
            def main -> 12 - 2 // 4",
           fun({ok, _}) -> ?test(11.5, test:main()) end).

operator_precedence_7_test_() ->
    ?setup("module test { main }
            import erlang/{*,apply}
            import math/pow
            def // n m -> apply('erlang', '/', [n, m])
            def ^ n m -> n.pow(m)
            def main -> 2 ^ 2 * 3 // 6",
           fun({ok, _}) -> ?test(2.0, test:main()) end).

operator_associativity_test_() ->
    ?setup("module test { right^, right*, right, left+, left-, left<, left>, left=, left$ }
            import kind/domain/Domain

            type Op -> (+Left+: { next: Op, value: Domain/Any } |
                        -Left-: { next: Op, value: Domain/Any } |
                        <Left>: { next: Op, value: Domain/Any } |
                        >Left<: { next: Op, value: Domain/Any } |
                        =Left=: { next: Op, value: Domain/Any } |
                        $Left$: { next: Op, value: Domain/Any } |
                        ^Right^: { value: Domain/Any, next: Op } |
                        *Right*: { value: Domain/Any, next: Op } |
                        :Right: { value: Domain/Any, next: Op })
            import Op/_

            def left+ -> Nil +Left+ 2 +Left+ 1
            def left- -> Nil -Left- 2 -Left- 1
            def left< -> Nil <Left> 2 <Left> 1
            def left> -> Nil >Left< 2 >Left< 1
            def left= -> Nil =Left= 2 =Left= 1
            def left$ -> Nil $Left$ 2 $Left$ 1
            def right^ -> 1 ^Right^ 2 ^Right^ Nil
            def right* -> 1 *Right* 2 *Right* Nil
            def right -> 1 :Right 2 :Right Nil",
           fun({ok, _}) ->
                   [?test({tagged, 'Op/+Left+', 
                           #{value := 1,
                             next := {tagged, 'Op/+Left+',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'left+'()),
                    ?test({tagged, 'Op/-Left-', 
                           #{value := 1,
                             next := {tagged, 'Op/-Left-',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'left-'()),
                    ?test({tagged, 'Op/<Left>', 
                           #{value := 1,
                             next := {tagged, 'Op/<Left>',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'left<'()),
                    ?test({tagged, 'Op/>Left<', 
                           #{value := 1,
                             next := {tagged, 'Op/>Left<',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'left>'()),
                    ?test({tagged, 'Op/=Left=', 
                           #{value := 1,
                             next := {tagged, 'Op/=Left=',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'left='()),
                    ?test({tagged, 'Op/$Left$', 
                           #{value := 1,
                             next := {tagged, 'Op/$Left$',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'left$'()),
                    ?test({tagged, 'Op/^Right^', 
                           #{value := 1,
                             next := {tagged, 'Op/^Right^',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'right^'()),
                    ?test({tagged, 'Op/*Right*', 
                           #{value := 1,
                             next := {tagged, 'Op/*Right*',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:'right*'()),
                    ?test({tagged, 'Op/:Right', 
                           #{value := 1,
                             next := {tagged, 'Op/:Right',
                                      #{value := 2,
                                        next := 'Option/Nil'}}}}, test:right())]
           end).

div_operator_test_() ->
    ?setup("module test { // }
            import erlang/apply
            def // a b -> apply('erlang', '/', [a, b])",
           fun({ok, _}) -> [?test(2.0, test:'//'(4,2))] end).
