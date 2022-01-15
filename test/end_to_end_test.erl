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

