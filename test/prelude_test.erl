-module(prelude_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

-define(setup(Code, Tests), {setup,
                             fun() -> kind:load(Code, #{sandboxed => false,
                                                        add_kind_libraries => true}) end,
                             fun clean/1,
                             Tests}).

clean({error, _}) -> noop;
clean({ok, Modules}) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].

compare_test_() ->
    {"Test the compare function",
     ?setup("def main a b -> compare(a, b)",
            fun({ok, Mods}) ->
                    Mod = lists:last(Mods),
                    [?test('Compare/LT', Mod:main(1, 2)),
                     ?test('Compare/GT', Mod:main(b, a)),
                     ?test('Compare/EQ', Mod:main([1, 2], [1, 2]))]
            end)}.

tagged_test_() ->
    {"get inner value from tagged function",
     ?setup("module test { Test, main }\n"
            "type Test -> Test: A\n"
            "def main t -> t.get",
            fun({ok, _}) ->
                    [?test('Test/A', test:main(test:'Test'()))]
            end)}.


binary_search_test_() ->
    {"Simple binary tree implementation",
     ?setup("module test { main }
             import kind/prelude/Compare/_
             import Tree/_
             type Tree a -> Leaf | Node: {left: Tree(a), value: a, right: Tree(a)}
             def insert root elem
               | Leaf _                         -> Node(Leaf, elem, Leaf)
               | (Node: {left, value, right}) _ -> compare(elem, value).match(
                     EQ -> root
                     LT -> Node(left.insert(elem), value, right)
                     GT -> Node(left, value, right.insert(elem)))
             def main n -> Leaf.insert(1).insert(3).insert(5).insert(4).insert(n).insert(2)",
            fun({ok, _}) ->
                    [?testEqual({tagged,'Tree/Node',
                                 #{left => 'Tree/Leaf',
                                   right =>
                                   {tagged,'Tree/Node',
                                    #{left =>
                                      {tagged,'Tree/Node',
                                       #{left => 'Tree/Leaf',right => 'Tree/Leaf',
                                         value => 2}},
                                      right =>
                                      {tagged,'Tree/Node',
                                       #{left =>
                                         {tagged,'Tree/Node',
                                          #{left => 'Tree/Leaf',right => 'Tree/Leaf',
                                            value => 4}},
                                         right =>
                                         {tagged,'Tree/Node',
                                          #{left => 'Tree/Leaf',right => 'Tree/Leaf',
                                            value => 7}},
                                         value => 5}},
                                      value => 3}},
                                   value => 1}}, test:main(7))]
            end)}.

       


