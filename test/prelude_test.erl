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
    {"extract value from tagged function",
     ?setup("module test { Test, main }\n"
            "type Test -> Test: A\n"
            "def main t -> t.value",
            fun({ok, _}) ->
                    [?test('Test/A', test:main(test:'Test'()))]
            end)}.


%binary_search_test_() ->
%    {"Simple binary tree implementation",
%     ?setup("import Compare/_\n"
%            "type Tree a -> Leaf | Node: {left: Tree(a), value: a, right: Tree(a)}\n"
%            "def insert root elem\n"
%            "  | Leaf _                         -> Leaf\n"
%            "  | (Node: {left, value, right}) _ -> compare(elem, value).match(\n"
%            "        EQ -> root\n"
%            "        LT -> Node(left.insert(elem), value, right)\n"
%            "        GT -> Node(left, value, right.insert(elem)))\n",

       


