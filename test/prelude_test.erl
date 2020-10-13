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

       


