-module(utils).
-export([duplicates/2, group_by/2, group_by/3, unique/1]).

duplicates(Elements, GetKey) ->
    F = fun(Elem, {Duplicates, Seen}) -> 
                Key = GetKey(Elem),
                case maps:is_key(Key, Seen) of
                    true    -> {[{Elem, maps:get(Key, Seen)} | Duplicates], Seen};
                    false   -> {Duplicates, maps:put(Key, Elem, Seen)}
                end end,
    {Duplicates, _} = lists:foldl(F, {[], #{}}, Elements),
    Duplicates.

group_by(KeyF, L) -> group_by(KeyF, fun(X) -> X end, L).
group_by(KeyF, ValF, L) -> 
	Dict = lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {KeyF(X), ValF(X)} || X <- L ]),
	dict:to_list(Dict).

unique(L) -> 
    {Out, _} = lists:foldl(fun(Elem, {Out, Seen}) -> 
                                      case ordsets:is_element(Elem, Seen) of
                                          true -> {Out, Seen};
                                          false -> {[Elem | Out], ordsets:add_element(Elem, Seen)}
                                      end end, {[], ordsets:new()}, L),
    lists:reverse(Out).
