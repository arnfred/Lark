-module(union).
-export([union/2, union/1]).

union(Ds) when is_list(Ds) -> lists:foldl(fun(D1, D2) -> union(D1, D2) end, none, Ds).

union({recur, S}, {recur, T}) -> {recur, fun() -> union(S(), T()) end};
union({recur, S}, D) -> {recur, fun() -> union(S(), D) end};
union(D, {recur, S}) -> union({recur, S}, D);

union(D, D) -> D;
union(D1, D2) when is_map(D1), is_map(D2) ->
    F = fun(K, _) -> case {maps:is_key(K, D1), maps:is_key(K, D2)} of
                         {true, true} -> union(maps:get(K, D1), maps:get(K, D2));
                         {false, true} -> maps:get(K, D2);
                         {true, false} -> maps:get(K, D1)
                     end
        end,
    maps:map(F, maps:merge(D1, D2));
union(any, _) -> any;
union(_, any) -> any;
union(none, D) -> D;
union(D, none) -> D;
union({error, E1}, {error, E2}) -> {error, E1 ++ E2};
union({error, _}, D) -> D;
union(D, {error, _}) -> D;
union({sum, D1}, {sum, D2}) -> {sum, ordsets:union(D1, D2)};
union({sum, D1}, D) -> {sum, ordsets:add_element(D, D1)};
union(D, {sum, D1}) -> union({sum, D1}, D);
union({tagged, Tag, D1}, {tagged, Tag, D2}) -> {tagged, Tag, union(D1, D2)};
union({product, D1}, {product, D2}) -> 
    {sum, ordsets:from_list([{product, D1}, {product, D2}])};
union({f, Name1, F1}, {f, Name2, F2}) -> 
    Name = list_to_atom(lists:flatten([atom_to_list(Name1), "_", atom_to_list(Name2)])), 
    case {domain_util:get_arity(F1), domain_util:get_arity(F2)} of
        {N, N} -> {f, Name, domain_util:mapfun(fun(Res1, Res2) -> union(Res1, Res2) end, F1, F2)};
        _ -> {sum, ordsets:from_list([{f, Name1, F1}, {f, Name2, F2}])}
    end;
union(D1, D2) -> {sum, ordsets:from_list([D1, D2])}.

get_arity(Fun) ->
    proplists:get_value(arity, erlang:fun_info(Fun)).
