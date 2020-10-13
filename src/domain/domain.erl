-module(domain).
-export([diff/2, union/1, union/2, intersection/1, intersection/2, compact/1, subset/2, lookup/2, expand/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

union(Ds) when is_list(Ds) -> 
    Unionized = lists:foldl(fun(D1,D2) -> union:union(D1, D2) end, none, Ds),
    compact(unroll(Unionized)).
union(D1, D2) -> compact(unroll(union:union(D1, D2))).

intersection(Ds) when is_list(Ds) -> 
    Intersected = lists:foldl(fun(E1,E2) -> intersection:intersection(E1, E2) end, any, Ds),
    compact(unroll(Intersected)).
intersection(D1, D2) -> compact(unroll(intersection:intersection(D1, D2))).

subset(D1, D2) -> diff(D1, intersection(D1, D2)) =:= none.

diff(Old, New) -> diff:diff([], Old, New).

compact(D) -> unroll(compact:compact(D)).

lookup({recur, F}, Elems) -> lookup(unroll(F()), Elems);
lookup(Map, Elems) when is_map(Map) -> 
    compact(maps:from_list([{K, intersection(maps:get(K, Map), D)} || 
                            {K, D} <- maps:to_list(Elems), maps:is_key(K, Map)]));
lookup({tagged, _, D}, Elems) -> lookup(D, Elems).

unroll(D) -> unroll_(100, D).
unroll_(0, _) -> {error, [{{possibly_infinite_recursion}, {domain}}]};
unroll_(N, {recur, D}) -> unroll_(N-1, D());
unroll_(_, D) -> D.

expand(N, D) -> expand_(N, unroll(D)).
expand_(0, D) -> D;
expand_(N, {Type, D}) -> {Type, expand_(N-1, D)};
expand_(N, {tagged, Tag, D}) -> {tagged, Tag, expand_(N-1, D)};
expand_(N, Ds) when is_list(Ds) -> lists:map(fun(D) -> expand_(N-1, D) end, Ds);
expand_(N, M) when is_map(M) -> maps:map(fun(_K, D) -> expand_(N-1, D) end, M);
expand_(N, S) -> case ordsets:is_set(S) of
                     true -> ordsets:map(fun(D) -> expand_(N-1, D) end, S);
                     false -> S
                 end.

-ifdef(TEST).

subset_sum_sum_test_() ->
    D1 = {sum, ordsets:from_list([a, b, c])},
    D2 = {sum, ordsets:from_list([a, b, c, d])},
    ?_assertEqual(true, subset(D1, D2)).

subset_non_sum_sum_test_() ->
    D1 = {sum, ordsets:from_list([a, b, c])},
    D2 = {sum, ordsets:from_list([b, c, d])},
    ?_assertEqual(false, subset(D1, D2)).
    
subset_product_product_test_() ->
    D1 = #{a => 1, b => 2},
    D2 = #{a => 1},
    ?_assertEqual(true, subset(D1, D2)).

subset_non_product_product_test_() ->
    D1 = #{a => 1, b => 2},
    D2 = #{a => 2},
    ?_assertEqual(false, subset(D1, D2)).

subset_tagged_tagged_test_() ->
    D1 = {tagged, t, t},
    D2 = {tagged, t, {sum, ordsets:from_list([s, t])}},
    ?_assertEqual(true, subset(D1, D2)).

subset_non_tagged_tagged_test_() ->
    D1 = {tagged, t, t},
    D2 = {tagged, s, {sum, ordsets:from_list([s, t])}},
    ?_assertEqual(false, subset(D1, D2)).

subset_sum_product_test_() ->
    D1 = #{a => 2, b => 3, c => 4},
    D2 = {sum, ordsets:from_list([#{a => 1, b => 2},
                               #{a => 2, b => 3}])},
    ?_assertEqual(true, subset(D1, D2)).

lookup_product_test_() ->
    D = #{a => 'A', b => 'B'},
    Elems = #{a => any},
    Expected = #{a => 'A'},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

lookup_recur_test_() ->
    D = {recur, fun() -> #{a => 'A', b => 'B'} end},
    Elems = #{a => any},
    Expected = #{a => 'A'},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

lookup_domain_intersection_test_() ->
    D = #{a => {sum, ordsets:from_list(['A', 'B'])}},
    Elems = #{a => 'A'},
    Expected = #{a => 'A'},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

lookup_tagged_test_() -> 
    D = {tagged, tag, #{a => 'A', b => 'B'}},
    Elems = #{a => any},
    Expected = #{a => 'A'},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

-endif.
