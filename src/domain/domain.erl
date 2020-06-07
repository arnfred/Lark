-module(domain).
-export([diff/2, union/1, union/2, intersection/1, intersection/2, compact/1, subset/2, lookup/2, expand/2]).
-include_lib("eunit/include/eunit.hrl").

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
lookup({product, Map}, Elems) -> 
    compact({product, maps:from_list([{K, intersection(maps:get(K, Map), D)} || 
                              {K, D} <- maps:to_list(Elems), maps:is_key(K, Map)])});
lookup({tagged, _, D}, Elems) -> lookup(D, Elems).

unroll(D) -> unroll_(100, D).
unroll_(0, _) -> {error, [{{possibly_infinite_recursion}, {domain}}]};
unroll_(N, {recur, D}) -> unroll_(N-1, D());
unroll_(_, D) -> D.

expand(N, D) -> expand_(N, unroll(D)).
expand_(0, D) -> D;
expand_(N, {sum, M}) -> maps:map(fun(K, true) -> expand_(N-1, K) end, M);
expand_(N, {Type, D}) -> {Type, expand_(N-1, D)};
expand_(N, {tagged, Tag, D}) -> {tagged, Tag, expand_(N-1, D)};
expand_(N, Ds) when is_list(Ds) -> lists:map(fun(D) -> expand_(N-1, D) end, Ds);
expand_(N, M) when is_map(M) -> maps:map(fun(_, D) -> expand_(N-1, D) end, M);
expand_(_, S) -> S.

-ifdef(TEST).

subset_sum_sum_test_() ->
    D1 = {sum, maps:from_list([{E, true} || E <- [a, b, c]])},
    D2 = {sum, maps:from_list([{E, true} || E <- [a, b, c, d]])},
    ?_assertEqual(true, subset(D1, D2)).

subset_non_sum_sum_test_() ->
    D1 = {sum, maps:from_list([{E, true} || E <- [a, b, c]])},
    D2 = {sum, maps:from_list([{E, true} || E <- [b, c, d]])},
    ?_assertEqual(false, subset(D1, D2)).
    
subset_product_product_test_() ->
    D1 = {product, #{a => 1, b => 2}},
    D2 = {product, #{a => 1}},
    ?_assertEqual(true, subset(D1, D2)).

subset_non_product_product_test_() ->
    D1 = {product, #{a => 1, b => 2}},
    D2 = {product, #{a => 2}},
    ?_assertEqual(false, subset(D1, D2)).

subset_tagged_tagged_test_() ->
    D1 = {tagged, t, t},
    D2 = {tagged, t, {sum, maps:from_list([{E, true} || E <- [s, t]])}},
    ?_assertEqual(true, subset(D1, D2)).

subset_non_tagged_tagged_test_() ->
    D1 = {tagged, t, t},
    D2 = {tagged, s, {sum, maps:from_list([{E, true} || E <- [s, t]])}},
    ?_assertEqual(false, subset(D1, D2)).

subset_sum_product_test_() ->
    D1 = {product, #{a => 2, b => 3, c => 4}},
    D2 = {sum, maps:from_list([{E, true} || E <- [{product, #{a => 1, b => 2}},
                                                  {product, #{a => 2, b => 3}}]])},
    ?_assertEqual(true, subset(D1, D2)).

lookup_product_test_() ->
    D = {product, #{a => 'A', b => 'B'}},
    Elems = #{a => any},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

lookup_recur_test_() ->
    D = {recur, fun() -> {product, #{a => 'A', b => 'B'}} end},
    Elems = #{a => any},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

lookup_domain_intersection_test_() ->
    D = {product, #{a => {sum, maps:from_list([{E, true} || E <- ['A', 'B']])}}},
    Elems = #{a => 'A'},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

lookup_tagged_test_() -> 
    D = {tagged, tag, {product, #{a => 'A', b => 'B'}}},
    Elems = #{a => any},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?_assertEqual(none, diff(Expected, Actual)).

-endif.
