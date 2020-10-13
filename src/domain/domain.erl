-module(domain).
-export([domain/1, diff/2, union/1, union/2, intersection/1, intersection/2, compact/1, subset/2, lookup/2, expand/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

domain(D) when is_record(D, value, 2) -> D;
domain(D) when is_record(D, list, 2) -> D;
domain(D) when is_record(D, sum, 2) -> D;
domain(D) when is_record(D, product, 2) -> D;
domain(D) when is_record(D, recur, 2) -> D;
domain(D) when is_record(D, tagged, 3) -> D;
domain(D) when is_record(D, f, 3) -> D;
domain(Atom) when is_atom(Atom) -> {value, Atom};
domain(Number) when is_number(Number) -> {value, Number};
domain(Binary) when is_binary(Binary) -> {value, Binary};
domain(Tuple) when is_tuple(Tuple) -> {list, [domain(E) || E <- tuple_to_list(Tuple)]};
domain(List) when is_list(List) -> {list, [domain(E) || E <- List]};
domain(Map) when is_map(Map) -> {product, maps:from_list([{Key, domain(Val)} || {Key, Val} <- maps:to_list(Map)])};
domain(F) when is_function(F) -> NewF = domain_util:mapfun(fun domain/1, F),
                                 {f, element(2, erlang:fun_info(F, name)), NewF}.

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
    D1 = {product, #{a => 1, b => 2}},
    D2 = {product, #{a => 1}},
    ?_assertEqual(true, subset(D1, D2)).

subset_non_product_product_test_() ->
    D1 = {product, #{a => 1, b => 2}},
    D2 = {product, #{a => 2}},
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
    D1 = {product, #{a => 2, b => 3, c => 4}},
    D2 = {sum, ordsets:from_list([{product, #{a => 1, b => 2}},
                               {product, #{a => 2, b => 3}}])},
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
    D = {product, #{a => {sum, ordsets:from_list(['A', 'B'])}}},
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

domain_test_() ->
    [?testEqual({value, atom}, domain(atom)),
     ?testEqual({value, 1.2}, domain(1.2)),
     ?testEqual({value, <<"❤️"/utf8>>}, domain(<<"❤️"/utf8>>)),
     ?testEqual({list, [{value, a}, {value, b}]}, domain({a, b})),
     ?testEqual({list, [{value, 1}, {value, 2}]}, domain([1, 2])),
     ?testEqual({product, #{a => {value, 1}}}, domain(#{a => 1})),
     ?test({f, _, _F}, domain(fun(_) -> 1 end)),
     ?test({value, 1}, erlang:apply(element(3, domain(fun(_) -> 1 end)), ['_']))
    ].


-endif.
