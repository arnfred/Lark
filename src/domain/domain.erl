-module(domain).
-export([diff/2, union/1, union/2, intersection/1, intersection/2, function/1,
         unroll/2, normalize/1, subset/2, lookup/2, expand/2, is_literal/1, to_term/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

union([D]) -> D;
union(Ds) when is_list(Ds) ->
    Unionized = lists:foldl(fun(D1,D2) -> union:union(D1, D2) end, none, Ds),
    normalize(unroll(Unionized)).
union(D1, D2) -> normalize(unroll(union:union(D1, D2))).

intersection([D]) -> D;
intersection(Ds) when is_list(Ds) ->
    Intersected = lists:foldl(fun(E1,E2) -> intersection:intersection(E1, E2) end, any, Ds),
    normalize(unroll(Intersected)).
intersection(D1, D2) -> normalize(unroll(intersection:intersection(D1, D2))).

subset(D1, D2) -> 
    diff(normalize(D1), intersection(D1, D2)) =:= none.

diff(Old, New) -> diff:diff([], Old, New).

normalize(D) -> normalize:normalize(D).

lookup({recur, F}, Elems) -> lookup(unroll(F()), Elems);
lookup(Map, Elems) when is_map(Map) ->
    normalize(maps:from_list([{K, intersection(maps:get(K, Map), D)} ||
                              {K, D} <- maps:to_list(Elems), maps:is_key(K, Map)]));
lookup({tagged, _, D}, Elems) -> lookup(D, Elems).

unroll(D) -> unroll_(100, D, {domain}).
unroll(D, ErrContext) -> unroll_(100, D, ErrContext).
unroll_(0, _, ErrContext) -> error:format({possibly_infinite_recursion}, ErrContext);
unroll_(N, {recur, D}, ErrContext) -> unroll_(N-1, D(), ErrContext);
unroll_(_, D, _) -> D.

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

% create a function domain matching any function
function(N) -> utils:function(N, fun(_) -> any end).

is_literal(any)                          -> false;
is_literal(none)                         -> false;
is_literal(whatever)                     -> false;
is_literal(D) when is_atom(D)            -> true;
is_literal(N) when is_number(N)          -> true;
is_literal(S) when is_binary(S)          -> true;
is_literal(F) when is_function(F)        -> false;
is_literal({sum, _})                     -> false;
is_literal(L) when is_list(L)            -> lists:all(fun(E) -> is_literal(E) end, L);
is_literal(M) when is_map(M)             -> lists:all(fun(E) -> is_literal(E) end, maps:values(M));
is_literal({sum, _, _})                  -> false;
is_literal({tagged, _, Val})             -> is_literal(Val).

to_term(Domain, Ctx) -> to_term_(Domain, maps:put(domain, Domain, Ctx)).
to_term_(Domain, Ctx) when is_list(Domain) -> {list, Ctx, [to_term(D, Ctx) || D <- Domain]};
to_term_(Domain, Ctx) when is_map(Domain) -> {dict, Ctx, [{pair, Ctx, {keyword, Ctx, K}, to_term(D, Ctx)}
                                                          || {K, D} <- maps:to_list(Domain)]};
to_term_({tagged, Path, Val}, Ctx) -> {tagged, Ctx, Path, to_term(Val, Ctx)};
to_term_({sum, Elems}, Ctx) -> {sum, Ctx, [to_term(E, Ctx) || E <- Elems]};
to_term_(Domain, Ctx) when is_atom(Domain) -> {value, Ctx, atom, Domain};
to_term_(Domain, Ctx) when is_integer(Domain) -> {value, Ctx, integer, Domain};
to_term_(Domain, Ctx) when is_float(Domain) -> {value, Ctx, float, Domain};
to_term_(Domain, Ctx) when is_binary(Domain) -> {value, Ctx, string, Domain}.

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
