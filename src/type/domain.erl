-module(domain).
-export([diff/2, union/1, union/2, intersection/1, intersection/2]).

-include_lib("eunit/include/eunit.hrl").

diff(Same, Same) -> none;
diff({Type, Old}, {Type, New}) -> 
    case diff(Old, New) of
        none -> none;
        Diff -> {Type, Diff}
    end;

diff({tagged, Tag, Old}, {tagged, Tag, New}) ->
    case diff(Old, New) of
        none -> none;
        Diff -> {tagged, Tag, Diff}
    end;
                                      
diff(Old, New) when is_map(Old), is_map(New) -> 
    Keys = sets:to_list(sets:from_list(maps:keys(Old) ++ maps:keys(New))),
    OnlyInOld = [Key || Key <- Keys, not maps:is_key(Key, New)],
    OnlyInNew = [Key || Key <- Keys, not maps:is_key(Key, Old)],
    Domains = [{Key, diff(maps:get(Key, Old), maps:get(Key, New))} || 
               Key <- Keys, maps:is_key(Key, Old) andalso maps:is_key(Key, New)],
    DiffDomains = [{K, D} || {K, D} <- Domains, not(D =:= none)],
    case {OnlyInOld, OnlyInNew, DiffDomains} of
        {[], [], []} -> none;
        _ -> #{only_in_old => OnlyInOld, 
               only_in_new => OnlyInNew, 
               diff => DiffDomains}
    end;

diff(Old, New) -> 
    case sets:is_set(Old) andalso sets:is_set(New) of
        true -> case set_eq(New,Old) of
                    true -> none;
                    false -> #{only_in_old => sets:to_list(sets:subtract(Old, New)),
                               only_in_new => sets:to_list(sets:subtract(New, Old))}
                end;
        false -> #{old => Old,
                   new => New}
    end.

% Union serves two purposes:
% 1. Compute the union of two environments
% 2. Compute the union of two domains
% Since both a product domain and an environment is represented by a map, both
% can be computed by the same function.
 
% There are several use-cases for computing the union of two domains. One of
% them is to find the domain of a function argument in a function with several
% pattern matching clauses. In this use case we want to know the smallest sets
% of constraints an argument can satisfy in order to pass at least one of the
% pattern matching clauses.  For example, if one clause is `{a, b}` (expecting
% a product with the keys `a` and `b`, and another clause is `{a}` (expecting a
% product with only the key `a`), then any argument with the key `a` would be
% accepted by the pattern match.

% List of Envs or Domains
union(Envs) when is_list(Envs) -> lists:foldl(fun(E1,E2) -> union(E1, E2) end, none, Envs).

% Union of two Envs or Domains
union(D1, D2) when is_map(D1), is_map(D2) ->
    F = fun(K, _) -> case {maps:is_key(K, D1), maps:is_key(K, D2)} of
                         {true, true} -> union(maps:get(K, D1), maps:get(K, D2));
                         {false, true} -> maps:get(K, D2);
                         {true, false} -> maps:get(K, D1)
                     end
        end,
    maps:map(F, maps:merge(D1, D2));

union(D, D) -> D;
union(any, _) -> any;
union(_, any) -> any;
union(none, D) -> D;
union(D, none) -> D;
union({f, F}, D) -> fun(Args) -> union(D, F(Args)) end;
union(D, {f, F}) -> union({f, F}, D);
union({sum, D1}, {sum, D2}) -> compact({sum, sets:union(D1, D2)});
union({sum, D1}, D) -> compact({sum, sets:add_element(D, D1)});
union(D, {sum, D1}) -> union({sum, D1}, D);
union({tagged, Tag, D1}, {tagged, Tag, D2}) -> {tagged, Tag, union(D1, D2)};
union({product, D1}, {product, D2}) -> 
    compact({sum, sets:from_list([{product, D1}, {product, D2}])});
union(D1, D2) -> compact({sum, sets:from_list([D1, D2])}).


intersection(Ds) when is_list(Ds) -> lists:foldl(fun(E1,E2) -> intersection(E1, E2) end, none, Ds).
intersection(D1, D2) when is_map(D1), is_map(D2) ->
    % When intersecting two maps we include all domains of the two maps.  This
    % is because a key not present in a map is assumed to have domain `any`,
    % and any narrower definition would need to be captured in the intersection
    F = fun(K, _) -> case {maps:is_key(K, D1), maps:is_key(K, D2)} of
                         {true, true} -> intersection(maps:get(K, D1), maps:get(K, D2));
                         {false, true} -> maps:get(K, D2);
                         {true, false} -> maps:get(K, D1)
                     end
        end,
    maps:map(F, maps:merge(D1, D2));

intersection(D, D) -> D;
intersection(any, D) -> D;
intersection(D, any) -> D;
intersection(none, _) -> none;
intersection(_, none) -> none;
intersection({f, F}, D) -> fun(Args) -> intersection(D, F(Args)) end;
intersection(D, {f, F}) -> intersection({f, F}, D);
intersection({sum, D1}, {sum, D2}) -> 
    io:format("D1 is: ~p~n", [D1]),
    io:format("D2 is: ~p~n", [D2]),
    compact({sum, sets:from_list([intersection(Dj, Di) || Di <- sets:to_list(D1), Dj <- sets:to_list(D2)])});
intersection({sum, D1}, D) -> 
    compact({sum, sets:from_list([intersection(D, Di) || Di <- sets:to_list(D1)])});
intersection(D, {sum, D1}) -> intersection({sum, D1}, D);
intersection({tagged, Tag, D1}, {tagged, Tag, D2}) -> 
    propagate_none({tagged, Tag, intersection(D1, D2)});
intersection({product, D1}, {product, D2}) -> propagate_none({product, intersection(D1, D2)});
intersection(_, _) -> none.


propagate_none({product, Map}) -> case lists:member(none, maps:values(Map)) of
                                      true -> none;
                                      false -> {product, Map}
                                  end;
propagate_none({tagged, T, {product, Map}}) -> case lists:member(none, maps:values(Map)) of
                                                   true -> none;
                                                   false -> {tagged, T, {product, Map}}
                                               end;
propagate_none(D) -> D.



compact({sum, S}) -> compact(S);

compact(S) ->
    io:format("S is: ~p~n", [S]),
    % Filter out `none` and group by domain type
    KeyFun = fun({Type, _}) -> Type;
                (Other)     -> Other end,
    Keyed = [{K, V} || {K, V} <- group_by(KeyFun, sets:to_list(S))],
    io:format("Keyed is: ~p~n", [Keyed]),


    % Based on domain type compact appropriately
    Compacted = lists:map(fun({product, Products})  -> compact_products(Products);
                             ({sum, Sums})          -> compact(sets:union([Set || {sum, Set} <- Sums]));
                             ({none, _})            -> [];
                             ({any, _})             -> [any];
                             ({_, Other})           -> Other
                          end, Keyed),

    % We want to remove any instances of `none` in the sum. If there are
    % instances of `any`, the whole sum is converted to an `any`

    io:format("Compacted is: ~p~n", [Compacted]),
    case lists:flatten(Compacted) of
        []      -> none;
        [D]     -> D;
        Elems   -> 
            NoNones = [E || E <- Elems, not(E =:= none)],
            case lists:member(any, NoNones) of 
                       true -> any; 
                       _ -> {sum, sets:from_list(Elems)} 
            end
    end.


compact_products(Products) -> 
    [{product, M} || M <- compact_maps([M || {product, M} <- Products])].

% Why is the union of two products tricky? 
%
% Because the union of a cartesian product is not equal to the union of each
% domain of the cartesian product.  to illustrate, consider the two product
% `{a: D_a, b: D_b/2}` and `{a: D_a/2, b: D_b}`. 
%
% In a clunky notation i'm denoting `D_b` as the full domain of `b`
% and `D_b/2` as some arbitrary partition of `D_b`. similarly for `D_a` and
% `D_a/2`. 
%
% The union of these two products is _NOT_ `{a: D_a, b: D_b}`. This set
% includes an element `(a_i, b_i)` where `a_i` is not a member of `D_a/2` and
% `b_i` is not a member of `D_b/2`. While this set is a member of `{a: D_a, b:
% D_b}`, it's not a member of any of the two original products.      
%
% Instead the union algorithm I've implemented will find any members of a list
% of products where the domains for all keys but one are the same and create
% the union of the last key.
% 
% The algorihm does this by picking out a pivot: the key with the smallest
% range of domains. All products are then grouped based on the domain of the
% pivot. If the pivot isn't part of a product, it's grouped by the domain
% `undefined. For each group we drop the pivot key from the product. If there
% is just one unique key left, we take the union of the rest of the domains.
% Otherwise we recurse with the group of products without the pivot key.
compact_maps([]) -> [];
compact_maps([M]) -> [M];
compact_maps(Maps) ->
    % Select the key appearing in most products as the first pivot key
    CountDomains = fun(Key) -> sets:size(sets:from_list([maps:get(Key, M, undefined) || M <- Maps])) end,
    Compare = fun(K1, K2) -> CountDomains(K1) < CountDomains(K2) end,
    Keys = lists:sort(Compare, lists:flatten([maps:keys(M) || M <- Maps])),
    Pivot = lists:nth(1, Keys),

    % We can do a union only if the product maps vary along a single dimension
    % (i.e. key). When there's only one key left, we know this to be the case.
    NumberOfUniqueKeys = sets:size(sets:from_list(Keys)),
    case NumberOfUniqueKeys of
        0 -> [];
        1 -> [#{Pivot => union([maps:get(Pivot, M, any) || M <- Maps])}];
        _ -> 
            % Group maps by domain of pivot key
            Grouped = group_by(fun(Map) -> maps:get(Pivot, Map, undefined) end, Maps),

            Compact = fun(Key, Domain, PivotedMaps) -> 
                              Compacted = compact_maps([maps:remove(Key, Map) || Map <- PivotedMaps]),

                              % We use `undefined` when a key doesn't exist in
                              % one of the maps. If we group by `undefined`, we
                              % remove the undefined key before reassembling
                              % the map
                              case Domain of
                                  undefined -> Compacted;
                                  _         -> [maps:put(Key, Domain, M) || M <- Compacted]
                              end
                      end,

            % For each group of domains, we remove the pivot key and call this
            % function recursively
            lists:flatten([Compact(Pivot, Domain, Group) || {Domain, Group} <- Grouped])
    end.

set_eq(S1, S2) -> sets:is_subset(S1, S2) andalso sets:is_subset(S2, S1).

group_by(F, L) -> dict:to_list(lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ])).


-ifdef(TEST).

union_product_one_key_test() ->
    D1 = {product, #{blip => true}},
    D2 = {product, #{blip => false}},
    Expected = {product, #{blip => {sum, sets:from_list([true, false])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_two_keys_mergeable_test() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = {product, #{blap => a, blip => {sum, sets:from_list([true, false])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_two_keys_non_mergeable_test() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blap => b, blip => false}},
    Expected = {sum, sets:from_list([{product, #{blap => a, blip => true}},
                                     {product, #{blap => b, blip => false}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_uneven_keys_mergeable_test() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blip => true}},
    Expected = {product, #{blap => any, blip => true}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_many_keys_test() ->
    D1 = {product, #{blip => true, blap => false, blup => extra_old}},
    D2 = {product, #{blip => true, blap => true, blep => extra_new}},
    Expected = {sum, sets:from_list([{product, #{blip => true, blap => false, blup => extra_old}},
                                     {product, #{blip => true, blap => true, blep => extra_new}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_same_keys_test() ->
    D1 = {product, #{blip => true, blap => false}},
    D2 = {product, #{blip => true, blap => true}},
    Expected = {product, #{blip => true, blap => {sum, sets:from_list([false, true])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_test() ->
    D1 = {sum, sets:from_list([a,b,c])},
    D2 = {sum, sets:from_list([b,c,d])},
    Expected = {sum, sets:from_list([a,b,c,d])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_none_test() ->
    D1 = {sum, sets:from_list([none])},
    D2 = {sum, sets:from_list([a,b])},
    Expected = {sum, sets:from_list([a,b])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_tagged_same_test() ->
    D1 = {tagged, kukkeluk, {product, #{blip => true}}},
    D2 = {tagged, kukkeluk, {product, #{blip => false}}},
    Expected = {tagged, kukkeluk, {product, #{blip => {sum, sets:from_list([true, false])}}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_tagged_diff_test() ->
    D1 = {tagged, kukkeluk, {product, #{blip => true, blap => false, blup => extra_old}}},
    D2 = {tagged, kakkelak, {product, #{blip => true, blap => true, blep => extra_new}}},
    Expected = {sum, sets:from_list([{tagged, kukkeluk, {product, #{blip => true, blap => false, blup => extra_old}}},
                                     {tagged, kakkelak, {product, #{blip => true, blap => true, blep => extra_new}}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_of_products_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2}}])},
    D2 = {sum, sets:from_list([{product, #{a => 1, c => 3}}])},
    Expected = {sum, sets:from_list([{product, #{a => 1, b => 2}}, {product, #{a => 1, c => 3}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_of_many_products_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2, c => 1}},
                               {product, #{a => 2, b => 2, c => 2}},
                               {product, #{a => 1, b => 2, c => 3}},
                               {product, #{a => 2, b => 2, c => 4}}])},
    D2 = {sum, sets:from_list([{product, #{a => 1}}])},
    Expected = {sum, sets:from_list([{product, #{a => 1, b => 2, c => {sum, sets:from_list([1, 3])}}}, 
                                     {product, #{a => 2, b => 2, c => {sum, sets:from_list([2, 4])}}},
                                     {product, #{a => 1}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_with_none_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2, c => 1}}, none])},
    D2 = {sum, sets:from_list([none])},
    Expected = {product, #{a => 1, b => 2, c => 1}}, 
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_with_any_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2, c => 1}}, any])},
    D2 = {sum, sets:from_list([none])},
    Expected = any,
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_of_products_with_same_keys_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2}}])},
    D2 = {sum, sets:from_list([{product, #{a => 1, b => 3}}])},
    Expected = {product, #{a => 1, b => {sum, sets:from_list([2,3])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_f_test() ->
    D1 = {f, fun([A]) -> A end},
    D2 = b,
    Expected = {sum, sets:from_list([a, b])},
    Actual = apply(union(D1, D2), [[a]]),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_product_one_key_test() ->
    D1 = {product, #{blip => true}},
    D2 = {product, #{blip => false}},
    Expected = none,
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_product_two_keys_mergeable_test() ->
    D1 = {product, #{blap => a, blip => {sum, sets:from_list([false,true])}}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = {product, #{blap => a, blip => false}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_product_two_keys_non_mergeable_test() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = none,
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_product_subset_test() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blip => true}},
    Expected = {product, #{blap => a, blip => true}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_product_non_subset_test() ->
    D1 = {product, #{blap => a, blup => true}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = {product, #{blap => a, blip => false, blup => true}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_test() ->
    D1 = {sum, sets:from_list([a,b,c])},
    D2 = {sum, sets:from_list([b,c,d])},
    Expected = {sum, sets:from_list([b,c])},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_any_test() ->
    D1 = {sum, sets:from_list([any])},
    D2 = {sum, sets:from_list([a,b])},
    Expected = {sum, sets:from_list([a,b])},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_tagged_same_test() ->
    D1 = {tagged, kukkeluk, {product, #{blip => {sum, sets:from_list([false, true])}}}},
    D2 = {tagged, kukkeluk, {product, #{blip => false}}},
    Expected = {tagged, kukkeluk, {product, #{blip => false}}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_tagged_diff_test() ->
    D1 = {tagged, kukkeluk, {product, #{blip => true, blap => false, blup => extra_old}}},
    D2 = {tagged, kakkelak, {product, #{blip => true, blap => true, blep => extra_new}}},
    Expected = none,
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_of_products_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2}}])},
    D2 = {sum, sets:from_list([{product, #{a => 1, b => {sum, sets:from_list([2,3])}}}])},
    Expected = {product, #{a => 1, b => 2}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_with_none_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2, c => 1}}, none])},
    D2 = {sum, sets:from_list([none])},
    Expected = none,
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_with_any_test() ->
    D1 = {sum, sets:from_list([{product, #{a => 1, b => 2, c => 1}}])},
    D2 = {sum, sets:from_list([any])},
    Expected = {product, #{a => 1, b => 2, c => 1}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_f_test() ->
    D1 = {f, fun([A]) -> A end},
    D2 = {sum, sets:from_list([a, b])},
    Expected = b,
    Actual = apply(intersection(D1, D2), [[{sum, sets:from_list([b, c])}]]),
    ?assertEqual(none, diff(Expected, Actual)).

diff_sum_product_test() ->
    Old = {product, #{blah => {values, sets:from_list([true, false])} } },
    New = {sum, sets:from_list([true, false])},
    Expected = #{old => Old,
                 new => New},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_sum_sum_test() ->
    Old = {sum, sets:from_list([blip, blop, honk])},
    New = {sum, sets:from_list([true, false, honk])},
    Expected = {sum, #{only_in_old => [blop, blip],
                       only_in_new => [false, true]}},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_product_product_test() ->
    Old = {product, #{blip => true, blap => false, blup => extra_old}},
    New = {product, #{blip => true, blap => true, blep => extra_new}},
    Expected = {product, #{only_in_old => [blup], 
                           only_in_new => [blep], 
                           diff => [{blap, #{old => false,
                                             new => true}}]}},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_equal_sum_sum_test() ->
    Old = {sum, sets:from_list([blip, blop, honk])},
    New = {sum, sets:from_list([blip, blop, honk])},
    Expected = none,
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_equal_product_product_test() ->
    Old = {product, #{blip => true, blap => false, blup => extra_old}},
    New = {product, #{blip => true, blap => false, blup => extra_old}},
    Expected = none,
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_nested1_test() ->
    Old = {product, #{blah => {sum, sets:from_list([true, false])} } },
    New = {product, #{blah => {sum, sets:from_list([true, false, blap])} } },
    Expected = {product, #{only_in_old => [],
                           only_in_new => [],
                           diff => [{blah, {sum, #{only_in_old => [],
                                                   only_in_new => [blap]}}}]}},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_nested2_test() ->
    OldProduct = {product, #{blip => true}},
    NewProduct = {product, #{blap => false}},
    Old = {sum, sets:from_list([OldProduct])},
    New = {sum, sets:from_list([NewProduct])},
    Expected = {sum, #{only_in_old => [OldProduct],
                       only_in_new => [NewProduct]}},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_equal_nested1_test() ->
    Old = {sum, sets:from_list([{product, #{blip => {sum, sets:from_list([c,b,a])}}}])},
    New = {sum, sets:from_list([{product, #{blip => {sum, sets:from_list([a,b,c])}}}])},
    Expected = none,
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_tagged_test() ->
    Old = {tagged, kukkeluk, true},
    New = {tagged, kukkeluk, false},
    Expected = {tagged, kukkeluk, #{old => true, new => false}},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_tagged_sum_test() ->
    Old = {tagged, kukkeluk, {sum, sets:from_list([a,b])}},
    New = {tagged, kukkeluk, {sum, sets:from_list([a,b])}},
    Expected = none,
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

    
-endif.
