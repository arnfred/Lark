-module(domain).
-export([diff/2, union/1, union/2, intersection/1, intersection/2, compact/1, subset/2, lookup/2]).
-include_lib("eunit/include/eunit.hrl").

diff(Old, New) -> diff_([], Old, New).

diff_(_, {recur, D}, {recur, D}) -> none;
diff_(Path, {recur, OldF}, {recur, NewF}) -> 
    OldTag = gen_tag(OldF),
    NewTag = gen_tag(NewF),
    case lists:member(OldTag, Path) or lists:member(NewTag, Path) of
        true -> none;
        false -> diff_([OldTag | [NewTag | Path]], OldF(), NewF())
    end;
diff_(Path, {recur, OldF}, New) -> 
    Tag = gen_tag(OldF),
    case lists:member(Tag, Path) of
        true -> none;
        false -> diff_([Tag | Path], OldF(), New)
    end;
diff_(Path, Old, {recur, NewF}) -> 
    Tag = gen_tag(NewF),
    case lists:member(Tag, Path) of
        true -> none;
        false -> diff_([Tag | Path], Old, NewF())
    end;
diff_(_, Same, Same) -> none;
diff_(Path, {Type, Old}, {Type, New}) -> 
    case diff_(Path, Old, New) of
        none -> none;
        Diff -> {Type, Diff}
    end;

diff_(Path, {tagged, Tag, Old}, {tagged, Tag, New}) ->
    case diff_(Path, Old, New) of
        none -> none;
        Diff -> {tagged, Tag, Diff}
    end;

diff_(Path, Old, New) when is_map(Old), is_map(New) -> 
    Keys = ordsets:to_list(ordsets:from_list(maps:keys(Old) ++ maps:keys(New))),
    OnlyInOld = [Key || Key <- Keys, not maps:is_key(Key, New)],
    OnlyInNew = [Key || Key <- Keys, not maps:is_key(Key, Old)],
    Domains = [{Key, diff_(Path, maps:get(Key, Old), maps:get(Key, New))} || 
               Key <- Keys, maps:is_key(Key, Old) andalso maps:is_key(Key, New)],
    DiffDomains = [{K, D} || {K, D} <- Domains, not(D =:= none)],
    case {OnlyInOld, OnlyInNew, DiffDomains} of
        {[], [], []} -> none;
        _ -> #{only_in_old => OnlyInOld, 
               only_in_new => OnlyInNew, 
               diff => DiffDomains}
    end;

diff_(Path, Old, New) -> 
    case ordsets:is_set(Old) andalso ordsets:is_set(New) of
        true -> diff_set(Path, Old, New);
        false -> #{old => Old,
                   new => New}
    end.

diff_set(Path, Old, New) ->
    InOld = ordsets:subtract(Old, New),
    InNew = ordsets:subtract(New, Old),
    Matched = [{O, N} || O <- InOld, N <- InNew, D <- [diff_(Path, O, N)], D =:= none],
    {MatchedOld, MatchedNew} = lists:unzip(Matched),
    OnlyInOld = ordsets:to_list(ordsets:subtract(InOld, ordsets:from_list(MatchedOld))),
    OnlyInNew = ordsets:to_list(ordsets:subtract(InNew, ordsets:from_list(MatchedNew))),
    case length(OnlyInOld) =:= 0 andalso length(OnlyInNew) =:= 0 of
        true -> none;
        false -> #{only_in_old => OnlyInOld,
                   only_in_new => OnlyInNew}
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
union(D1, D2) -> compact(unroll(union_(D1, D2))).

union_({recur, S}, {recur, T}) -> {recur, fun() -> union_(S(), T()) end};
union_({recur, S}, {sum, _} = D) -> {recur, fun() -> union_(S(), D) end};
union_({recur, S}, {product, _} = D) -> {recur, fun() -> union_(S(), D) end};
union_({recur, S}, {tagged, _, _} = D) -> {recur, fun() -> union_(S(), D) end};
union_({recur, _}, _) -> none;
union_(D, {recur, S}) -> union_({recur, S}, D);

union_(D, D) -> D;
union_(D1, D2) when is_map(D1), is_map(D2) ->
    F = fun(K, _) -> case {maps:is_key(K, D1), maps:is_key(K, D2)} of
                         {true, true} -> union_(maps:get(K, D1), maps:get(K, D2));
                         {false, true} -> maps:get(K, D2);
                         {true, false} -> maps:get(K, D1)
                     end
        end,
    maps:map(F, maps:merge(D1, D2));
union_({error, E1}, {error, E2}) -> {error, E1 ++ E2};
union_({error, _} = E, _) -> E;
union_(_, {error, _} = E) -> E;
union_(any, _) -> any;
union_(_, any) -> any;
union_(none, D) -> D;
union_(D, none) -> D;
union_({sum, D1}, {sum, D2}) -> {sum, ordsets:union(D1, D2)};
union_({sum, D1}, D) -> {sum, ordsets:add_element(D, D1)};
union_(D, {sum, D1}) -> union_({sum, D1}, D);
union_({tagged, Tag, D1}, {tagged, Tag, D2}) -> {tagged, Tag, union_(D1, D2)};
union_({product, D1}, {product, D2}) -> 
    {sum, ordsets:from_list([{product, D1}, {product, D2}])};
union_(D1, D2) -> {sum, ordsets:from_list([D1, D2])}.



intersection(Ds) when is_list(Ds) -> lists:foldl(fun(E1,E2) -> intersection(E1, E2) end, any, Ds).
intersection(D1, D2) -> compact(unroll(intersect_(D1, D2))).

intersect_({recur, S}, {recur, T}) -> {recur, fun() -> intersect_(S(), T()) end};
intersect_({recur, S}, {sum, _} = D) -> {recur, fun() -> intersect_(S(), D) end};
intersect_({recur, S}, {product, _} = D) -> {recur, fun() -> intersect_(S(), D) end};
intersect_({recur, S}, {tagged, _, _} = D) -> {recur, fun() -> intersect_(S(), D) end};
intersect_({recur, _}, _) -> none;
intersect_(D, {recur, S}) -> intersect_({recur, S}, D);

intersect_(D1, D2) when is_map(D1), is_map(D2) ->
    % When intersecting two maps we include all domains of the two maps.  This
    % is because a key is assumed to have domain `any` when it is not present
    % in a map and any narrower definition would need to be captured in the
    % intersection
    F = fun(K, _) -> case {maps:is_key(K, D1), maps:is_key(K, D2)} of
                         {true, true} -> intersect_(maps:get(K, D1), maps:get(K, D2));
                         {false, true} -> maps:get(K, D2);
                         {true, false} -> maps:get(K, D1)
                     end
        end,
    maps:map(F, maps:merge(D1, D2));

intersect_({error, E1}, {error, E2}) -> {error, E1 ++ E2};
intersect_({error, _} = E, _) -> E;
intersect_(_, {error, _} = E) -> E;

intersect_(any, D) -> D;
intersect_(D, any) -> D;
intersect_(none, _) -> none;
intersect_(_, none) -> none;

intersect_({f, F1}, {f, F2}) -> 
    case {get_arity(F1), get_arity(F2)} of
        {N, N} -> {f, mapfun(fun(Res1, Res2) -> intersect_(Res1, Res2) end, F1, F2)};
        _ -> none
    end;

intersect_({sum, D1}, {sum, D2}) -> 
    {sum, ordsets:from_list([intersect_(Dj, Di) || Di <- D1, Dj <- D2])}; 
intersect_({sum, D1}, D) -> 
    {sum, ordsets:from_list([intersect_(D, Di) || Di <- D1])};
intersect_(D, {sum, D1}) -> intersect_({sum, D1}, D);

intersect_({tagged, Tag, D1}, {tagged, Tag, D2}) -> 
    propagate_none({tagged, Tag, intersect_(D1, D2)});
intersect_({product, D1}, {product, D2}) -> propagate_none({product, intersect_(D1, D2)});

intersect_(D, D) -> D;
intersect_(_, _) -> none.


subset(D1, D2) -> diff(D1, intersection(D1, D2)) =:= none.


lookup({recur, F}, Elems) -> lookup(unroll(F()), Elems);
lookup({product, Map}, Elems) -> 
    compact({product, maps:from_list([{K, intersection(maps:get(K, Map), D)} || 
                              {K, D} <- maps:to_list(Elems), maps:is_key(K, Map)])});
lookup({tagged, _, D}, Elems) -> lookup(D, Elems).


compact(D) -> unroll(compact_(D)).
compact_({sum, S}) -> compact_sum({sum, S});
compact_({product, Map}) -> compact_product({product, Map});
compact_({tagged, Tag, Domain}) -> {tagged, Tag, compact_(Domain)};
compact_({f, DomainFun}) -> {f, mapfun(fun(D) -> compact_(D) end, DomainFun)};
compact_({recur, D}) -> {recur, fun() -> compact_(D()) end};
compact_(T) -> T.

compact_product({product, ProductMap}) ->
    Elements = [{K, compact_(V)} || {K, V} <- maps:to_list(ProductMap)],
    io:format("Elements: ~p~n", [Elements]),
    IsRecur = fun({_, {recur, _}}) -> true;
                 (_) -> false end,
    case lists:partition(IsRecur, Elements) of
        {[], []}            -> none;
        {[], _}             -> {product, maps:from_list(Elements)};
        {Recurs, Other} ->
            % If one or more recurs elements are present in the product, wrap
            % the entire product in a 'recur' domain
            {recur, fun() -> {product, maps:from_list(Other ++ [{K, D()} || {K, {_, D}} <- Recurs])} end}
    end.

compact_sum({sum, SumSet}) ->
    io:format("Sum: ~p~n", [SumSet]),
    Elements = [compact_(E) || E <- ordsets:to_list(SumSet)],
    io:format("Elements: ~p~n", [Elements]),
    IsRecur = fun({recur, _}) -> true;
                 (_) -> false end,
    case lists:partition(IsRecur, Elements) of
        {[], []}        -> none;
        {[], _}         -> list_to_sum(compact_sum_groups(Elements));
        {Recurs, Other} ->
        % If one or more recursive elements are present in the sum, make the
        % sum recursive and compact all elements
            {recur, fun() -> list_to_sum(compact_sum_groups(Other ++ [D() || {recur, D} <- Recurs])) end}
    end.


compact_sum_groups(Elements) ->

    % Filter out `none` and group by domain type
    KeyFun = fun({Type, _}) -> Type;
                (Other)     -> Other end,
    Keyed = [{K, V} || {K, V} <- group_by(KeyFun, Elements)],

    % Based on domain type compact appropriately
    L = lists:map(fun({product, Products}) -> compact_product_list(Products);
                     ({sum, Sums})         -> Sets = [Set || {sum, Set} <- Sums],
                                              SumElements = ordsets:to_list(ordsets:union(Sets)),
                                              compact_sum_groups(SumElements);
                     ({none, _})           -> [];
                     ({any, _})            -> [any];
                     ({_, Others})         -> Others
                  end, Keyed),

    lists:flatten(L).

list_to_sum([]) -> none;
list_to_sum([D]) -> D;
list_to_sum(Elems) when is_list(Elems) ->
    NoNones = [E || E <- Elems, not(E =:= none)],
    case lists:member(any, NoNones) of 
        true -> any; 
        _ -> {sum, ordsets:from_list(Elems)} 
    end;
list_to_sum(Domain) -> Domain.


compact_product_list(Products) -> 
    [{product, M} || M <- compact_maps([M || {product, M} <- Products])].

% Why is the union of two products tricky? 
%
% Because the union of a cartesian product is not equal to the union of each
% domain of the cartesian product.  to illustrate, consider the two products
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
    CountDomains = fun(Key) -> ordsets:size(ordsets:from_list([maps:get(Key, M, undefined) || M <- Maps])) end,
    Compare = fun(K1, K2) -> CountDomains(K1) < CountDomains(K2) end,
    Keys = lists:sort(Compare, lists:flatten([maps:keys(M) || M <- Maps])),
    Pivot = lists:nth(1, Keys),

    % We can do a union only if the product maps vary along a single dimension
    % (i.e. key). When there's only one key left, we know this to be the case.
    NumberOfUniqueKeys = ordsets:size(ordsets:from_list(Keys)),
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

unroll(D) -> unroll_(100, D).
unroll_(0, _) -> {error, [{{possibly_infinite_recursion}, {domain}}]};
unroll_(N, {recur, D}) -> unroll_(N-1, D());
unroll_(_, D) -> D.

group_by(F, L) -> dict:to_list(lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ])).

propagate_none({product, Map}) -> 
    case lists:member(none, maps:values(Map)) of
        true -> none;
        false -> {product, Map}
    end;
propagate_none({tagged, T, {product, Map}}) -> 
    case lists:member(none, maps:values(Map)) of
        true -> none;
        false -> {tagged, T, {product, Map}}
    end;
propagate_none(D) -> D.

gen_tag(F) -> 
    {name, Tag} = erlang:fun_info(F, name),
    Tag.

get_arity(Fun) ->
    proplists:get_value(arity, erlang:fun_info(Fun)).

is_recur({recur, _}) -> true;
is_recur(_)          -> false.

mapfun(Mapper, Fun) -> 
    case get_arity(Fun) of
        0  -> Mapper(Fun());
        1  -> fun(Arg) -> Mapper(Fun(Arg)) end;
        2  -> fun(A1, A2) -> Mapper(Fun(A1, A2)) end;
        3  -> fun(A1, A2, A3) -> Mapper(Fun(A1, A2, A3)) end;
        4  -> fun(A1, A2, A3, A4) -> Mapper(Fun(A1, A2, A3, A4)) end;
        5  -> fun(A1, A2, A3, A4, A5) -> Mapper(Fun(A1, A2, A3, A4, A5)) end;
        6  -> fun(A1, A2, A3, A4, A5, A6) -> Mapper(Fun(A1, A2, A3, A4, A5, A6)) end;
        7  -> fun(A1, A2, A3, A4, A5, A6, A7) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7)) end;
        8  -> fun(A1, A2, A3, A4, A5, A6, A7, A8) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8)) end;
        9  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9)) end;
        10 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) end;
        11 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) end;
        12 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) end;
        13 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) end
    end.

mapfun(Mapper, Fun1, Fun2) -> 
    case {get_arity(Fun1), get_arity(Fun2)} of
        {0, 0}   -> Mapper(Fun1(), Fun2());
        {1, 1}   -> fun(Arg) -> Mapper(Fun1(Arg), Fun2(Arg)) end;
        {2, 2}   -> fun(A1, A2) -> Mapper(Fun1(A1, A2), Fun2(A1, A2)) end;
        {3, 3}   -> fun(A1, A2, A3) -> Mapper(Fun1(A1, A2, A3), Fun2(A1, A2, A3)) end;
        {4, 4}   -> fun(A1, A2, A3, A4) -> Mapper(Fun1(A1, A2, A3, A4), Fun2(A1, A2, A3, A4)) end;
        {5, 5}   -> fun(A1, A2, A3, A4, A5) -> Mapper(Fun1(A1, A2, A3, A4, A5), Fun2(A1, A2, A3, A4, A5)) end;
        {6, 6}   -> fun(A1, A2, A3, A4, A5, A6) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6), Fun2(A1, A2, A3, A4, A5, A6)) end;
        {7, 7}   -> fun(A1, A2, A3, A4, A5, A6, A7) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7), Fun2(A1, A2, A3, A4, A5, A6, A7)) end;
        {8, 8}   -> fun(A1, A2, A3, A4, A5, A6, A7, A8) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8), Fun2(A1, A2, A3, A4, A5, A6, A7, A8)) end;
        {9, 9}   -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9)) end;
        {10, 10} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) end;
        {11, 11} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) end;
        {12, 12} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) end;
        {13, 13} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) end
    end.

-ifdef(TEST).

union_array_test() ->
    D = blip,
    Expected = blip,
    Actual = union([D]),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_one_key_test() ->
    D1 = {product, #{blip => true}},
    D2 = {product, #{blip => false}},
    Expected = {product, #{blip => {sum, ordsets:from_list([true, false])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_two_keys_mergeable_test() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = {product, #{blap => a, blip => {sum, ordsets:from_list([true, false])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_two_keys_non_mergeable_test() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blap => b, blip => false}},
    Expected = {sum, ordsets:from_list([{product, #{blap => a, blip => true}},
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
    Expected = {sum, ordsets:from_list([{product, #{blip => true, blap => false, blup => extra_old}},
                                     {product, #{blip => true, blap => true, blep => extra_new}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_product_same_keys_test() ->
    D1 = {product, #{blip => true, blap => false}},
    D2 = {product, #{blip => true, blap => true}},
    Expected = {product, #{blip => true, blap => {sum, ordsets:from_list([false, true])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_test() ->
    D1 = {sum, ordsets:from_list([a,b,c])},
    D2 = {sum, ordsets:from_list([b,c,d])},
    Expected = {sum, ordsets:from_list([a,b,c,d])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_none_test() ->
    D1 = {sum, ordsets:from_list([none])},
    D2 = {sum, ordsets:from_list([a,b])},
    Expected = {sum, ordsets:from_list([a,b])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_tagged_same_test() ->
    D1 = {tagged, kukkeluk, {product, #{blip => true}}},
    D2 = {tagged, kukkeluk, {product, #{blip => false}}},
    Expected = {tagged, kukkeluk, {product, #{blip => {sum, ordsets:from_list([true, false])}}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_tagged_diff_test() ->
    D1 = {tagged, kukkeluk, {product, #{blip => true, blap => false, blup => extra_old}}},
    D2 = {tagged, kakkelak, {product, #{blip => true, blap => true, blep => extra_new}}},
    Expected = {sum, ordsets:from_list([{tagged, kukkeluk, {product, #{blip => true, blap => false, blup => extra_old}}},
                                     {tagged, kakkelak, {product, #{blip => true, blap => true, blep => extra_new}}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_of_products_test() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2}}])},
    D2 = {sum, ordsets:from_list([{product, #{a => 1, c => 3}}])},
    Expected = {sum, ordsets:from_list([{product, #{a => 1, b => 2}}, {product, #{a => 1, c => 3}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_of_many_products_test() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => 1}},
                                  {product, #{a => 2, b => 2, c => 2}},
                                  {product, #{a => 1, b => 2, c => 3}},
                                  {product, #{a => 2, b => 2, c => 4}}])},
    D2 = {sum, ordsets:from_list([{product, #{a => 1}}])},
    Expected = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => {sum, ordsets:from_list([1, 3])}}}, 
                                        {product, #{a => 2, b => 2, c => {sum, ordsets:from_list([2, 4])}}},
                                        {product, #{a => 1}}])},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_with_none_test() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => 1}}, none])},
    D2 = {sum, ordsets:from_list([none])},
    Expected = {product, #{a => 1, b => 2, c => 1}}, 
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_with_any_test() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => 1}}, any])},
    D2 = {sum, ordsets:from_list([none])},
    Expected = any,
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_sum_of_products_with_same_keys_test() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2}}])},
    D2 = {sum, ordsets:from_list([{product, #{a => 1, b => 3}}])},
    Expected = {product, #{a => 1, b => {sum, ordsets:from_list([2,3])}}},
    Actual = union(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

union_recur_recur_sum_sum_test() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = union({recur, R}, {recur, S}),
    ?assertMatch({sum, [a, b, c, {recur, _}, {recur, _}]}, Actual).

intersection_array_test() ->
    D = blip,
    Expected = blip,
    Actual = intersection([D]),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_product_one_key_test() ->
    D1 = {product, #{blip => true}},
    D2 = {product, #{blip => false}},
    Expected = none,
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_product_two_keys_mergeable_test() ->
    D1 = {product, #{blap => a, blip => {sum, ordsets:from_list([false,true])}}},
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
    D1 = {sum, ordsets:from_list([a,b,c])},
    D2 = {sum, ordsets:from_list([b,c,d])},
    Expected = {sum, ordsets:from_list([b,c])},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_any_test() ->
    D1 = {sum, ordsets:from_list([any])},
    D2 = {sum, ordsets:from_list([a,b])},
    Expected = {sum, ordsets:from_list([a,b])},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_tagged_same_test() ->
    D1 = {tagged, kukkeluk, {product, #{blip => {sum, ordsets:from_list([false, true])}}}},
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
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2}}])},
    D2 = {sum, ordsets:from_list([{product, #{a => 1, b => {sum, ordsets:from_list([2,3])}}}])},
    Expected = {product, #{a => 1, b => 2}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_with_none_test() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => 1}}, none])},
    D2 = {sum, ordsets:from_list([none])},
    Expected = none,
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_sum_with_any_test() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => 1}}])},
    D2 = {sum, ordsets:from_list([any])},
    Expected = {product, #{a => 1, b => 2, c => 1}},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_f_test() ->
    D1 = {f, fun([A], _) -> A end},
    D2 = {f, fun([A], _) -> {sum, ordsets:from_list([A, b])} end},
    Expected = a,
    {f, DomainFun} = intersection(D1, D2),
    Actual = DomainFun([a], []),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_map_map_test() ->
    D1 = #{a => any, b => any},
    D2 = #{b => blup, c => none},
    Expected = #{a => any, b => blup, c => none},
    Actual = intersection(D1, D2),
    ?assertEqual(none, diff(Expected, Actual)).

intersection_recur_val_test() ->
    R = fun R() -> {sum, ordsets:from_list([{product, #{recurse => {recur, R}}}])} end,
    Input = {recur, R},
    ?assertEqual(none, intersection(Input, val)).

intersection_sum_sum_recur_recur_test() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = intersection(R(), S()),
    ?assertMatch({sum, [b, {recur, _}]}, Actual).

intersection_sum_recur_recur_sum_test() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = intersection(R(), {recur, S}),
    ?assertMatch({sum, [b, {recur, _}]}, Actual).

intersection_recur_recur_sum_sum_test() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = intersection({recur, R}, {recur, S}),
    ?assertMatch({sum, [b, {recur, _}]}, Actual).

intersection_recur_recur_sum_sum_product_test() ->
    P1 = {product, #{a => 1, b => {sum, ordsets:from_list([2,3])}}},
    P2 = {product, #{a => 1, b => {sum, ordsets:from_list([3,4])}}},
    R = fun R() -> {sum, ordsets:from_list([P1, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([P2, {recur, S}])} end,
    Actual = intersection({recur, R}, {recur, S}),
    P3 = {product, #{a => 1, b => 3}},
    ?assertMatch({sum, [P3, {recur, _}, {recur, _}, {recur, _}]}, Actual).

intersection_infinite_recur_test() ->
    Inf1 = fun I() -> {recur, I} end,
    Actual = compact(Inf1()),
    Expected = {error, [{{possibly_infinite_recursion}, {domain}}]},
    ?assertEqual(Expected, Actual).

diff_sum_product_test() ->
    Old = {product, #{blah => {values, ordsets:from_list([true, false])} } },
    New = {sum, ordsets:from_list([true, false])},
    Expected = #{old => Old,
                 new => New},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_sum_sum_test() ->
    Old = {sum, ordsets:from_list([blip, blop, honk])},
    New = {sum, ordsets:from_list([true, false, honk])},
    Expected = {sum, #{only_in_old => [blip, blop],
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
    Old = {sum, ordsets:from_list([blip, blop, honk])},
    New = {sum, ordsets:from_list([blip, blop, honk])},
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
    Old = {product, #{blah => {sum, ordsets:from_list([true, false])} } },
    New = {product, #{blah => {sum, ordsets:from_list([true, false, blap])} } },
    Expected = {product, #{only_in_old => [],
                           only_in_new => [],
                           diff => [{blah, {sum, #{only_in_old => [],
                                                   only_in_new => [blap]}}}]}},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_nested2_test() ->
    OldProduct = {product, #{blip => true}},
    NewProduct = {product, #{blap => false}},
    Old = {sum, ordsets:from_list([OldProduct])},
    New = {sum, ordsets:from_list([NewProduct])},
    Expected = {sum, #{only_in_old => [OldProduct],
                       only_in_new => [NewProduct]}},
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_equal_nested1_test() ->
    Old = {sum, ordsets:from_list([{product, #{blip => {sum, ordsets:from_list([c,b,a])}}}])},
    New = {sum, ordsets:from_list([{product, #{blip => {sum, ordsets:from_list([a,b,c])}}}])},
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
    Old = {tagged, kukkeluk, {sum, ordsets:from_list([a,b])}},
    New = {tagged, kukkeluk, {sum, ordsets:from_list([a,b])}},
    Expected = none,
    Actual = diff(Old, New),
    ?assertEqual(Expected, Actual).

diff_recur_recur_test() ->
    F = fun() -> f end,
    G = fun() -> g end,
    ?assertEqual(none, diff({recur, F}, {recur, F})),
    Expected = #{old => 'f', new => 'g'},
    ?assertEqual(Expected, diff({recur, F}, {recur, G})).

diff_recur_other_test() ->
    R = fun R() -> {product, #{recurse => {recur, R}}} end,
    ?assertEqual(none, diff(R(), {recur, R})),

    S = fun S() -> {product, #{blup => {recur, S}}} end,
    Expected = {product, #{only_in_old => [recurse], only_in_new => [blup], diff => []}},
    ?assertEqual(Expected, diff(R(), {recur, S})).

diff_sum_recur_diff_test() ->
    Old = fun O() -> {sum, ordsets:from_list([{recur, O}])} end,
    New = fun N() -> {sum, ordsets:from_list([{recur, N}])} end,
    ?assertEqual(none, diff(Old(), New())).


compact_sum_sum_test() ->
    Expected = {sum, ordsets:from_list([a, b, c])},
    Actual = compact({sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}),
    ?assertEqual(Expected, Actual).

compact_product_sum_sum_test() ->
    Expected = {product, #{a => {sum, ordsets:from_list([a, b, c])}}},
    Actual = compact({product, #{a => {sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}}}),
    ?assertEqual(Expected, Actual).

compact_tagged_sum_sum_test() ->
    Expected = {tagged, t, {sum, ordsets:from_list([a, b, c])}},
    Actual = compact({tagged, t, {sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}}),
    ?assertEqual(Expected, Actual).

compact_sum_tagged_sum_sum_test() ->
    Expected = {tagged, t, {sum, ordsets:from_list([a, b, c])}},
    Actual = compact({sum, ordsets:from_list([{tagged, t, {sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}}])}),
    ?assertEqual(none, diff(Expected, Actual)).

compact_sum_item_test() ->
    Expected = {sum, ordsets:from_list([a, b, c])},
    Actual = compact({sum, ordsets:from_list([a, b, {sum, ordsets:from_list([c])}])}),
    ?assertEqual(none, diff(Expected, Actual)).

compact_f_test() ->
    Input = {f, fun(A1, A2) -> {sum, ordsets:from_list([{sum, ordsets:from_list([A1, A2])}])} end},
    Expected = {sum, ordsets:from_list([a1, a2])},
    {f, DomainFun} = compact(Input),
    Actual = DomainFun(a1, a2),
    ?assertEqual(none, diff(Expected, Actual)).

compact_recur_test() ->
    R = fun R() -> {sum, ordsets:from_list([a, {product, #{recurse => {recur, R}}}])} end,
    Input = {recur, R},
    Expected = {recur, R},
    ?assertEqual(none, diff(Expected, compact(Input))).

compact_recur_flatten_test() ->
    % Sum with one item will be compacted: _____________________________________
    R = fun R() -> {sum, ordsets:from_list([{product, #{recurse => {recur, R}}}])} end,
    Input = {recur, R},
    Actual = compact(Input),
    ?assertMatch({product, _}, Actual),
    {product, Map} = Actual,
    ?assertMatch([{recurse, {recur, _}}], maps:to_list(Map)).

compact_list_test() ->
    List = fun List() -> {sum, ordsets:from_list(['List/Nil',
                                                  {tagged, 'List/Cons', 
                                                   #{head => 'List/Nil',
                                                     tail => {recur, List}}}])} end,
    Input = List(),
    Actual = compact(Input),
    ?assertEqual(none, diff(Input, Actual)).


subset_sum_sum_test() ->
    D1 = {sum, ordsets:from_list([a, b, c])},
    D2 = {sum, ordsets:from_list([a, b, c, d])},
    ?assertEqual(true, subset(D1, D2)).

subset_non_sum_sum_test() ->
    D1 = {sum, ordsets:from_list([a, b, c])},
    D2 = {sum, ordsets:from_list([b, c, d])},
    ?assertEqual(false, subset(D1, D2)).
    
subset_product_product_test() ->
    D1 = {product, #{a => 1, b => 2}},
    D2 = {product, #{a => 1}},
    ?assertEqual(true, subset(D1, D2)).

subset_non_product_product_test() ->
    D1 = {product, #{a => 1, b => 2}},
    D2 = {product, #{a => 2}},
    ?assertEqual(false, subset(D1, D2)).

subset_tagged_tagged_test() ->
    D1 = {tagged, t, t},
    D2 = {tagged, t, {sum, ordsets:from_list([s, t])}},
    ?assertEqual(true, subset(D1, D2)).

subset_non_tagged_tagged_test() ->
    D1 = {tagged, t, t},
    D2 = {tagged, s, {sum, ordsets:from_list([s, t])}},
    ?assertEqual(false, subset(D1, D2)).

subset_sum_product_test() ->
    D1 = {product, #{a => 2, b => 3, c => 4}},
    D2 = {sum, ordsets:from_list([{product, #{a => 1, b => 2}},
                               {product, #{a => 2, b => 3}}])},
    ?assertEqual(true, subset(D1, D2)).


lookup_product_test() ->
    D = {product, #{a => 'A', b => 'B'}},
    Elems = #{a => any},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?assertEqual(none, diff(Expected, Actual)).

lookup_recur_test() ->
    D = {recur, fun() -> {product, #{a => 'A', b => 'B'}} end},
    Elems = #{a => any},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?assertEqual(none, diff(Expected, Actual)).

lookup_domain_intersection_test() ->
    D = {product, #{a => {sum, ordsets:from_list(['A', 'B'])}}},
    Elems = #{a => 'A'},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?assertEqual(none, diff(Expected, Actual)).

lookup_tagged_test() -> 
    D = {tagged, tag, {product, #{a => 'A', b => 'B'}}},
    Elems = #{a => any},
    Expected = {product, #{a => 'A'}},
    Actual = lookup(D, Elems),
    ?assertEqual(none, diff(Expected, Actual)).


-endif.
