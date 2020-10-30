-module(compact).
-import(union, [union/1]).
-export([compact/1]).
-include_lib("eunit/include/eunit.hrl").

compact({sum, S}) -> compact_sum({sum, S});
compact(Map) when is_map(Map) -> compact_product(Map);
compact({tagged, Tag, Domain}) -> {tagged, Tag, compact(Domain)};
compact(L) when is_list(L) -> [compact(E) || E <- L];
compact(F) when is_function(F) -> utils:mapfun(fun(D) -> compact(D) end, F);
compact({recur, D}) -> {recur, fun() -> compact(D()) end};
compact(T) -> T.

compact_product(ProductMap) ->
    Elements = [{K, compact(V)} || {K, V} <- maps:to_list(ProductMap)],
    IsRecur = fun({_, {recur, _}}) -> true;
                 (_) -> false end,
    case lists:partition(IsRecur, Elements) of
        {[], []}            -> none;
        {[], _}             -> maps:from_list(Elements);
        {Recurs, Other} ->
            % If one or more recurs elements are present in the product, wrap
            % the entire product in a 'recur' domain
            F = fun() -> Elems = Other ++ [{K, D()} || {K, {_, D}} <- Recurs],
                         maps:from_list(Elems) end,
            {recur, F}
    end.

compact_sum({sum, SumSet}) ->
    Elements = [compact(E) || E <- ordsets:to_list(SumSet)],
    IsRecur = fun({recur, _})   -> true;
                 (_)            -> false end,
    case lists:partition(IsRecur, Elements) of
        {[], []}        -> none;
        {[], _}         -> list_to_sum(compact_sum_groups(Elements));
        {Recurs, Other} ->
        % If one or more recursive elements are present in the sum, make the
        % sum recursive and compact all elements
            F = fun() -> Elems = Other ++ [D() || {recur, D} <- Recurs],
                         list_to_sum(compact_sum_groups(Elems)) end,
            {recur, F}
    end.


compact_sum_groups(Elements) ->

    % Filter out `none` and group by domain type
    KeyFun = fun({Type, _}) -> Type;
                (Map) when is_map(Map) -> product;
                (List) when is_list(List) -> list;
                (Other)     -> Other end,
    Keyed = [{K, V} || {K, V} <- utils:group_by(KeyFun, Elements)],
    
    % Based on domain type compact appropriately
    L = lists:map(fun({product, Products}) -> compact_maps(Products);
                     ({list, Lists})       -> compact_lists(Lists);
                     ({sum, Sums})         -> Sets = [Set || {sum, Set} <- Sums],
                                              SumElements = ordsets:to_list(ordsets:union(Sets)),
                                              compact_sum_groups(SumElements);
                     ({recur, Rs})         -> F = fun() -> union:union([D() || {_, D} <- Rs]) end,
                                              [{recur, F}];
                     ({none, _})           -> [];
                     ({any, _})            -> [any];
                     ({_, Others})         -> Others
                  end, Keyed),

    [E || Es <- L, E <- Es].

list_to_sum([]) -> none;
list_to_sum([D]) -> D;
list_to_sum(Elems) when is_list(Elems) ->
    NoNones = [E || E <- Elems, not(E =:= none)],
    case lists:member(any, NoNones) of 
        true -> any; 
        _ -> {sum, ordsets:from_list(Elems)} 
    end;
list_to_sum(Domain) -> Domain.


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
% number of domains. All products are then grouped based on the domain of the
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
        1 -> [#{Pivot => union:union([maps:get(Pivot, M, any) || M <- Maps])}];
        _ -> 
            % Group maps by domain of pivot key
            Grouped = utils:group_by(fun(Map) -> maps:get(Pivot, Map, undefined) end, Maps),

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
            [Compacted || {Domain, Group} <- Grouped,
                          Compacted <- Compact(Pivot, Domain, Group)]
    end.

compact_lists([]) -> [];
compact_lists([L]) -> [L];
compact_lists(Lists) ->
    Grouped = utils:group_by(fun(L) -> length(L) end, Lists),
    [domain:union(Group) || {_Domain, Group} <- Grouped].
