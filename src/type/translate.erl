-module(translate).
-export([to_core/1]).

-include_lib("eunit/include/eunit.hrl").

to_core({sum, S}) ->
    Elems = ordsets:to_list(S),
    list_to_tuple([sum | [to_core(E) || E <- lists:sort(Elems)]]);

to_core({product, M}) ->
    {_, Values} = lists:unzip(lists:sort(maps:to_list(M))),
    list_to_tuple([product | [to_core(E) || E <- Values]]);

to_core({tagged, _, D}) -> {tagged, to_core(D)};

to_core({f, _, F}) -> F;

to_core(D) -> D.

-ifdef(TEST).

sum_test() ->
    Actual = to_core({sum, ordsets:from_list(['A', 'B'])}),
    Expected = {sum, 'A', 'B'},
    ?assertEqual(Expected, Actual).

sum_order_test() ->
    Actual = to_core({sum, ordsets:from_list(['B', 'A'])}),
    Expected = {sum, 'A', 'B'},
    ?assertEqual(Expected, Actual).

product_test() ->
    Actual = to_core({product, #{a => 'A', b => 'B'}}),
    Expected = {product, 'A', 'B'},
    ?assertEqual(Expected, Actual).
    
product_order_test() ->
    Actual = to_core({product, #{z => 'A', q => 'B'}}),
    Expected = {product, 'B', 'A'},
    ?assertEqual(Expected, Actual).

sum_product_sum_test() ->
    Actual = to_core({sum, ordsets:from_list(['A',
                                      {product, #{a => {sum, ordsets:from_list(['C', 'D'])}, 
                                                          b => 'B'}}])}),
    Expected = {sum, 'A', {product, {sum, 'C', 'D'}, 'B'}},
    ?assertEqual(Expected, Actual).

tagged_test() ->
    Actual = to_core({tagged, 'Tag', {product, #{z => 'A', q => 'B'}}}),
    Expected = {tagged, {product, 'B', 'A'}},
    ?assertEqual(Expected, Actual).


-endif.
