-module(normalize_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

set(Elems) when is_list(Elems) -> ordsets:from_list(Elems).
sum(Elems) when is_list(Elems) -> {sum, set(Elems)}.

normalize_sum_sum_test_() ->
    Actual = domain:normalize({sum, set([a,{sum, set([b,c])}])}),
    Expected = {sum, set([a, b, c])},
    ?_assertEqual(Expected, Actual).

normalize_product_sum_sum_test_() ->
    Actual = domain:normalize(#{a => {sum, set([a,{sum, set([b,c])}])}}),
    ?test({sum, [#{a := a}, #{a := b}, #{a := c}]}, Actual).

normalize_tagged_sum_sum_test_() ->
    Actual = domain:normalize({tagged, t, {sum, set([a,{sum, set([b,c])}])}}),
    Expected = {sum, set([{tagged, t, a}, {tagged, t, b}, {tagged, t, c}])},
    ?_assertEqual(Expected, Actual).

normalize_sum_tagged_sum_sum_test_() ->
    Actual = domain:normalize({sum, set([{tagged, t, {sum, set([a,{sum, set([b,c])}])}}])}),
    Expected = {sum, set([{tagged, t, a}, {tagged, t, b}, {tagged, t, c}])},
    ?_assertEqual(none, domain:diff(Expected, Actual)).

normalize_sum_item_test_() ->
    Actual = domain:normalize({sum, set([a, b, {sum, set([c])}])}),
    Expected = {sum, set([a, b, c])},
    ?_assertEqual(none, domain:diff(Expected, Actual)).

normalize_recur_test_() ->
    R = fun R() -> {sum, set([a, #{recurse => {recur, R}}])} end,
    Input = {recur, R},
    Expected = {recur, R},
    ?_assertEqual(none, domain:diff(Expected, domain:normalize(Input))).

normalize_recursive_linked_list_test_() ->
    List = fun List() -> {sum, set(['List/Nil',
                                    {tagged, 'List/Cons',
                                     #{head => 'List/Nil',
                                       tail => {recur, List}}}])} end,
    Input = List(),
    Actual = domain:normalize(Input),
    ?_assertEqual(none, domain:diff(Input, Actual)).

list_normalize_test_() ->
    L = [{sum, set([b, none])}, {sum, set([2, none])}],
    Expected = [b, 2],
    ?testEqual(Expected, domain:normalize(L)).

sum_list_normalize_test_() ->
    L = {sum, set([a, [1, 2]])},
    Expected = {sum, set([a, [1, 2]])},
    ?testEqual(Expected, domain:normalize(L)).

simple_sum_list_sum_test_() ->
    L = sum([[sum([a, 1]), b]]),
    Expected = sum([[a, b], [1, b]]),
    ?testEqual(Expected, domain:normalize(L)).

sum_list_sum_normalize_test_() ->
    L = sum([[sum([a, 1]), sum([b, 2])],
             [1, 2]]),
    Expected = sum([[a, b], [a, 2], [1, b], [1, 2]]),
    ?testEqual(Expected, domain:normalize(L)).

list_sum_test_() ->
    L = [sum([a, b])],
    Expected = sum([[a], [b]]),
    ?testEqual(Expected, domain:normalize(L)).
    
sum_tuple_sum_test_() ->
    T = sum([{sum([a, 1]), sum([b, 2])},
             {1, 2}]),
    Expected = sum([{a, b}, {a, 2}, {1, b}, {1, 2}]),
    ?testEqual(Expected, domain:normalize(T)).

