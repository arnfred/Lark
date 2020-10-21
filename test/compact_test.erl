-module(compact_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

compact_sum_sum_test_() ->
    Expected = {sum, ordsets:from_list([a, b, c])},
    Actual = domain:compact({sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}),
    ?_assertEqual(Expected, Actual).

compact_product_sum_sum_test_() ->
    Expected = #{a => {sum, ordsets:from_list([a, b, c])}},
    Actual = domain:compact(#{a => {sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}}),
    ?_assertEqual(Expected, Actual).

compact_tagged_sum_sum_test_() ->
    Expected = {tagged, t, {sum, ordsets:from_list([a, b, c])}},
    Actual = domain:compact({tagged, t, {sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}}),
    ?_assertEqual(Expected, Actual).

compact_sum_tagged_sum_sum_test_() ->
    Expected = {tagged, t, {sum, ordsets:from_list([a, b, c])}},
    Actual = domain:compact({sum, ordsets:from_list([{tagged, t, {sum, ordsets:from_list([a,{sum, ordsets:from_list([b,c])}])}}])}),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_sum_item_test_() ->
    Expected = {sum, ordsets:from_list([a, b, c])},
    Actual = domain:compact({sum, ordsets:from_list([a, b, {sum, ordsets:from_list([c])}])}),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_f_test_() ->
    Input = fun(A1, A2) -> {sum, ordsets:from_list([{sum, ordsets:from_list([A1, A2])}])} end,
    Expected = {sum, ordsets:from_list([a1, a2])},
    DomainFun = domain:compact(Input),
    Actual = DomainFun(a1, a2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_recur_test_() ->
    R = fun R() -> {sum, ordsets:from_list([a, #{recurse => {recur, R}}])} end,
    Input = {recur, R},
    Expected = {recur, R},
    ?_assertEqual(none, domain:diff(Expected, domain:compact(Input))).

compact_recur_flatten_test_() ->
    % Sum with one item will be compacted: _____________________________________
    R = fun R() -> {sum, ordsets:from_list([#{recurse => {recur, R}}])} end,
    Input = {recur, R},
    Actual = domain:compact(Input),
    [?_assertMatch([{recurse, {recur, _}}], maps:to_list(Actual))].

compact_recursive_linked_list_test_() ->
    List = fun List() -> {sum, ordsets:from_list(['List/Nil',
                                                  {tagged, 'List/Cons', 
                                                   #{head => 'List/Nil',
                                                     tail => {recur, List}}}])} end,
    Input = List(),
    Actual = domain:compact(Input),
    ?_assertEqual(none, domain:diff(Input, Actual)).

list_compact_test_() ->
    L = [{sum, ordsets:from_list([b, none])}, {sum, ordsets:from_list([2, none])}],
    Expected = [b, 2],
    ?testEqual(Expected, domain:compact(L)).

sum_list_compact_test_() ->
    L = {sum, ordsets:from_list([a, [1, 2]])},
    Expected = {sum, ordsets:from_list([a, [1, 2]])},
    ?testEqual(Expected, domain:compact(L)).

sum_list_sum_compact_test_() ->
    L = {sum, ordsets:from_list([[{sum, ordsets:from_list([a, 1])}, {sum, ordsets:from_list([b, 2])}], [1, 2]])},
    Expected = [{sum, ordsets:from_list([a, 1])}, {sum, ordsets:from_list([b, 2])}],
    ?testEqual(Expected, domain:compact(L)).
