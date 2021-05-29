-module(compact_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

set(Elems) when is_list(Elems) -> set(Elems).

compact_sum_sum_test_() ->
    Expected = {sum, set([a, b, c])},
    Actual = domain:compact({sum, set([a,{sum, set([b,c])}])}),
    ?_assertEqual(Expected, Actual).

compact_product_sum_sum_test_() ->
    Expected = #{a => {sum, set([a, b, c])}},
    Actual = domain:compact(#{a => {sum, set([a,{sum, set([b,c])}])}}),
    ?_assertEqual(Expected, Actual).

compact_tagged_sum_sum_test_() ->
    Expected = {tagged, t, {sum, set([a, b, c])}},
    Actual = domain:compact({tagged, t, {sum, set([a,{sum, set([b,c])}])}}),
    ?_assertEqual(Expected, Actual).

compact_sum_tagged_sum_sum_test_() ->
    Expected = {tagged, t, {sum, set([a, b, c])}},
    Actual = domain:compact({sum, set([{tagged, t, {sum, set([a,{sum, set([b,c])}])}}])}),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_sum_item_test_() ->
    Expected = {sum, set([a, b, c])},
    Actual = domain:compact({sum, set([a, b, {sum, set([c])}])}),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_f_test_() ->
    Input = fun(A1, A2) -> {sum, set([{sum, set([A1, A2])}])} end,
    Expected = {sum, set([a1, a2])},
    DomainFun = domain:compact(Input),
    Actual = DomainFun(a1, a2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_recur_test_() ->
    R = fun R() -> {sum, set([a, #{recurse => {recur, R}}])} end,
    Input = {recur, R},
    Expected = {recur, R},
    ?_assertEqual(none, domain:diff(Expected, domain:compact(Input))).

compact_recur_flatten_test_() ->
    % Sum with one item will be compacted: _____________________________________
    R = fun R() -> {sum, set([#{recurse => {recur, R}}])} end,
    Input = {recur, R},
    Actual = domain:compact(Input),
    [?_assertMatch([{recurse, {recur, _}}], maps:to_list(Actual))].

compact_recursive_linked_list_test_() ->
    List = fun List() -> {sum, set(['List/Nil',
                                    {tagged, 'List/Cons', 
                                     #{head => 'List/Nil',
                                       tail => {recur, List}}}])} end,
    Input = List(),
    Actual = domain:compact(Input),
    ?_assertEqual(none, domain:diff(Input, Actual)).

list_compact_test_() ->
    L = [{sum, set([b, none])}, {sum, set([2, none])}],
    Expected = [b, 2],
    ?testEqual(Expected, domain:compact(L)).

sum_list_compact_test_() ->
    L = {sum, set([a, [1, 2]])},
    Expected = {sum, set([a, [1, 2]])},
    ?testEqual(Expected, domain:compact(L)).

sum_list_sum_compact_test_() ->
    L = {sum, set([[{sum, set([a, 1])}, {sum, set([b, 2])}], [1, 2]])},
    Expected = [{sum, set([a, 1])}, {sum, set([b, 2])}],
    ?testEqual(Expected, domain:compact(L)).
