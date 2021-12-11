-module(diff_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/macros.hrl").

diff_sum_product_test_() ->
    Old = #{blah => {values, ordsets:from_list([true, false])} } ,
    New = {sum, ordsets:from_list([true, false])},
    Expected = #{old => Old,
                 new => New},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_sum_sum_test_() ->
    Old = {sum, ordsets:from_list([blip, blop, honk])},
    New = {sum, ordsets:from_list([true, false, honk])},
    Expected = {sum, #{only_in_old => [blip, blop],
                       only_in_new => [false, true]}},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_product_product_test_() ->
    Old = #{blip => true, blap => false, blup => extra_old},
    New = #{blip => true, blap => true, blep => extra_new},
    Expected = #{only_in_old => [blup], 
                          only_in_new => [blep], 
                          diff => [{blap, #{old => false,
                                            new => true}}]},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_equal_sum_sum_test_() ->
    Old = {sum, ordsets:from_list([blip, blop, honk])},
    New = {sum, ordsets:from_list([blip, blop, honk])},
    Expected = none,
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_equal_product_product_test_() ->
    Old = #{blip => true, blap => false, blup => extra_old},
    New = #{blip => true, blap => false, blup => extra_old},
    Expected = none,
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_nested1_test_() ->
    Old = #{blah => {sum, ordsets:from_list([true, false])} } ,
    New = #{blah => {sum, ordsets:from_list([true, false, blap])} } ,
    Expected = #{only_in_old => [],
                          only_in_new => [],
                          diff => [{blah, {sum, #{only_in_old => [],
                                                  only_in_new => [blap]}}}]},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_nested2_test_() ->
    OldProduct = #{blip => true},
    NewProduct = #{blap => false},
    Old = {sum, ordsets:from_list([OldProduct])},
    New = {sum, ordsets:from_list([NewProduct])},
    Expected = {sum, #{only_in_old => [OldProduct],
                       only_in_new => [NewProduct]}},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_equal_nested1_test_() ->
    Old = {sum, ordsets:from_list([#{blip => {sum, ordsets:from_list([c,b,a])}}])},
    New = {sum, ordsets:from_list([#{blip => {sum, ordsets:from_list([a,b,c])}}])},
    Expected = none,
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_tagged_test_() ->
    Old = {tagged, kukkeluk, true},
    New = {tagged, kukkeluk, false},
    Expected = {tagged, kukkeluk, #{old => true, new => false}},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_tagged_sum_test_() ->
    Old = {tagged, kukkeluk, {sum, ordsets:from_list([a,b])}},
    New = {tagged, kukkeluk, {sum, ordsets:from_list([a,b])}},
    Expected = none,
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_f_f_test_() ->
    F = fun(_A, _B) -> any end,
    G = fun(_A, _B) -> none end,
    [?test(none, domain:diff(F, F)),
     ?test(#{old := F, new := G}, domain:diff(F, G))].

diff_recur_recur_test_() ->
    F = fun() -> f end,
    G = fun() -> g end,
    Expected = #{old => 'f', new => 'g'},
    [?_assertEqual(none, domain:diff({recur, F}, {recur, F})),
     ?_assertEqual(Expected, domain:diff({recur, F}, {recur, G}))].

diff_recur_other_test_() ->
    R = fun R() -> #{recurse => {recur, R}} end,

    S = fun S() -> #{blup => {recur, S}} end,
    Expected = #{only_in_old => [recurse], only_in_new => [blup], diff => []},
    [?_assertEqual(none, domain:diff(R(), {recur, R})),
     ?_assertEqual(Expected, domain:diff(R(), {recur, S}))].

diff_sum_recur_diff_test_() ->
    Old = fun O() -> {sum, ordsets:from_list([{recur, O}])} end,
    New = fun N() -> {sum, ordsets:from_list([{recur, N}])} end,
    ?_assertEqual(none, domain:diff(Old(), New())).

diff_list_test_() ->
    [?test(none, domain:diff({list, [1, 2, 3]}, {list, [1, 2, 3]})),
     ?testEqual({list, [none, #{old => 2, new => b}, none]},
                domain:diff({list, [1, 2, 3]}, {list, [1, b, 3]})),
     ?testEqual({list, #{only_in_old => [], only_in_new => [c], diff => [none, #{old => 2, new => b}]}},
                domain:diff({list, [1, 2]}, {list, [1, b, c]}))].

whatever_test_() ->
    [?testEqual(none, domain:diff(whatever, 1)),
     ?testEqual(none, domain:diff(1, whatever))].

diff_sum_non_sum_test_() ->
    [?testEqual(none, domain:diff(1, {sum, ordsets:from_list([1, 1])}))].
