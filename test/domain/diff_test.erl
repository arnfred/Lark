-module(diff_test).

-include_lib("eunit/include/eunit.hrl").

diff_sum_product_test_() ->
    Old = {product, #{blah => {values, maps:from_list([{E, true} || E <- [true, false]])} } },
    New = {sum, maps:from_list([{E, true} || E <- [true, false]])},
    Expected = #{old => Old,
                 new => New},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_sum_sum_test_() ->
    Old = {sum, maps:from_list([{E, true} || E <- [blip, blop, honk]])},
    New = {sum, maps:from_list([{E, true} || E <- [true, false, honk]])},
    Expected = {sum, #{diff => [],
                       only_in_old => [blip, blop],
                       only_in_new => [false, true]}},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_product_product_test_() ->
    Old = {product, #{blip => true, blap => false, blup => extra_old}},
    New = {product, #{blip => true, blap => true, blep => extra_new}},
    Expected = {product, #{only_in_old => [blup], 
                           only_in_new => [blep], 
                           diff => [{blap, #{old => false,
                                             new => true}}]}},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_equal_sum_sum_test_() ->
    Old = {sum, maps:from_list([{E, true} || E <- [blip, blop, honk]])},
    New = {sum, maps:from_list([{E, true} || E <- [blip, blop, honk]])},
    Expected = none,
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_equal_product_product_test_() ->
    Old = {product, #{blip => true, blap => false, blup => extra_old}},
    New = {product, #{blip => true, blap => false, blup => extra_old}},
    Expected = none,
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_nested1_test_() ->
    Old = {product, #{blah => {sum, maps:from_list([{E, true} || E <- [true, false]])} } },
    New = {product, #{blah => {sum, maps:from_list([{E, true} || E <- [true, false, blap]])} } },
    Expected = {product, #{only_in_old => [],
                           only_in_new => [],
                           diff => [{blah, {sum, #{diff => [],
                                                   only_in_old => [],
                                                   only_in_new => [blap]}}}]}},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_nested2_test_() ->
    OldProduct = {product, #{blip => true}},
    NewProduct = {product, #{blap => false}},
    Old = {sum, maps:from_list([{E, true} || E <- [OldProduct]])},
    New = {sum, maps:from_list([{E, true} || E <- [NewProduct]])},
    Expected = {sum, #{diff => [],
                       only_in_old => [OldProduct],
                       only_in_new => [NewProduct]}},
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_equal_nested1_test_() ->
    Old = {sum, #{{product, #{blip => {sum, maps:from_list([{E, true} || E <- [c,b,a]])}}} => true}},
    New = {sum, #{{product, #{blip => {sum, maps:from_list([{E, true} || E <- [a,b,c]])}}} => true}},
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
    Old = {tagged, kukkeluk, {sum, maps:from_list([{E, true} || E <- [a,b]])}},
    New = {tagged, kukkeluk, {sum, maps:from_list([{E, true} || E <- [a,b]])}},
    Expected = none,
    Actual = domain:diff(Old, New),
    ?_assertEqual(Expected, Actual).

diff_recur_recur_test_() ->
    F = fun() -> f end,
    G = fun() -> g end,
    Expected = #{old => 'f', new => 'g'},
    [?_assertEqual(none, domain:diff({recur, F}, {recur, F})),
     ?_assertEqual(Expected, domain:diff({recur, F}, {recur, G}))].

diff_recur_other_test_() ->
    R = fun R() -> {product, #{recurse => {recur, R}}} end,

    S = fun S() -> {product, #{blup => {recur, S}}} end,
    Expected = {product, #{only_in_old => [recurse], only_in_new => [blup], diff => []}},
    [?_assertEqual(none, domain:diff(R(), {recur, R})),
     ?_assertEqual(Expected, domain:diff(R(), {recur, S}))].

diff_sum_recur_diff_test_() ->
    Old = fun O() -> {sum, #{{recur, O} => true}} end,
    New = fun N() -> {sum, #{{recur, N} => true}} end,
    ?_assertEqual(none, domain:diff(Old(), New())).
