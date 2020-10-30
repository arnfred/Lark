-module(union_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

union_array_test_() ->
    D = blip,
    Expected = blip,
    Actual = domain:union([D]),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_product_one_key_test_() ->
    D1 = #{blip => true},
    D2 = #{blip => false},
    Expected = #{blip => {sum, ordsets:from_list([true, false])}},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_product_two_keys_mergeable_test_() ->
    D1 = #{blap => a, blip => true},
    D2 = #{blap => a, blip => false},
    Expected = #{blap => a, blip => {sum, ordsets:from_list([true, false])}},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_product_two_keys_non_mergeable_test_() ->
    D1 = #{blap => a, blip => true},
    D2 = #{blap => b, blip => false},
    Expected = {sum, ordsets:from_list([#{blap => a, blip => true},
                                     #{blap => b, blip => false}])},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_product_uneven_keys_mergeable_test_() ->
    D1 = #{blap => a, blip => true},
    D2 = #{blip => true},
    Expected = #{blap => any, blip => true},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_product_many_keys_test_() ->
    D1 = #{blip => true, blap => false, blup => extra_old},
    D2 = #{blip => true, blap => true, blep => extra_new},
    Expected = {sum, ordsets:from_list([#{blip => true, blap => false, blup => extra_old},
                                     #{blip => true, blap => true, blep => extra_new}])},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_product_same_keys_test_() ->
    D1 = #{blip => true, blap => false},
    D2 = #{blip => true, blap => true},
    Expected = #{blip => true, blap => {sum, ordsets:from_list([false, true])}},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_sum_test_() ->
    D1 = {sum, ordsets:from_list([a,b,c])},
    D2 = {sum, ordsets:from_list([b,c,d])},
    Expected = {sum, ordsets:from_list([a,b,c,d])},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_sum_none_test_() ->
    D1 = {sum, ordsets:from_list([none])},
    D2 = {sum, ordsets:from_list([a,b])},
    Expected = {sum, ordsets:from_list([a,b])},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_tagged_same_test_() ->
    D1 = {tagged, kukkeluk, #{blip => true}},
    D2 = {tagged, kukkeluk, #{blip => false}},
    Expected = {tagged, kukkeluk, #{blip => {sum, ordsets:from_list([true, false])}}},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_tagged_diff_test_() ->
    D1 = {tagged, kukkeluk, #{blip => true, blap => false, blup => extra_old}},
    D2 = {tagged, kakkelak, #{blip => true, blap => true, blep => extra_new}},
    Expected = {sum, ordsets:from_list([{tagged, kukkeluk, #{blip => true, blap => false, blup => extra_old}},
                                     {tagged, kakkelak, #{blip => true, blap => true, blep => extra_new}}])},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_sum_of_products_test_() ->
    D1 = {sum, ordsets:from_list([#{a => 1, b => 2}])},
    D2 = {sum, ordsets:from_list([#{a => 1, c => 3}])},
    Expected = {sum, ordsets:from_list([#{a => 1, b => 2}, #{a => 1, c => 3}])},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_sum_of_many_products_test_() ->
    D1 = {sum, ordsets:from_list([#{a => 1, b => 2, c => 1},
                                  #{a => 2, b => 2, c => 2},
                                  #{a => 1, b => 2, c => 3},
                                  #{a => 2, b => 2, c => 4}])},
    D2 = {sum, ordsets:from_list([#{a => 1}])},
    Expected = {sum, ordsets:from_list([#{a => 1, b => 2, c => {sum, ordsets:from_list([1, 3])}}, 
                                        #{a => 2, b => 2, c => {sum, ordsets:from_list([2, 4])}},
                                        #{a => 1}])},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_sum_with_none_test_() ->
    D1 = {sum, ordsets:from_list([#{a => 1, b => 2, c => 1}, none])},
    D2 = {sum, ordsets:from_list([none])},
    Expected = #{a => 1, b => 2, c => 1}, 
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_sum_with_any_test_() ->
    D1 = {sum, ordsets:from_list([#{a => 1, b => 2, c => 1}, any])},
    D2 = {sum, ordsets:from_list([none])},
    Expected = any,
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_sum_of_products_with_same_keys_test_() ->
    D1 = {sum, ordsets:from_list([#{a => 1, b => 2}])},
    D2 = {sum, ordsets:from_list([#{a => 1, b => 3}])},
    Expected = #{a => 1, b => {sum, ordsets:from_list([2,3])}},
    Actual = domain:union(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_recur_recur_sum_sum_test_() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = domain:union({recur, R}, {recur, S}),
    Expected = fun E() -> {sum, ordsets:from_list([a, b, c, {recur, E}])} end,
    [?_assertEqual(none, domain:diff(Expected(), Actual)),
     ?_assertMatch({sum, [a, b, c, {recur, _}]}, Actual)].

union_recur_constant_test_() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    Actual = domain:union({recur, R}, blip),
    Expected = blip,
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_recur_recur_sum_sum_sum_test_() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Result = domain:union({recur, R}, {recur, S}),
    T = fun T() -> {sum, ordsets:from_list([c, d, {recur, T}])} end,
    Actual = domain:union(Result, T()),
    Expected = fun E() -> {sum, ordsets:from_list([a, b, c, d, {recur, E}])} end,
    [?_assertEqual(none, domain:diff(Expected(), Actual)),
     ?_assertMatch({sum, [a, b, c, d, {recur, _}]}, Actual)].

union_merge_recur_products_test_() ->
    P1 = fun P1() -> #{c => 2, b => {recur, P1}} end,
    P2 = fun P2() -> #{c => 2, b => {recur, P2}} end,
    Actual = domain:union(P1(), P2()),
    Expected = #{c => 2, b => {sum, ordsets:from_list([{recur, P1}, {recur, P2}])}},
    ?_assertEqual(none, domain:diff(Expected, Actual)).

union_merge_recur_products_with_single_key_test_() ->
    P1 = fun P1() -> #{r => {recur, P1}} end,
    P2 = fun P2() -> #{r => {recur, P2}} end,
    Actual = domain:union(P1(), P2()),
    Expected = #{r => {sum, ordsets:from_list([{recur, P1}, {recur, P2}])}},
    ?_assertEqual(none, domain:diff(Expected, Actual)).

list_equal_length_test_() ->
    L1 = [a, b],
    L2 = [1, 2],
    Expected = [domain:union(a, 1), domain:union(b, 2)],
    ?testEqual(Expected, domain:union(L1, L2)).

list_unqeual_length_test_() ->
    L1 = [a, b],
    L2 = [1, 2, 3],
    Expected = {sum, ordsets:from_list([L1, L2])},
    ?testEqual(Expected, domain:union(L1, L2)).

