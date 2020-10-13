-module(intersection_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

intersection_array_test_() ->
    D = blip,
    Expected = blip,
    Actual = domain:intersection([D]),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_product_one_key_test_() ->
    D1 = {product, #{blip => true}},
    D2 = {product, #{blip => false}},
    Expected = none,
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_product_two_keys_mergeable_test_() ->
    D1 = {product, #{blap => a, blip => {sum, ordsets:from_list([false,true])}}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = {product, #{blap => a, blip => false}},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_product_two_keys_non_mergeable_test_() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = none,
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_product_subset_test_() ->
    D1 = {product, #{blap => a, blip => true}},
    D2 = {product, #{blip => true}},
    Expected = {product, #{blap => a, blip => true}},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_product_non_subset_test_() ->
    D1 = {product, #{blap => a, blup => true}},
    D2 = {product, #{blap => a, blip => false}},
    Expected = {product, #{blap => a, blip => false, blup => true}},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_sum_test_() ->
    D1 = {sum, ordsets:from_list([a,b,c])},
    D2 = {sum, ordsets:from_list([b,c,d])},
    Expected = {sum, ordsets:from_list([b,c])},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_sum_any_test_() ->
    D1 = {sum, ordsets:from_list([any])},
    D2 = {sum, ordsets:from_list([a,b])},
    Expected = {sum, ordsets:from_list([a,b])},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_tagged_same_test_() ->
    D1 = {tagged, kukkeluk, {product, #{blip => {sum, ordsets:from_list([false, true])}}}},
    D2 = {tagged, kukkeluk, {product, #{blip => false}}},
    Expected = {tagged, kukkeluk, {product, #{blip => false}}},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_tagged_diff_test_() ->
    D1 = {tagged, kukkeluk, {product, #{blip => true, blap => false, blup => extra_old}}},
    D2 = {tagged, kakkelak, {product, #{blip => true, blap => true, blep => extra_new}}},
    Expected = none,
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_sum_of_products_test_() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2}}])},
    D2 = {sum, ordsets:from_list([{product, #{a => 1, b => {sum, ordsets:from_list([2,3])}}}])},
    Expected = {product, #{a => 1, b => 2}},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_sum_with_none_test_() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => 1}}, none])},
    D2 = {sum, ordsets:from_list([none])},
    Expected = none,
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_sum_with_any_test_() ->
    D1 = {sum, ordsets:from_list([{product, #{a => 1, b => 2, c => 1}}])},
    D2 = {sum, ordsets:from_list([any])},
    Expected = {product, #{a => 1, b => 2, c => 1}},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_f_test_() ->
    D1 = {f, d1, fun(A) -> A end},
    D2 = {f, d2, fun(A) -> {sum, ordsets:from_list([A, b])} end},
    {f, d1_d2, DomainFun} = domain:intersection(D1, D2),
    Actual = DomainFun(a),
    Expected = a,
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_map_map_test_() ->
    D1 = #{a => any, b => any},
    D2 = #{b => blup, c => none},
    Expected = #{a => any, b => blup, c => none},
    Actual = domain:intersection(D1, D2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_recur_val_test_() ->
    R = fun R() -> {sum, ordsets:from_list([{product, #{recurse => {recur, R}}}])} end,
    Input = {recur, R},
    ?_assertEqual(none, domain:intersection(Input, val)).

intersection_sum_sum_recur_recur_test_() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = domain:intersection(R(), S()),
    Expected = fun E() -> {sum, ordsets:from_list([b, {recur, E}])} end,
    [?_assertEqual(none, domain:diff(Expected(), Actual)),
     ?_assertMatch({sum, [b, {recur, _}]}, Actual)].

intersection_sum_recur_recur_sum_test_() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = domain:intersection(R(), {recur, S}),
    Expected = fun E() -> {sum, ordsets:from_list([b, {recur, E}])} end,
    [?_assertEqual(none, domain:diff(Expected(), Actual)),
     ?_assertMatch({sum, [b, {recur, _}]}, Actual)].

intersection_recur_recur_sum_sum_test_() ->
    R = fun R() -> {sum, ordsets:from_list([a, b, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([b, c, {recur, S}])} end,
    Actual = domain:intersection({recur, R}, {recur, S}),
    Expected = fun E() -> {sum, ordsets:from_list([b, {recur, E}])} end,
    ?_assertEqual(none, domain:diff(Expected(), Actual)).

intersection_recur_recur_sum_sum_product_test_() ->
    P1 = {product, #{a => 1, b => {sum, ordsets:from_list([2,3])}}},
    P2 = {product, #{a => 1, b => {sum, ordsets:from_list([3,4])}}},
    R = fun R() -> {sum, ordsets:from_list([P1, {recur, R}])} end,
    S = fun S() -> {sum, ordsets:from_list([P2, {recur, S}])} end,
    Actual = domain:intersection({recur, R}, {recur, S}),
    E = fun E() -> {sum, ordsets:from_list([{product, #{a => 1, b => 3}}, {recur, E}])} end,
    Expected = E(),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

intersection_infinite_recur_test_() ->
    Inf1 = fun I() -> {recur, I} end,
    Actual = domain:compact(Inf1()),
    Expected = {error, [{{possibly_infinite_recursion}, {domain}}]},
    ?_assertEqual(Expected, Actual).

list_equal_length_test_() ->
    L1 = [{sum, ordsets:from_list([a, b])}, {sum, ordsets:from_list([1, 2])}],
    L2 = [{sum, ordsets:from_list([b, c])}, {sum, ordsets:from_list([2, 3])}],
    Expected = [b, 2],
    ?testEqual(Expected, domain:intersection(L1, L2)).

list_unqeual_length_test_() ->
    L1 = [a, b],
    L2 = [1, 2, 3],
    Expected = none,
    ?testEqual(Expected, domain:intersection(L1, L2)).
