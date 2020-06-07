-module(compact_test).
-include_lib("eunit/include/eunit.hrl").

compact_sum_sum_test_() ->
    Actual = domain:compact({sum, #{a => true,
                                    {sum, #{b => true,
                                            c => true}} => true}}),
    Expected = {sum, maps:from_list([{E, true} || E <- [a, b, c]])},
    ?_assertEqual(Expected, Actual).

compact_product_sum_sum_test_() ->
    Expected = {product, #{a => {sum, maps:from_list([{E, true} || E <- [a, b, c]])}}},
    Actual = domain:compact({product, #{a => {sum, maps:from_list([{E, true} || E <- [a,{sum, maps:from_list([{E, true} || E <- [b,c]])}]])}}}),
    ?_assertEqual(Expected, Actual).

compact_tagged_sum_sum_test_() ->
    Expected = {tagged, t, {sum, maps:from_list([{E, true} || E <- [a, b, c]])}},
    Actual = domain:compact({tagged, t, {sum, maps:from_list([{E, true} || E <- [a,{sum, maps:from_list([{E, true} || E <- [b,c]])}]])}}),
    ?_assertEqual(Expected, Actual).

compact_sum_tagged_sum_sum_test_() ->
    Expected = {tagged, t, {sum, maps:from_list([{E, true} || E <- [a, b, c]])}},
    Actual = domain:compact({sum, maps:from_list([{E, true} || E <- [{tagged, t, {sum, maps:from_list([{E, true} || E <- [a,{sum, maps:from_list([{E, true} || E <- [b,c]])}]])}}]])}),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_sum_item_test_() ->
    Expected = {sum, maps:from_list([{E, true} || E <- [a, b, c]])},
    Actual = domain:compact({sum, maps:from_list([{E, true} || E <- [a, b, {sum, maps:from_list([{E, true} || E <- [c]])}]])}),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_f_test_() ->
    Input = {f, name, fun(A1, A2) -> {sum, maps:from_list([{E, true} || E <- [{sum, maps:from_list([{E, true} || E <- [A1, A2]])}]])} end},
    Expected = {sum, maps:from_list([{E, true} || E <- [a1, a2]])},
    {f, name, DomainFun} = domain:compact(Input),
    Actual = DomainFun(a1, a2),
    ?_assertEqual(none, domain:diff(Expected, Actual)).

compact_recur_test_() ->
    R = fun R() -> {sum, maps:from_list([{E, true} || E <- [a, {product, #{recurse => {recur, R}}}]])} end,
    Input = {recur, R},
    Expected = {recur, R},
    ?_assertEqual(none, domain:diff(Expected, domain:compact(Input))).

compact_recur_flatten_test_() ->
    % Sum with one item will be compacted: _____________________________________
    R = fun R() -> {sum, maps:from_list([{E, true} || E <- [{product, #{recurse => {recur, R}}}]])} end,
    Input = {recur, R},
    Actual = domain:compact(Input),
    {product, Map} = Actual,
    [?_assertMatch({product, _}, Actual),
     ?_assertMatch([{recurse, {recur, _}}], maps:to_list(Map))].

compact_list_test_() ->
    List = fun List() -> {sum, maps:from_list([{E, true} || E <- ['List/Nil',
                                                  {tagged, 'List/Cons', 
                                                   #{head => 'List/Nil',
                                                     tail => {recur, List}}}]])} end,
    Input = List(),
    Actual = domain:compact(Input),
    ?_assertEqual(none, domain:diff(Input, Actual)).
