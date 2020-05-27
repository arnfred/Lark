-module(error).

-export([flatten/1, map/2]).

-include_lib("eunit/include/eunit.hrl").

map({ok, E}, F) -> {ok, F(E)};
map({error, E}, _) -> {error, E};
map(D, F) -> F(D).

map2({ok, E1}, {ok, E2}, F) -> {ok, F(E1, E2)};
map2({error, E1}, {error, E2}, _) -> {error, E1 ++ E2};
map2({error, E1}, _, _) -> {error, E1};
map2(_, {error, E2}, _) -> {error, E2};
map2(D1, D2, F) -> F(D1, D2).

flatten(List) when is_list(List) -> flatten_ok(List, []).

flatten_ok([], Ret) -> {ok, lists:reverse(Ret)};
flatten_ok([{ok, Head} | Tail], Ret) -> flatten_ok(Tail, [Head | Ret]);
flatten_ok([{error, Err} | Tail], _) -> flatten_error(Tail, [Err]).

flatten_error([], Ret) -> {error, sets:to_list(sets:from_list(lists:reverse(lists:flatten(Ret))))};
flatten_error([{error, Err} | Tail], Ret) -> flatten_error(Tail, [Err | Ret]);
flatten_error([{ok, _} | Tail], Ret) -> flatten_error(Tail, Ret).

-ifdef(TEST).

flatten_all_ok_test() ->
    Input = [{ok, 1}, {ok, 2}],
    Expected = {ok, [1, 2]},
    Actual = flatten(Input),
    ?assertEqual(Expected, Actual).

flatten_all_err_test() ->
    Input = [{error, [1]}, {error, [2]}],
    Expected = {error, [2, 1]},
    Actual = flatten(Input),
    ?assertEqual(Expected, Actual).

flatten_some_err_test() ->
    Input = [{ok, 1}, {error, [2]}, {ok, 3}, {error, [4]}],
    Expected = {error, [2, 4]},
    Actual = flatten(Input),
    ?assertEqual(Expected, Actual).

map_ok_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({ok, 11}, map({ok, 1}, F)).

map_error_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({error, [1]}, map({error, [1]}, F)).

map_domain_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual(11, map(1, F)).

map2_ok_test() ->
    F = fun(N, M) -> N + M end,
    ?assertEqual({ok, 11}, map2({ok, 1}, {ok, 10}, F)).

map2_error_elem_test() ->
    F = fun(N, M) -> N+M end,
    ?assertEqual({error, [1]}, map2({error, [1]}, {ok, 1}, F)).

map2_error_both_test() ->
    F = fun(N, M) -> N+M end,
    ?assertEqual({error, [1,2]}, map2({error, [1]}, {error, [2]}, F)).

map2_domain_test() ->
    F = fun(N, M) -> N+M end,
    ?assertEqual(11, map2(1, 10, F)).

-endif.
