-module(error).

-export([flatten/1, map/2]).

-include_lib("eunit/include/eunit.hrl").

map({ok, E}, F) -> {ok, F(E)};
map(Err, _) -> Err.

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
    Input = [{error, 1}, {error, 2}],
    Expected = {error, [2, 1]},
    Actual = flatten(Input),
    ?assertEqual(Expected, Actual).

flatten_some_err_test() ->
    Input = [{ok, 1}, {error, 2}, {ok, 3}, {error, 4}],
    Expected = {error, [2, 4]},
    Actual = flatten(Input),
    ?assertEqual(Expected, Actual).

map_ok_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({ok, 11}, map({ok, 1}, F)).

map_error_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({error, 1}, map({error, 1}, F)).
    
-endif.
