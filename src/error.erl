-module(error).

-export([collate/1, map/2]).

-include_lib("eunit/include/eunit.hrl").

map({ok, E}, F) -> {ok, F(E)};
map(Err, _) -> Err.

collate(List) when is_list(List) -> collate_ok(List, []).

collate_ok([], Ret) -> {ok, lists:reverse(Ret)};
collate_ok([{ok, Head} | Tail], Ret) -> collate_ok(Tail, [Head | Ret]);
collate_ok([{error, Err} | Tail], _) -> collate_error(Tail, [Err]).

collate_error([], Ret) -> {error, sets:to_list(sets:from_list(lists:reverse(lists:flatten(Ret))))};
collate_error([{error, Err} | Tail], Ret) -> collate_error(Tail, [Err | Ret]);
collate_error([{ok, _} | Tail], Ret) -> collate_error(Tail, Ret).

-ifdef(TEST).

collate_all_ok_test() ->
    Input = [{ok, 1}, {ok, 2}],
    Expected = {ok, [1, 2]},
    Actual = collate(Input),
    ?assertEqual(Expected, Actual).

collate_all_err_test() ->
    Input = [{error, 1}, {error, 2}],
    Expected = {error, [2, 1]},
    Actual = collate(Input),
    ?assertEqual(Expected, Actual).

collate_some_err_test() ->
    Input = [{ok, 1}, {error, 2}, {ok, 3}, {error, 4}],
    Expected = {error, [2, 4]},
    Actual = collate(Input),
    ?assertEqual(Expected, Actual).

map_ok_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({ok, 11}, map({ok, 1}, F)).

map_error_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({error, 1}, map({error, 1}, F)).
    
-endif.
