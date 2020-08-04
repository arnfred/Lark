-module(error).

-export([collect/1, map/2, map2/3, flatmap/2, flatmap2/3, leftbias/2, format/2, format/1]).

-include_lib("eunit/include/eunit.hrl").

format(Type, Context) -> {error, [{Type, Context}]}.
format(Type) -> {error, [{Type, no_context}]}.

map({ok, E}, F) -> {ok, F(E)};
map({error, E}, _) -> {error, E};
map(D, F) -> map({ok, D}, F).

flatmap({ok, Elem}, F) -> F(Elem);
flatmap({error, Elem}, _) -> {error, Elem};
flatmap(D, F) -> flatmap({ok, D}, F).

map2({ok, E1}, {ok, E2}, F) -> {ok, F(E1, E2)};
map2({error, E1}, {error, E2}, _) -> {error, E1 ++ E2};
map2({error, E1}, _, _) -> {error, E1};
map2(_, {error, E2}, _) -> {error, E2};
map2(D1, D2, F) -> map2({ok, D1}, {ok, D2}, F).

flatmap2({ok, E1}, {ok, E2}, F) -> F(E1, E2);
flatmap2({error, E1}, {error, E2}, _) -> {error, E1 ++ E2};
flatmap2({error, E1}, _, _) -> {error, E1};
flatmap2(_, {error, E2}, _) -> {error, E2};
flatmap2(D1, D2, F) -> flatmap2({ok, D1}, {ok, D2}, F).

leftbias({error, _} = E, _) -> E;
leftbias(_, E) -> E.

collect(List) when is_list(List) -> collect_ok(List, []).

collect_ok([], Ret) -> {ok, lists:reverse(Ret)};
collect_ok([{ok, Head} | Tail], Ret) -> collect_ok(Tail, [Head | Ret]);
collect_ok([{error, Err} | Tail], _) -> collect_error(Tail, [Err]);
collect_ok([Head | Tail], Ret) -> collect_ok(Tail, [Head | Ret]).

collect_error([], Ret) -> {error, utils:unique(lists:reverse(lists:flatten(Ret)))};
collect_error([{error, Err} | Tail], Ret) -> collect_error(Tail, [Err | Ret]);
collect_error([{ok, _} | Tail], Ret) -> collect_error(Tail, Ret);
collect_error([_ | Tail], Ret) -> collect_error(Tail, Ret).


-ifdef(TEST).

collect_all_ok_test() ->
    Input = [{ok, 1}, {ok, 2}],
    Expected = {ok, [1, 2]},
    Actual = collect(Input),
    ?assertEqual(Expected, Actual).

collect_all_err_test() ->
    Input = [{error, [1]}, {error, [2]}],
    Expected = {error, [1, 2]},
    Actual = collect(Input),
    ?assertEqual(Expected, Actual).

collect_some_err_test() ->
    Input = [{ok, 1}, {error, [2]}, {ok, 3}, {error, [4]}],
    Expected = {error, [2, 4]},
    Actual = collect(Input),
    ?assertEqual(Expected, Actual).

map_ok_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({ok, 11}, map({ok, 1}, F)).

map_error_elem_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({error, [1]}, map({error, [1]}, F)).

map_domain_test() ->
    F = fun(N) -> N+10 end,
    ?assertEqual({ok, 11}, map(1, F)).

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
    ?assertEqual({ok, 11}, map2(1, 10, F)).

-endif.
