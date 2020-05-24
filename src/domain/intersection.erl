-module(intersection).
-export([intersection/2]).

intersection({recur, S}, {recur, T}) -> {recur, fun() -> intersection(S(), T()) end};
intersection({recur, S}, {sum, _} = D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, S}, {product, _} = D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, S}, {tagged, _, _} = D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, _}, _) -> none;
intersection(D, {recur, S}) -> intersection({recur, S}, D);

intersection(D1, D2) when is_map(D1), is_map(D2) ->
    % When intersecting two maps we include all domains of the two maps.  This
    % is because a key is assumed to have domain `any` when it is not present
    % in a map and any narrower definition would need to be captured in the
    % intersection
    F = fun(K, _) -> case {maps:is_key(K, D1), maps:is_key(K, D2)} of
                         {true, true} -> intersection(maps:get(K, D1), maps:get(K, D2));
                         {false, true} -> maps:get(K, D2);
                         {true, false} -> maps:get(K, D1)
                     end
        end,
    maps:map(F, maps:merge(D1, D2));

intersection({error, E1}, {error, E2}) -> {error, E1 ++ E2};
intersection({error, _} = E, _) -> E;
intersection(_, {error, _} = E) -> E;

intersection(any, D) -> D;
intersection(D, any) -> D;
intersection(none, _) -> none;
intersection(_, none) -> none;

intersection({f, F1}, {f, F2}) -> 
    case {get_arity(F1), get_arity(F2)} of
        {N, N} -> {f, mapfun(fun(Res1, Res2) -> intersection(Res1, Res2) end, F1, F2)};
        _ -> none
    end;

intersection({sum, D1}, {sum, D2}) -> 
    {sum, ordsets:from_list([intersection(Dj, Di) || Di <- D1, Dj <- D2])}; 
intersection({sum, D1}, D) -> 
    {sum, ordsets:from_list([intersection(D, Di) || Di <- D1])};
intersection(D, {sum, D1}) -> intersection({sum, D1}, D);

intersection({tagged, Tag, D1}, {tagged, Tag, D2}) -> 
    propagate_none({tagged, Tag, intersection(D1, D2)});
intersection({product, D1}, {product, D2}) -> propagate_none({product, intersection(D1, D2)});

intersection(D, D) -> D;
intersection(_, _) -> none.

propagate_none({product, Map}) -> 
    case lists:member(none, maps:values(Map)) of
        true -> none;
        false -> {product, Map}
    end;
propagate_none({tagged, T, {product, Map}}) -> 
    case lists:member(none, maps:values(Map)) of
        true -> none;
        false -> {tagged, T, {product, Map}}
    end;
propagate_none(D) -> D.

get_arity(Fun) ->
    proplists:get_value(arity, erlang:fun_info(Fun)).

mapfun(Mapper, Fun1, Fun2) -> 
    case {get_arity(Fun1), get_arity(Fun2)} of
        {0, 0}   -> Mapper(Fun1(), Fun2());
        {1, 1}   -> fun(Arg) -> Mapper(Fun1(Arg), Fun2(Arg)) end;
        {2, 2}   -> fun(A1, A2) -> Mapper(Fun1(A1, A2), Fun2(A1, A2)) end;
        {3, 3}   -> fun(A1, A2, A3) -> Mapper(Fun1(A1, A2, A3), Fun2(A1, A2, A3)) end;
        {4, 4}   -> fun(A1, A2, A3, A4) -> Mapper(Fun1(A1, A2, A3, A4), Fun2(A1, A2, A3, A4)) end;
        {5, 5}   -> fun(A1, A2, A3, A4, A5) -> Mapper(Fun1(A1, A2, A3, A4, A5), Fun2(A1, A2, A3, A4, A5)) end;
        {6, 6}   -> fun(A1, A2, A3, A4, A5, A6) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6), Fun2(A1, A2, A3, A4, A5, A6)) end;
        {7, 7}   -> fun(A1, A2, A3, A4, A5, A6, A7) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7), Fun2(A1, A2, A3, A4, A5, A6, A7)) end;
        {8, 8}   -> fun(A1, A2, A3, A4, A5, A6, A7, A8) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8), Fun2(A1, A2, A3, A4, A5, A6, A7, A8)) end;
        {9, 9}   -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9)) end;
        {10, 10} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) end;
        {11, 11} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) end;
        {12, 12} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) end;
        {13, 13} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) end
    end.

