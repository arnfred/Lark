-module(intersection).
-export([intersection/2]).

intersection(D, D) -> D;

intersection({recur, S}, {recur, T}) -> {recur, fun() -> intersection(S(), T()) end};
intersection({recur, S}, {sum, _} = D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, S}, {tagged, _, _} = D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, S}, D) when is_map(D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, _}, _) -> none;
intersection(D, {recur, S}) -> intersection({recur, S}, D);

intersection({error, E1}, {error, E2}) -> {error, E1 ++ E2};
intersection({error, _} = E, _) -> E;
intersection(_, {error, _} = E) -> E;

intersection(any, D) -> D;
intersection(D, any) -> D;
intersection('Domain/Any', D) -> D;
intersection(D, 'Domain/Any') -> D;
intersection(none, _) -> none;
intersection(_, none) -> none;

intersection(F1, F2) when is_function(F1), is_function(F2) -> 
    case {utils:get_arity(F1), utils:get_arity(F2)} of
        {N, N} -> utils:mapfun(fun(Res1, Res2) -> domain:intersection(Res1, Res2) end, F1, F2);
        _ -> none
    end;

intersection({sum, D1}, {sum, D2}) -> 
    {sum, ordsets:from_list([intersection(Dj, Di) || Di <- D1, Dj <- D2])}; 
intersection({sum, D1}, D) -> 
    {sum, ordsets:from_list([intersection(D, Di) || Di <- D1])};
intersection(D, {sum, D1}) -> intersection({sum, D1}, D);

intersection(L1, L2) when is_list(L1) andalso is_list(L2) andalso length(L1) =:= length(L2) ->
    propagate_none([intersection(E1, E2) || {E1, E2} <- lists:zip(L1, L2)]);

intersection({tagged, Tag, D1}, {tagged, Tag, D2}) -> 
    propagate_none({tagged, Tag, intersection(D1, D2)});
intersection(D1, D2) when is_map(D1), is_map(D2) -> propagate_none(intersect_map(D1, D2));

intersection(_, _) -> none.

propagate_none(Map) when is_map(Map) -> 
    case lists:member(none, maps:values(Map)) of
        true -> none;
        false -> Map
    end;
propagate_none(List) when is_list(List) -> 
    case lists:member(none, List) of
        true -> none;
        false -> List
    end;
propagate_none({tagged, T, Map}) when is_map(Map) -> 
    case lists:member(none, maps:values(Map)) of
        true -> none;
        false -> {tagged, T, Map}
    end;
propagate_none(D) -> D.

intersect_map(D1, D2) when is_map(D1), is_map(D2) ->
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
    maps:map(F, maps:merge(D1, D2)).
