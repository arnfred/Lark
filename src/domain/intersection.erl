-module(intersection).
-export([intersection/2]).

intersection(D, D) -> D;

intersection({recur, S}, {recur, T}) -> {recur, fun() -> intersection(S(), T()) end};
intersection({recur, S}, {sum, _} = D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, S}, {tagged, _, _} = D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, S}, D) when is_map(D) -> {recur, fun() -> intersection(S(), D) end};
intersection({recur, _}, _) -> none;
intersection(D, {recur, S}) -> intersection({recur, S}, D);

intersection(any, D) -> D;
intersection(D, any) -> D;
intersection(none, _) -> none;
intersection(_, none) -> none;
intersection(whatever, D) -> D;
intersection(D, whatever) -> D;

% TODO: A function can be considered a value like `5` or 'atom'. While a
% function can also be seen as a constructor which given some inputs returns a
% domain, it isn't meaningful to consider the intersection of two such
% constructors, unless they are identical. While we could compute a new
% function which returns the intersection of domains of F1 and F2, this
% function would not correspond to any real function value.
intersection(F1, F2) when is_function(F1), is_function(F2) ->
    case utils:gen_tag(F1) =:= utils:gen_tag(F2) of
        true    -> F1;
        false   -> none
    end;

intersection({sum, D1}, {sum, D2}) ->
    {sum, ordsets:from_list([Elem || Di <- D1,
                                     Dj <- D2,
                                     Elem <- [intersection(Di, Dj)],
                                     not(Elem =:= none)])};
intersection({sum, D1}, D) ->
    {sum, ordsets:from_list([Elem || Di <- D1,
                                     Elem <- [intersection(D, Di)],
                                     not(Elem =:= none)])};
intersection(D, {sum, D1}) -> intersection({sum, D1}, D);

% For two lists where one is a prefix of the other, the intersection is the
% shorter list. For example, the intersection of `[1, 2]` and `[1, 2, 3]` would
% be `[1, 2]`
intersection(L1, L2) when is_list(L1) andalso is_list(L2) ->
    Length = min(length(L1), length(L2)),
    LL1 = lists:sublist(L1, Length),
    LL2 = lists:sublist(L2, Length),
    propagate_none([intersection(E1, E2) || {E1, E2} <- lists:zip(LL1, LL2)]);

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
propagate_none({tagged, _, D} = Term) ->
    case propagate_none(D) of
        none    -> none;
        _       -> Term
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
