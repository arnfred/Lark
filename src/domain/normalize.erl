-module(normalize).
-export([normalize/1]).
-include_lib("eunit/include/eunit.hrl").

normalize({sum, S}) ->
    list_to_sum([ExEl || Elem <- S,
                         ExEl <- expand_sum(normalize(Elem)),
                         not(ExEl =:= none)]);

normalize(Map) when is_map(Map) ->
    Entries = [[{K, ExEl} || ExEl <- expand_sum(normalize(V))] || {K, V} <- maps:to_list(Map)],
    list_to_sum([maps:from_list(Set) || Set <- combinations(Entries)]);

normalize({tagged, Tag, Domain}) ->
    case normalize(Domain) of
        {sum, Elems}    -> list_to_sum([{tagged, Tag, E} || E <- Elems]);
        D               -> {tagged, Tag, D}
    end;

normalize(L) when is_list(L) ->
    Elems = [expand_sum(normalize(E)) || E <- L],
    list_to_sum([Set || Set <- combinations(Elems)]);

normalize({recur, D}) -> {recur, fun() -> normalize(D()) end};
normalize(T) -> T.

list_to_sum(Elems) when is_list(Elems) ->
    NoNones = [E || E <- Elems, not(E =:= none)],
    case lists:member(any, NoNones) of
        true    -> any;
        false   -> case NoNones of
                       []   -> none;
                       [E]  -> E;
                       _    -> {sum, ordsets:from_list(Elems)}
                   end
    end;
list_to_sum(Domain) -> Domain.

combinations([Elements | Rest]) -> [[E | Tail] || E <- Elements,
                                                  Tail <- combinations(Rest)];
combinations([])                -> [[]].

expand_sum({sum, Elems}) -> Elems;
expand_sum(Domain) -> [Domain].
