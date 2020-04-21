-module(symbol).
-export([id/1]).

id(Path) -> 
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString, "_", get_random_string(6)])).

get_random_string(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyz1234567890",
    F = fun(_, Acc) -> 
        Char = lists:nth(crypto:rand_uniform(1, length(AllowedChars)), AllowedChars),
        [Char] ++ Acc
    end,
    lists:foldl(F, [], lists:seq(1, Length)).
