-module(symbol).
-export([id/1, tag/1, name/1, is/1]).

id(Path) when is_list(Path) -> 
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString, "_", get_random_string(6)]));
id(Symbol) -> id([Symbol]).

tag({type, _, Symbols}) -> 
    list_to_atom(lists:flatten([atom_to_list(A) || A <- lists:join('/', Symbols)]));
tag({type, _, _, Symbols}) -> 
    list_to_atom(lists:flatten([atom_to_list(A) || A <- lists:join('/', Symbols)]));
tag({tagged, _, Symbols, _}) -> 
    list_to_atom(lists:flatten([atom_to_list(A) || A <- lists:join('/', Symbols)]));
tag({symbol, _, _, S}) -> S;
tag({variable, _, _, Tag}) -> Tag;
tag({key, _, K}) -> K.

name({pair, _, K, _}) -> name(K);
name({key, _, Key}) -> Key;
name({symbol, _, _, S}) -> S;
name({qualified_type, _, S}) -> S;
name({qualified_variable, _, S}) -> S;
name({qualified_symbol, _, S}) -> S;
name({variable, _, Key, _}) -> Key;
name({type, _, Key}) -> Key.

is({symbol, _, _, _})           -> true;
is({variable, _, _, _})         -> true;
is({type, _, _, _})             -> true;
is({qualified_type, _, _})      -> true;
is({qualified_variable, _, _})  -> true;
is({qualified_symbol, _, _})    -> true;
is(_)                           -> false.

get_random_string(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyz1234567890",
    F = fun(_, Acc) -> 
        Char = lists:nth(rand:uniform(length(AllowedChars)), AllowedChars),
        [Char] ++ Acc
    end,
    lists:foldl(F, [], lists:seq(1, Length)).
