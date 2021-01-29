-module(symbol).
-export([id/1, tag/1, name/1, is/1, ctx/1]).

id(Path) when is_list(Path) -> 
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString, "_", get_random_string(6)]));
id(Symbol) -> id([Symbol]).

atom_id(Symbol) -> atom_id([Symbol]);
atom_id(Path) -> list_to_atom(id(Path)).

tag(Symbols) when is_list(Symbols) ->
    list_to_atom(lists:flatten([atom_to_list(A) || A <- lists:join('/', Symbols)]));
tag({type, _, Symbols}) -> tag(Symbols);
tag({type, _, _, Symbols}) -> tag(Symbols);
tag({type_def, _, Name, _}) -> Name;
tag({def, _, Name, _}) -> Name;
tag({recursive_type, _, _, Symbols}) -> tag(Symbols);
tag({tagged, _, Symbols, _}) -> tag(Symbols);
tag({symbol, _, _, S}) -> S;
tag({variable, _, _, Tag}) -> Tag;
tag({key, _, K}) -> K;
tag(Term) -> list_to_atom("expr_" ++ integer_to_list(erlang:phash2(Term))).

ctx(Term) -> element(2, Term).

name({pair, _, K, _}) -> name(K);
name({dict_pair, _, K, _}) -> name(K);
name({key, _, Key}) -> Key;
name({symbol, _, _, S}) -> S;
name({qualified_symbol, _, _, S}) -> S;
name({qualified_symbol, _, S}) -> S;
name({variable, _, Key, _}) -> Key;
name({type, _, Key}) -> Key;
name({type, _, Key, _}) -> Key.

is({symbol, _, _, _})           -> true;
is({variable, _, _, _})         -> true;
is({type, _, _, _})             -> true;
is({recursive_type, _, _, _})   -> true;
is({qualified_symbol, _, _, _}) -> true;
is({qualified_symbol, _, _})    -> true;
is(_)                           -> false.

get_random_string(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyz1234567890",
    F = fun(_, Acc) -> 
        Char = lists:nth(rand:uniform(length(AllowedChars)), AllowedChars),
        [Char] ++ Acc
    end,
    lists:foldl(F, [], lists:seq(1, Length)).
