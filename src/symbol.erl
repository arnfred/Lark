-module(symbol).
-export([id/1, tag/1, name/1, is/1, ctx/1, rename/2]).

id(Path) when is_list(Path) -> 
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString, "_", get_random_string(6)]));
id(Symbol) -> id([Symbol]).

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
tag({qualified_symbol, _, Path, S}) -> tag(Path ++ [S]);
tag(Term) -> list_to_atom("expr_" ++ integer_to_list(erlang:phash2(Term))).

ctx(Term) -> element(2, Term).

name({pair, _, K, _}) -> name(K);
name({dict_pair, _, K, _}) -> name(K);
name({key, _, Key}) -> Key;
name({symbol, _, _, S}) -> S;
name({link, _, Term}) -> name(Term);
name({qualified_symbol, _, _, S}) -> S;
name({qualified_symbol, _, S}) -> S;
name({tagged, _, Symbols, _}) -> lists:last(Symbols);
name({variable, _, Key, _}) -> Key;
name({type, _, Key}) -> Key;
name({type, _, Key, _}) -> Key;
name({type_def, _, Name, _}) -> Name.

rename({pair, Ctx, K, V}, Name) -> {pair, Ctx, rename(K, Name), V};
rename({dict_pair, Ctx, K, V}, Name) -> {dict_pair, Ctx, rename(K, Name), V};
rename({key, Ctx, _}, Name) -> {key, Ctx, Name};
rename({symbol, Ctx, Path, _}, Name) -> {symbol, Ctx, Path, Name};
rename({link, Ctx, Term}, Name) -> {link, Ctx, rename(Term, Name)};
rename({qualified_symbol, Ctx, Path, _}, Name) -> {qualified_symbol, Ctx, Path, Name};
rename({qualified_symbol, Ctx, _}, Name) -> {qualified_symbol, Ctx, Name};
rename({tagged, Ctx, Symbols, Expr}, Name) -> {tagged, Ctx, lists:droplast(Symbols) ++ [Name], Expr};
rename({variable, Ctx, _, Tag}, Name) -> {variable, Ctx, Name, Tag};
rename({type, Ctx, _}, Name) -> {type, Ctx, Name};
rename({type, Ctx, _, Tag}, Name) -> {type, Ctx, Name, Tag};
rename({type_def, Ctx, _, Expr}, Name) -> {type_def, Ctx, Name, Expr}.


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
