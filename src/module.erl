-module(module).
-export([prepare/2, beam_name/1, kind_name/1]).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

prepare(Statements, FileName) ->
    Modules = [M || M = {module, _, _, _} <- Statements],
    Imports = [I || I = {import, _, _} <- Statements],
    Defs = maps:from_list([{Name, T} || T = {Type, _, Name, _, _} <- Statements, Type == type_def orelse Type == def]),
    case error:collect([handle_modules(M, Defs) || M <- Modules]) of
        {error, Errs}   -> {error, Errs};
        {ok, Mods}        ->
            case handle_imports(Imports) of
                {error, Errs}                           -> {error, Errs};
                {ok, {Aliases, Rewrites, Dependencies}} -> 
                    NewImports = {imports, #{file => FileName}, Aliases, Rewrites, Dependencies},
                    {ok, {ast, #{file => FileName}, Mods, NewImports, Defs}}
            end
    end.

handle_modules({module, Ctx, Name, Exports}, Defs) ->
    F = fun({pair, _, K, _} = Elem) -> case maps:is_key(symbol:tag(K), Defs) of
                                           true  -> {ok, Elem};
                                           false -> error:format({export_missing, symbol:tag(K)}, {module, Elem})
                                       end;
           (Elem)                   -> case maps:is_key(symbol:tag(Elem), Defs) of
                                           true  -> {ok, Elem};
                                           false -> error:format({export_missing, symbol:tag(Elem)}, {module, Elem})
                                       end
        end,
    case error:collect([F(E) || E <- maps:keys(Exports)]) of
        {error, Errs}   -> {error, Errs};
        {ok, _}         -> {ok, {module, Ctx, beam_name(Name), kind_name(Name), Exports}}
    end.

handle_imports(ImportClauses) ->
    case error:collect([import:format(Imp, #{}) || Imp <- ImportClauses]) of
        {error, Errs} -> {error, Errs};
        {ok, All} ->
            Dependencies = lists:filter(fun({dependency, _, _}) -> true;
                                           (_) -> false end, All),
            Rewrites = lists:filter(fun({rewrite, _, _, _}) -> true;
                                       (_) -> false end, All),
            Aliases = lists:filter(fun({alias, _, _, _}) -> true;
                                      (_) -> false end, All),
            ErrFun = fun({alias, _, Alias, _} = Term) -> error:format({duplicate_import, Alias}, {import, Term}) end,
            case duplicates(Aliases, fun({_, _, Alias, _}) -> Alias end) of
                []          -> {ok, {Aliases, Rewrites, Dependencies}};
                Duplicates  -> error:collect([ErrFun(Dup) || Dup <- Duplicates])
            end
    end.

duplicates(Elements, GetKey) ->
    F = fun(Elem, {Duplicates, Seen}) -> 
                Key = GetKey(Elem),
                case maps:is_key(Key, Seen) of
                    true    -> {[Elem | Duplicates], Seen};
                    false   -> {Duplicates, maps:put(Key, Elem, Seen)}
                end end,
    {Duplicates, _} = lists:foldl(F, {[], #{}}, Elements),
    Duplicates.


beam_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString])).

kind_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('/', Path)],
    list_to_atom(lists:flatten([PathString])).

-ifdef(TEST).


-endif.
