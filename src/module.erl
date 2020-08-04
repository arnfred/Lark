-module(module).
-export([format/1, beam_name/1, kind_name/1]).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

format(Sources) ->
    case error:collect([prepare(File, Code) || {File, Code} <- Sources]) of
        {error, Errs}           -> {error, Errs};
        {ok, PreparedSources}   ->
            Modules = [{Name, Term, File} || {File, {ast, Modules, _, _}} <- PreparedSources,
                                                    {module, _, Name, _, _} = Term <- Modules],
            KeyF = fun({_, Name, _}) -> Name end,
            ErrF = fun({{Name, Term1, File1}, {Name, Term2, File2}}) ->
                           error:format({duplicate_module, Name, File1, File2}, {module, Term1, Term2}) end,
            case utils:duplicates(Modules, KeyF) of
                []      -> {ok, PreparedSources};
                Dups    -> error:collect([ErrF(D) || D <- Dups])
            end
    end.

prepare(File, Code) ->
    Modules = [M || M = {module, _, _, _} <- Code],
    Imports = [I || I = {import, _, _} <- Code],
    Defs = maps:from_list([{Name, T} || T = {Type, _, Name, _, _} <- Code, Type == type_def orelse Type == def]),
    case error:collect([handle_modules(M, Defs) || M <- Modules]) of
        {error, Errs}   -> {error, Errs};
        {ok, Mods}        -> {ok, {File, {ast, #{file => File}, Mods, Imports, Defs}}}
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
        {ok, _}         -> {ok, {module, Ctx, Name, Exports}}
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
            case utils:duplicates(Aliases, fun({_, _, Alias, _}) -> Alias end) of
                []          -> {ok, {Aliases, Rewrites, Dependencies}};
                Duplicates  -> error:collect([ErrFun(Dup) || Dup <- Duplicates])
            end
    end.



beam_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString])).

kind_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('/', Path)],
    list_to_atom(lists:flatten([PathString])).

-ifdef(TEST).


-endif.
