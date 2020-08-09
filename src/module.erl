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

handle_modules({module, Ctx, Path, Exports}, Defs) ->
    F = fun
            % blah: T
            ({pair, _, K, _} = Elem) ->
                case maps:is_key(symbol:tag(K), Defs) of
                    true  -> {ok, symbol:tag(K)};
                    false -> error:format({export_missing, symbol:tag(K)}, {module, Elem})
                end;
            % Boolean/True
            % When compiling types:
            % * Include all exported types in the module
            % * Include all exported subtypes in the module under their name. Maybe check for conflicts.
            % To do this, return Forms from typegen. That way we can include
            % them in the final module

            % Another but separate step is to reorder the parsing logic so we
            % collect types before assembling the module. That will allow us to
            % check if a subtype exists when it's exported.
            ({qualified_type, _, Symbols} = Elem) ->
                [P, T] = [S || {symbol, _, _, S} <- Symbols],
                case maps:is_key(P, Defs) of
                    true  -> {ok, T};
                    false -> error:format({export_missing, module:kind_name([P, T])}, {module, Elem})
                end;
            % blah
            (Elem) ->
                case maps:is_key(symbol:tag(Elem), Defs) of
                    true  -> {ok, symbol:tag(Elem)};
                    false -> error:format({export_missing, symbol:tag(Elem)}, {module, Elem})
                end
        end,
    Name = [S || {symbol, _, variable, S} <- Path],
    case error:collect([F(E) || E <- Exports]) of
        {error, Errs}   -> {error, Errs};
        {ok, Tags}      -> ExportMap = maps:from_list(lists:zip(Tags, Exports)),
                           {ok, {module, Ctx, Name, ExportMap}}
    end.

beam_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString])).

kind_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('/', Path)],
    list_to_atom(lists:flatten([PathString])).

