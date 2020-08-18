-module(module).
-export([format/2, beam_name/1, kind_name/1]).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

format(Sources, Types) ->
    case error:collect([prepare(File, AST, Ts) || {{File, AST}, Ts} <- lists:zip(Sources, Types)]) of
        {error, Errs}           -> {error, Errs};
        {ok, PreparedSources}   ->
            Modules = [{Name, Term, File} || {File, {ast, _, Modules, _, _}} <- PreparedSources,
                                                    {module, _, Name, _, _} = Term <- Modules],
            KeyF = fun({_, Name, _}) -> Name end,
            ErrF = fun({{Name, Term1, File1}, {Name, Term2, File2}}) ->
                           error:format({duplicate_module, Name, File1, File2}, {module, Term1, Term2}) end,
            case utils:duplicates(Modules, KeyF) of
                []      -> {ok, PreparedSources};
                Dups    -> error:collect([ErrF(D) || D <- Dups])
            end
    end.

prepare(File, Code, Types) ->
    Modules = [M || M = {module, _, _, _} <- Code],
    Imports = [I || I = {import, _, _} <- Code],
    Defs = maps:from_list([{Name, T} || T = {Type, _, Name, _, _} <- Code, Type == type_def orelse Type == def]),
    case error:collect([handle_modules(M, Defs, Types) || M <- Modules]) of
        {error, Errs}   -> {error, Errs};
        {ok, Mods}        -> {ok, {File, {ast, #{file => File}, Mods, Imports, Defs}}}
    end.

handle_modules({module, ModuleCtx, Path, Exports}, Defs, Types) ->
    F = fun
            % blah: T
            Make_export({pair, Ctx, K, V}) ->
                error:map(Make_export(K), fun({Export, _, Key, none}) ->
                                                  {Export, Ctx, Key, V} end);
            % Boolean/True
            Make_export({qualified_type, Ctx, Symbols} = Elem) ->
                [P, T] = [S || {symbol, _, _, S} <- Symbols],
                case maps:is_key(P, Defs) andalso 
                     maps:is_key(P, Types) andalso
                     lists:member(T, maps:get(P, Types)) of
                    false -> error:format({export_missing, module:kind_name([P, T])}, {module, Elem});
                    true  -> case maps:is_key(T, Defs) of
                                 false  -> {ok, {T, {type_export, Ctx, [P, T], none}}};
                                 true   -> error:format({type_export_already_defined, symbol:tag([P, T]), T}, {module, Elem})
                             end
                end;
            % blah
            Make_export({symbol, Ctx, Kind, Val} = Elem) ->
                Export = case Kind of
                             variable -> export;
                             type     -> type_export
                         end,
                case maps:is_key(symbol:tag(Elem), Defs) of
                    true  -> {ok, {symbol:tag(Elem), {Export, Ctx, [Val], none}}};
                    false -> error:format({export_missing, symbol:tag(Elem)}, {module, Elem})
                end
        end,
    Name = [S || {symbol, _, _, S} <- Path],
    case error:collect([F(E) || E <- Exports]) of
        {error, Errs}       -> {error, Errs};
        {ok, ExportTerms}   -> ExportMap = maps:from_list(ExportTerms),
                               {ok, {module, ModuleCtx, Name, ExportMap}}
    end.


beam_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString])).

kind_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('/', Path)],
    list_to_atom(lists:flatten([PathString])).

