-module(typer).
-export([type/3, load/2]).

type(Module, TypeDefs, Defs) -> 
    case load(Module, TypeDefs) of
        {error, Errs} -> {error, Errs};
        {ok, TypeMod} ->
            Envs = scanner:scan(TypeMod, Defs),
            io:format("Envs: ~p~n", [Envs]),
            {ok, ok}
    end.

load(Module, TypeDefs) ->
    TypeMod = lists:flatten([Module, "_types"]),
    case typegen:gen(TypeMod, TypeDefs) of
        {error, Errs} -> 
            io:format("Errs: ~p~n", [Errs]),
            {error, Errs};
        {ok, Forms} ->
            io:format("Forms are ~p~n", [Forms]),
            {ok, Mod, TypeBin} = compile:forms(Forms, [report, verbose, from_core]),
            io:format("Mod: ~p, TypeMod: ~p~n", [Mod, TypeMod]),
            BeamName = lists:flatten([TypeMod, ".beam"]),
            code:load_binary(Mod, BeamName, TypeBin),
            {ok, Mod}
    end.



