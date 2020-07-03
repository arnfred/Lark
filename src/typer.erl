-module(typer).
-export([type/2, load/2]).

type(Module, Defs) -> 
    case load(Module, Defs) of
        {error, Errs} -> {error, Errs};
        {ok, TypeMod} ->
            Envs = scanner:scan(TypeMod, Defs),
            io:format("Envs: ~p~n", [Envs]),
            {ok, Envs}
    end.

load(Module, Defs) ->
    TypeMod = lists:flatten([Module, "_types"]),
    case typegen:gen(TypeMod, Defs) of
        {error, Errs} -> 
            io:format("Errs: ~p~n", [Errs]),
            {error, Errs};
        {ok, Forms} ->
            io:format("Forms are ~p~n", [Forms]),
            case compile:forms(Forms, [report, verbose, from_core]) of
                error -> error:format({compilation_error}, {typer, Forms});
                {error, Err} -> error:format({compilation_error, Err}, {typer, Forms});
                {ok, Mod, TypeBin} ->
                    io:format("Mod: ~p, TypeMod: ~p~n", [Mod, TypeMod]),
                    BeamName = lists:flatten([TypeMod, ".beam"]),
                    code:load_binary(Mod, BeamName, TypeBin),
                    {ok, Mod}
            end
    end.
