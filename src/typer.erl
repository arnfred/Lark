-module(typer).
-export([type/3, load/2]).

type(Module, TypeDefs, Defs) -> 
    {ok, TypeMod} = load(Module, TypeDefs),
    Envs = scanner:scan(TypeMod, Defs),
    io:format("Envs: ~p~n", [Envs]),
    {ok, ok}.

load(Module, TypeDefs) ->
    TypeMod = lists:flatten([Module, "_types"]),
    {ok, Forms} = typegen:gen(TypeMod, TypeDefs),
    io:format("Forms are ~p~n", [Forms]),
    {ok, Mod, TypeBin} = compile:forms(Forms, [report, verbose, from_core]),
    io:format("Mod: ~p, TypeMod: ~p~n", [Mod, TypeMod]),
    BeamName = lists:flatten([TypeMod, ".beam"]),
    code:load_binary(Mod, BeamName, TypeBin),
    {ok, Mod}.


