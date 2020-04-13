-module(kind).
-export([compile/1]).

compile({Module, Code}) ->
    {ok, Tokens, _} = lexer:string(Code),
    {ok, Parsed} = parser:parse(Tokens),
    {ok, Forms} = codegen:gen({Module, Parsed}),
    {ok, Mod, Bin} = compile:forms(Forms, [report, verbose, from_core]),
    BeamName = lists:flatten(io_lib:format("~w.beam", [Module])),
    code:load_binary(Mod, BeamName, Bin).
