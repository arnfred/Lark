-module(kind).
-export([compile/1]).

compile({Module, Code}) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
    {ok, AST} = parser:parse(Tokens),
    io:format("AST is ~p~n", [AST]),
    {ok, _, TaggedAST} = tagger:tag(AST),
    io:format("Tagged AST is ~p~n", [TaggedAST]),
    {ok, Env} = typer:type(AST),
    {ok, Forms} = codegen:gen({Module, Env, TaggedAST}),
    io:format("Erlang Core Forms are ~p~n", [Forms]),
    {ok, Mod, Bin} = compile:forms(Forms, [report, verbose, from_core]),
    BeamName = lists:flatten(io_lib:format("~w.beam", [Module])),
    code:load_binary(Mod, BeamName, Bin).
