-module(kind).
-export([compile/1, get_AST/1]).

compile({Module, Code}) ->
    {ok, _, {TypeAST, DefAST}} = get_AST(Code),
    io:format("Tagged AST is ~p~n", [DefAST]),
    {ok, _} = typer:type(Module, TypeAST, DefAST),
    {ok, Forms} = codegen:gen({Module, DefAST}),
    io:format("Erlang Core Forms are ~p~n", [Forms]),
    {ok, Mod, Bin} = compile:forms(Forms, [report, verbose, from_core]),
    BeamName = lists:flatten(io_lib:format("~w.beam", [Module])),
    code:load_binary(Mod, BeamName, Bin).

get_AST(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
    {ok, AST} = parser:parse(Tokens),
    io:format("AST is ~p~n", [AST]),
    tagger:tag(AST).
