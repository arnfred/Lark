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
    case lexer:string(Code) of
        {error, Error}          -> error:format({lexer_error, Error},{kind});
        {error, Error1, Error2} -> error:format({lexer_error, Error1, Error2},{kind});
        {ok, Tokens, _}         ->
            case parser:parse(Tokens) of
                {error, Error}  -> error:format({parser_error, Error}, {kind});
                {ok, Parsed}    ->
                    case preener:preen(Parsed) of
                        {error, Errs}   -> {error, Errs};
                        {ok, AST}       ->
                            io:format("AST is ~p~n", [AST]),
                            tagger:tag(AST)
                    end
            end
    end.
