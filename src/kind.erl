-module(kind).
-export([compile/1, get_AST/1]).

compile({Module, Code}) ->
    case get_AST(Code) of
        {error, Errs} -> {error, Errs};
        {ok, {_Env, AST}} ->
            io:format("Tagged AST is ~p~n", [AST]),
            case typer:type(Module, AST) of
                {error, Errs} -> {error, Errs};
                {ok, _} ->
                    case codegen:gen({Module, AST}) of
                        {error, Errs} -> {error, Errs};
                        {ok, Forms} ->
                            io:format("Erlang Core Forms are ~p~n", [Forms]),
                            case compile:forms(Forms, [report, verbose, from_core]) of
                                {ok, Mod, Bin} ->
                                    BeamName = lists:flatten(io_lib:format("~w.beam", [Module])),
                                    code:load_binary(Mod, BeamName, Bin);
                                Error -> Error
                            end
                    end
            end
    end.

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
