-module(kind).
-export([compile/1]).

compile({Module, Code}) ->
    case parser:parse(text, [Code]) of
        {error, Errs}   -> {error, Errs};
        {ok, [{_, AST}]}     ->
            io:format("Tagged AST is ~p~n", [AST]),
            case typer:type(Module, AST) of
                {error, Errs} -> {error, Errs};
                {ok, _}       ->
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
