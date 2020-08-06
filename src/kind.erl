-module(kind).
-export([load/1]).
-import(lists, [zip/2, unzip/1]).
-include_lib("eunit/include/eunit.hrl").

load(CodeRaw) ->
    Code = "import kind/prelude/_\n" ++ CodeRaw,
    case parser:parse([{path, "src/lib/"}, {text, Code}]) of
        {error, Errs}   -> {error, Errs};
        {ok, ASTs}     ->
            io:format("Tagged AST is ~p~n", [ASTs]),
            case error:collect([typer:type(Path, AST) || {Path, AST} <- ASTs]) of
                {error, Errs} -> {error, Errs};
                {ok, _}       ->
                    Forms = lists:flatten([codegen:gen(AST) || {_, AST} <- ASTs]),
                    io:format("Erlang Core Forms are ~p~n", [Forms]),
                    compile(Forms)
            end
    end.

compile(Forms) ->
    Options = [report, verbose, from_core],
    CompiledList = [compile_form(Form, Options) || {_, Form} <- Forms],
    case error:collect(CompiledList) of
        {error, Errs}   -> {error, Errs};
        {ok, Bins}      ->
            LoadedModules = [load_binary(B) || B <- Bins],
            error:collect(LoadedModules)
    end.

compile_form(Form, Options) ->
    Module = cerl:atom_val(cerl:module_name(Form)),
    case compile:forms(Form, Options) of
        {ok, Module, Bin}   -> {ok, {Module, Bin}};
        Error               -> error:format({compile_error, Error}, {kind, Module})
    end.

load_binary({Module, Bin}) ->
    BeamName = lists:flatten(io_lib:format("~w.beam", [Module])),
    case code:load_binary(Module, BeamName, Bin) of
        {module, Module} -> {ok, Module};
        {error, Err}     -> error:format({loading_error, Err}, {kind, Module})
    end.

