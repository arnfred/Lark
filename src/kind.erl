-module(kind).
-export([load/1]).
-import(lists, [zip/2, zip3/3, unzip/1]).
-include_lib("eunit/include/eunit.hrl").

load(CodeRaw) ->
    Code = "import kind/prelude/_\n" ++ CodeRaw,
    case parser:parse([{path, "src/lib/"}, {text, Code}]) of
        {error, Errs}   -> {error, Errs};
        {ok, ASTs}     ->
            io:format("Tagged AST is ~p~n", [ASTs]),
            case error:collect([type_and_compile(AST) || AST <- ASTs]) of
                {error, Errs}       -> {error, Errs};
                {ok, Modules}       -> {ok, lists:flatten(Modules)}
            end
    end.

% Current problem: As we run typegen and codegen, we call out to compiled type
% definitions from other modules that might or might not be compiled yet. To
% ensure that types are compiled as we need them, we have to order the sources
% by their dependencies
type_and_compile(AST) ->
    case typer:type(AST) of
        {error, Errs}                               -> {error, Errs};
        {ok, {_, Types, TypeModules, DomainDef}} ->
            {ok, Forms} = codegen:gen(AST, DomainDef, Types),
            case compile(Forms) of
                {error, Errs}       -> {error, Errs};
                {ok, ModuleNames}   -> {ok, ModuleNames ++ TypeModules}
            end
    end.

compile(Forms) ->
    Options = [report, verbose, from_core],
    CompiledList = [compile_form(Form, Options) || {_, Form} <- Forms],
    case error:collect(CompiledList) of
        {error, Errs}   -> {error, Errs};
        {ok, Bins}      ->
            LoadedModules = [load_binary(ModuleName, Bin) || {ModuleName, Bin} <- Bins],
            error:collect(LoadedModules)
    end.

compile_form(Form, Options) ->
    ModuleName = cerl:atom_val(cerl:module_name(Form)),
    case compile:forms(Form, Options) of
        {ok, ModuleName, Bin}   -> {ok, {ModuleName, Bin}};
        Error                   -> error:format({compile_error, Error}, {kind, ModuleName})
    end.

load_binary(ModuleName, Bin) ->
    BeamName = lists:flatten(io_lib:format("~w.beam", [ModuleName])),
    case code:load_binary(ModuleName, BeamName, Bin) of
        {module, ModuleName}    -> {ok, ModuleName};
        {error, Err}            -> error:format({loading_error, Err}, {kind, ModuleName})
    end.

