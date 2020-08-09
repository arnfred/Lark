-module(typer).
-export([type/1, load/1]).

type(AST) -> 
    case load(AST) of
        {error, Errs} -> {error, Errs};
        {ok, TypeModules} ->
            [ScannerModule | _] = TypeModules,
            Envs = scanner:scan(ScannerModule, AST),
            io:format("Envs: ~p~n", [Envs]),
            {ok, {TypeModules, Envs}}
    end.

load(AST) ->
    case typegen:gen(AST) of
        {error, Errs}       -> {error, Errs};
        {ok, TypeModules}   -> Loaded = [load_type_module(Name, Form) || {Name, Form} <- TypeModules],
                               error:collect(Loaded)
    end.

load_type_module(Name, ModuleForm) ->
    case compile:forms(ModuleForm, [report, verbose, from_core]) of
        error -> error:format({compilation_error}, {typer, Name});
        {error, Err} -> error:format({compilation_error, Err}, {typer, Name});
        {ok, Name, TypeBin} ->
            BeamName = lists:flatten([atom_to_list(Name), ".beam"]),
            code:load_binary(Name, BeamName, TypeBin),
            {ok, Name}
    end.

