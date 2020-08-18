-module(typer).
-export([type/1, load/1]).

type(AST) -> 
    case load(AST) of
        {error, Errs}                                       -> {error, Errs};
        {ok, {Exported, ScannerMod, TypeMods, DomainDef}}   ->
            Envs = scanner:scan(ScannerMod, AST),
            true = code:soft_purge(ScannerMod),
            true = code:delete(ScannerMod),
            {ok, {Envs, Exported, TypeMods, DomainDef}}
    end.

load(AST) ->
    case typegen:gen(AST) of
        {error, Errs}                                       -> {error, Errs};
        {ok, {Exported, ScannerMod, TypeMods, DomainDef}}   ->
            AllModules = [ScannerMod | TypeMods],
            LoadedModules = error:collect([load_type_module(Name, Form) || {Name, Form} <- AllModules]),
            error:map(LoadedModules, fun([ScannerModule | Mods]) ->
                                             {Exported, ScannerModule, Mods, DomainDef} end)
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

