-module(typer).
-export([type/2]).

-include_lib("eunit/include/eunit.hrl").

type({module, _, _, _, Exports, Defs} = Module, _Options) ->
    case domain_gen:gen(Module) of
        {error, Errs}       	-> {error, Errs};
        {TypesEnv, DomainForm}  ->
            case load_domain_module(DomainForm) of
                {error, Errs}       -> {error, Errs};
                {ok, ModuleName}    ->
                    case error:collect([typecheck_def(ModuleName, E) || E <- maps:keys(Exports),
                                                                        maps:is_key(E, Defs)]) of
                        {error, Errs}   -> {type_error, ModuleName, Errs};
                        {ok, _}         -> {ok, {TypesEnv, ModuleName}}
                    end
            end
    end.

typecheck_def(DomainMod, Name) ->
    Arity = utils:get_min_arity(DomainMod, Name),
    Anys = fun(N) -> [any || _ <- lists:seq(1, N)] end,
    erlang:apply(DomainMod, Name, [normal | Anys(Arity)]).

load_domain_module(ModuleForm) ->
    ModuleName = cerl:atom_val(cerl:module_name(ModuleForm)),
    case compile:forms(ModuleForm, [report, verbose, from_core]) of
        error -> error:format({compilation_error}, {typer, ModuleName});
        {error, Err} -> error:format({compilation_error, Err}, {typer, ModuleName});
        {ok, ModuleName, TypeBin} ->
            BeamName = lists:flatten([atom_to_list(ModuleName), ".beam"]),
            code:load_binary(ModuleName, BeamName, TypeBin),
            {ok, ModuleName}
    end.
