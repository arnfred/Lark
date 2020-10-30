-module(def_gen).
-export([gen/2]).
-include_lib("eunit/include/eunit.hrl").

gen(TypesEnv, {ast, _, Modules, _, _} = AST) ->
    case gen_defs(TypesEnv, AST) of
        {error, Errs}       -> {error, Errs};
        {ok, {Defs, _}}     ->
            ModuleName = symbol:id(defs),
            Arities = count_arities(Defs, TypesEnv),
            RootModule = gen_root_module(Arities, ModuleName, Defs, TypesEnv),
            ExportedModules = gen_modules(Arities, RootModule, Modules),
            {ok, [RootModule | ExportedModules]}
    end.

gen_defs(TypesEnv, AST) ->
    ast:traverse(fun pre_gen_term/3, fun code_gen:gen/3, TypesEnv, AST).

pre_gen_term(top_level, _, {type_def, _, _, _}) -> skip;
pre_gen_term(Type, Scope, Term)                 -> code_gen:pre_gen(Type, Scope, Term).

gen_root_module(Arities, ModuleName, Defs, TypesEnv) ->
    Types = [{cerl:c_fname(Name, maps:get(Name, Arities)), Fun} || {Name, Fun} <- maps:to_list(TypesEnv),
                                                                   not(maps:is_key(Name, Defs))],
    {TypeExports, _} = lists:unzip(Types),

    {DefExports, _} = lists:unzip(maps:values(Defs)),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), DefExports ++ TypeExports, [], maps:values(Defs) ++ Types)}.

count_arities(Defs, TypesEnv) ->
    TypeArities = maps:map(fun(_, F) -> case cerl:is_c_fun(F) of
                                            true    -> cerl:fun_arity(F);
                                            false   -> 0
                                        end end, TypesEnv),
    DefArities = maps:map(fun(_, {FName, _}) -> cerl:fname_arity(FName) end, Defs),
    maps:merge(TypeArities, DefArities).

gen_modules(Arities, ModuleName, Modules) ->

    % Generate module with `main` function callable
    MainModule = gen_main_module(Arities, ModuleName),

    MainModule ++ [gen_module(Arities, ModuleName, M) || M <- Modules].

gen_module(Arities, RootModule, {module, _, ModulePath, Exports}) ->

    ModuleDefs = [gen_call_form(Name, maps:get(Name, Arities, 0), RootModule, ModulePath)
                  || Name <- maps:keys(Exports)],
    {ModuleExports, _} = lists:unzip(ModuleDefs),
    ModuleName = module:beam_name(ModulePath),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), ModuleExports, [], ModuleDefs)}.

gen_main_module(Arities, ModuleName) ->
    case maps:is_key(main, Arities) of
        false   -> [];
        true    -> [gen_module(Arities, ModuleName, {module, #{}, [symbol:id(main)], #{main => true}})]
    end.

% Call def form for the module if it exists (i.e. if `Name` refers to a `def`
% and not `type`), otherwise, call domain (e.g. for type constants)
gen_call_form(Name, Arity, {RootName, RootModule}, ModulePath) ->
    DomainModule = module:beam_name(ModulePath ++ [domain]),
    RootExports = maps:from_list([{cerl:var_name(Var), true} || Var <- cerl:module_exports(RootModule)]),
    Module = case maps:is_key({Name, Arity}, RootExports) of
                 true   -> RootName;
                 false  -> DomainModule
             end,
    Vars = [cerl:c_var(symbol:id(arg)) || _ <- lists:seq(1, Arity)],
    FName = cerl:c_fname(Name, Arity),
    {FName, cerl:c_fun(Vars, cerl:c_call(cerl:c_atom(Module), cerl:c_atom(Name), Vars))}.
