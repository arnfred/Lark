-module(def_gen).
-export([gen/3]).

gen(TypesEnv, TypeForms, AST) ->
    case gen_defs(TypesEnv, AST) of
        {error, Errs}           -> {error, Errs};
        {ok, {DefForms, _}}     -> gen_modules(DefForms, TypeForms, AST)
    end.

gen_defs(TypesEnv, AST) ->
    ast:traverse(fun pre_gen_term/3, fun code_gen:gen/3, TypesEnv, AST).

pre_gen_term(top_level, _, {type_def, _, _, _}) -> skip;
pre_gen_term(Type, Scope, Term)                    -> code_gen:pre_gen(Type, Scope, Term).

gen_modules(DefFormMap, TypeForms, {ast, _, Modules, _, Defs}) ->

    DefForms = [maps:get(Name, DefFormMap) || {def, _, Name, _} <- maps:values(Defs)],

    % Generate module with `main` function callable
    MainModule = gen_main_module(Defs, DefForms, TypeForms),

    ModuleForms = MainModule ++ [gen_module(M, Defs, DefForms, TypeForms) || M <- Modules],
    {ok, ModuleForms}.

gen_module({module, _, Path, Exports}, Defs, DefForms, TypeForms) ->
    TypeExports = [FName || {FName, _Form} <- TypeForms,
                            (cerl:fname_id(FName) == domain) or maps:is_key(cerl:fname_id(FName), Exports)],

    DefExports = [FName || {FName, _} <- DefForms, maps:is_key(cerl:fname_id(FName), Exports)],
    ModuleExports = TypeExports ++ DefExports,
    ModuleName = module:beam_name(Path),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), ModuleExports, [], DefForms ++ TypeForms)}.

gen_main_module(Defs, DefForms, TypeForms) ->
    case maps:is_key(main, Defs) of
        false   -> [];
        true    -> [gen_module({module, #{}, [symbol:id(main)], #{main => true}},
                               Defs, DefForms, TypeForms)]
    end.
