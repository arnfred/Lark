-module(def_gen).
-export([gen/3]).

gen(TypesEnv, TypeForms, AST) ->
    case gen_defs(TypesEnv, AST) of
        {error, Errs}           -> {error, Errs};
        {ok, {DefForms, _}}     -> gen_modules(DefForms, TypeForms, AST)
    end.

gen_defs(TypesEnv, AST) ->
    Pre = fun(Type, Scope, Term) -> pre_gen_term(TypesEnv, Type, Scope, Term) end,
    Post = fun(_, _, _) -> ok end,
    ast:traverse(Pre, Post, AST).

pre_gen_term(_, top_level, _, {type_def, _, _, _, _}) -> skip;
pre_gen_term(TypesEnv, top_level, _, Term)       -> {change, expr_gen:gen(TypesEnv), Term};
pre_gen_term(TypesEnv, pattern, _, Term)         -> {change, pattern_gen:gen(TypesEnv), Term};
pre_gen_term(TypesEnv, expr, _, Term)            -> {change, expr_gen:gen(TypesEnv), Term};
pre_gen_term(_, _, _, _)                         -> ok.

gen_modules(DefFormMap, TypeForms, {ast, _, Modules, _, Defs}) ->

    DefForms = [maps:get(Name, DefFormMap) || {def, _, Name, _, _} <- maps:values(Defs)],

    % Generate module with `main` function callable
    MainModule = gen_main_module(Defs, DefForms, TypeForms),

    ModuleForms = MainModule ++ [gen_module(M, Defs, DefForms, TypeForms) || M <- Modules],
    {ok, ModuleForms}.

gen_module({module, _, Path, Exports}, Defs, DefForms, TypeForms) ->
    {TypeExports, _} = lists:unzip(TypeForms), 
    DefExports = [cerl:c_fname(Name, length(Args)) || {def, _, Name, Args, _} <- maps:values(Defs), 
                                                           maps:is_key(Name, Exports)],
    ModuleExports = TypeExports ++ DefExports,
    ModuleName = module:beam_name(Path),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), ModuleExports, [], DefForms ++ TypeForms)}.

gen_main_module(Defs, DefForms, TypeForms) ->
    case maps:is_key(main, Defs) of
        false   -> [];
        true    -> [gen_module({module, #{}, [symbol:id(main)], #{main => true}},
                               Defs, DefForms, TypeForms)]
    end.
