-module(code_gen).
-export([gen/2]).

gen(Scope, {module, _, ModulePath, _, Exports, _} = Module) -> 
    ModuleName = module:beam_name(ModulePath),
    case ast:traverse(fun pre_gen/3, fun post_gen/3, Scope, Module) of
        {error, Errs}                               -> {error, Errs};
        {ok, {_, {module, _, _, _, _, DefFormMap}}}   ->
            ExportForms = [FName || {Tag, {FName, _}} <- maps:to_list(DefFormMap), maps:is_key(Tag, Exports)],
            DefForms = maps:values(DefFormMap),
            {ok, cerl:c_module(cerl:c_atom(ModuleName), ExportForms, [], DefForms)}
    end.

post_gen(expr, Scope, Term) -> expr_gen:gen_expr(expr, Scope, Term);
post_gen(pattern, Scope, Term) -> pattern_gen:gen_pattern(pattern, Scope, Term);
post_gen(top_level, Scope, Term) -> expr_gen:gen_expr(expr, Scope, Term).

pre_gen(pattern, _, {application, _, _, _}) -> leave_intact;
pre_gen(pattern, _, {qualified_application, _, _, _, _}) -> leave_intact;
pre_gen(_, _, _) -> ok.
