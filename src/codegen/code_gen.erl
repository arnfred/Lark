-module(code_gen).
-export([gen/1]).

gen({module, _, ModulePath, _, Exports, _} = Module) -> 
    ModuleName = module:beam_name(ModulePath),
    case ast:traverse(fun pre_gen/3, fun post_gen/3, #{}, Module) of
        {error, Errs}                               -> {error, Errs};
        {ok, {_, {module, _, _, _, _, DefFormMap}}}   ->
            ExportForms = [FName || {Tag, {FName, _}} <- maps:to_list(DefFormMap), maps:is_key(Tag, Exports)],
            DefForms = maps:values(DefFormMap),
            {ok, cerl:c_module(cerl:c_atom(ModuleName), ExportForms, [], DefForms)}
    end.

post_gen(expr, Scope, Term) -> expr_gen:gen_expr(Scope, Term);
post_gen(pattern, Scope, Term) -> pattern_gen:gen_pattern(Scope, Term);
post_gen(top_level, Scope, Term) -> expr_gen:gen_expr(Scope, Term).

pre_gen(_, _, _) -> ok.
