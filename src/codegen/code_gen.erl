-module(code_gen).
-export([gen/3, pre_gen/3]).

gen(expr, Scope, Term) -> expr_gen:gen_expr(expr, Scope, Term);
gen(pattern, Scope, Term) -> pattern_gen:gen_pattern(pattern, Scope, Term);
gen(top_level, Scope, Term) -> expr_gen:gen_expr(expr, Scope, Term).

pre_gen(pattern, _, {application, _, _, _}) -> leave_intact;
pre_gen(pattern, _, {qualified_type_application, _, _, _, _}) -> leave_intact;
pre_gen(_, _, _) -> ok.
