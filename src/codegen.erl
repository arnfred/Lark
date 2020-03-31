-module(codegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").

gen({Name, Defs}) ->
    Env = gen_module_env(Defs),
    io:format("Defs are ~w~n", Defs),
    io:format("Env is ~w~n", [Env]),
    CompiledDefs = [gen_def(Env, D) || D <- Defs],
    CompiledExports = [cerl:c_fname(N, Arity) || {N, Arity} <- Env],
    {ok, cerl:c_module(
	   cerl:c_atom(Name),
	   CompiledExports,
	   [],
	   CompiledDefs)
    }.

gen_module_env(Defs) -> [{Name, length(Args)} || {def, _, [{symbol, _, Name}|Args], _} <- Defs].

gen_def(Env, {def, _, [{symbol, _, Name}|Args], Body}) ->
    io:format("~nCompiling definition ~s~n", [Name]),
    FName = cerl:c_fname(Name, length(Args)),
    CompiledArgs = [cerl:c_var(A) || {symbol, _, A} <- Args],
    CompiledBody = gen_expr(Env, Body),
    {FName, cerl:c_fun(CompiledArgs, CompiledBody)}.

gen_expr(_, {symbol, _, S}) ->
    cerl:c_var(S);
gen_expr(Env, {application, _, Name, Args}) ->
    Arity = length(Args),
    FName = cerl:c_fname(Name, Arity),
    cerl:c_apply(FName, [gen_expr(Env, E) || E <- Args]).


-ifdef(TEST).

parse_and_gen(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    {ok, Parsed} = parser:parse(Tokens),
    {ok, Forms} = codegen:gen({"test", Parsed}),
    compile:forms(Forms, [report, verbose, from_core]).

identity_compile_test() ->
    Code =
        "def id a = a",
    {ok, _, _Bin} = parse_and_gen(Code).

-endif.
