-module(codegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").

gen({Name, Defs}) ->
    io:format("Defs are ~p~n", [Defs]),
    Env = gen_module_env(Defs),
    io:format("Env is ~w~n", [Env]),
    CompiledDefs = [gen_def(Env, D) || D <- Defs],
    CompiledExports = [cerl:c_fname(N, Arity) || {N, Arity} <- Env],
    {ok, cerl:c_module(
	   cerl:c_atom(Name),
	   CompiledExports,
	   [],
	   CompiledDefs)
    }.

gen_module_env(Defs) -> [{Name, length(Args)} || {_, _, [{symbol, _, Name}|Args], _} <- Defs].

gen_def(Env, {def, _, [{symbol, _, Name}|Args], Body}) ->
    io:format("~nCompiling definition ~s~n", [Name]),
    FName = cerl:c_fname(Name, length(Args)),
    CompiledArgs = [cerl:c_var(A) || {symbol, _, A} <- Args],
    CompiledBody = gen_expr(Env, Body),
    {FName, cerl:c_fun(CompiledArgs, CompiledBody)};

gen_def(Env, {def_pattern_match, _, [{symbol, _, Name}|Args], {pattern_match, Clauses}}) ->
    io:format("~nCompiling pattern definition ~s~n", [Name]),
    FName = cerl:c_fname(Name, length(Args)),
    CompiledArgs = [cerl:c_var(A) || {symbol, _, A} <- Args],
    io:format("Args: ~p~n", [Args]),
    io:format("Clauses: ~p~n", [Clauses]),
    CompiledBody = gen_pattern_match(Env, Args, Clauses),
    {FName, cerl:c_fun(CompiledArgs, CompiledBody)};

gen_def(Env, {type, _, Name, Body}) ->
    io:format("~nCompiling type ~s with body ~p ~n", [Name, Body]),
    FName = cerl:c_fname(Name, 0),
    CompiledBody = gen_type_fun(Env, Body),
    {FName, cerl:c_fun([], CompiledBody)}.

gen_pattern_match(Env, [{symbol, _, Arg}], Clauses) ->
    CompiledClauses = [gen_clause(Env, [Pattern], Expr) || {pattern_clause, _, Pattern, Expr} <- Clauses],
    cerl:c_case(cerl:c_var(Arg), CompiledClauses). 

gen_clause(Env, Patterns, Expr) ->
    CompiledPatterns = [gen_expr(Env, P) || P <- Patterns],
    cerl:c_clause(CompiledPatterns, gen_expr(Env, Expr)).

gen_type_fun(_, Values) ->
    Instances = [cerl:c_map_pair(cerl:c_atom(S), cerl:c_atom(true)) ||{type_symbol, _, S} <- Values],
    cerl:c_map(Instances).

gen_expr(_, {symbol, _, S}) -> cerl:c_var(S);

gen_expr(Env, {application, _, Name, Args}) ->
    Arity = length(Args),
    FName = cerl:c_fname(Name, Arity),
    cerl:c_apply(FName, [gen_expr(Env, E) || E <- Args]);

gen_expr(_, {type_application, _, S, []}) -> cerl:c_atom(S).


-ifdef(TEST).

binary(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    {ok, Parsed} = parser:parse(Tokens),
    {ok, Forms} = gen({"test", Parsed}),
    compile:forms(Forms, [report, verbose, from_core]).

run(Code, RunAsserts) ->
    {ok, Mod, Bin} = binary(Code),
    {module, Mod} = code:load_binary(Mod, "test.beam", Bin),
    RunAsserts(Mod),
    true = code:soft_purge(Mod),
    true = code:delete(Mod).

identity_compile_test() ->
    Code = "def id a = a",
    {ok, _, _Bin} = binary(Code).

identity_run_test() ->
    Code = "def id a = a",
    RunAsserts = fun(Mod) ->
                         ?assertEqual(2, Mod:id(2)),
                         ?assertEqual(1.3, Mod:id(1.3)),
                         ?assertEqual("string", Mod:id("string")),
                         ?assertEqual(atom, Mod:id(atom))
                 end,
    run(Code, RunAsserts).

function_call_test() ->
    Code = 
        "def id a = a\n"
        "def callId b = b.id",
    RunAsserts = fun(Mod) -> ?assertEqual(2, Mod:callId(2)) end,
    run(Code, RunAsserts).

function_call_multiple_args_test() ->
    Code = 
        "def firstId a b c = a\n"
        "def callId a b = b.firstId(b, a)",
    RunAsserts = fun(Mod) -> ?assertEqual(3, Mod:callId(2, 3)) end,
    run(Code, RunAsserts).

always_true_test() ->
    Code = 
        "type Bool = True | False\n"
        "def alwaysTrue a = True",
    RunAsserts = fun(Mod) -> ?assertEqual('True', Mod:alwaysTrue(2)) end,
    run(Code, RunAsserts).

pattern_match_test() ->
    Code = 
        "type Bool = True | False\n"
        "def rexor a\n"
        " | True -> False\n"
        " | False -> True",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:rexor('False')),
                         ?assertEqual('False', Mod:rexor('True'))
                 end,
    run(Code, RunAsserts).

-endif.
