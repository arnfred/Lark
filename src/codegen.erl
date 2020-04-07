-module(codegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").

gen({Name, Defs}) ->
    io:format("Defs are ~p~n", [Defs]),
    Env = gen_module_env(Defs),
    io:format("Env is ~w~n", [Env]),
    CompiledDefs = [gen_def(Env, D) || D <- Defs],
    CompiledExports = [cerl:c_fname(N, Arity) || {N, Arity} <- Env],
    {ok, cerl:c_module(cerl:c_atom(Name), CompiledExports, [], CompiledDefs)}.

substitute_underscore('_') -> 
    N = case get('_') of
            undefined -> 0;
            S -> S
        end,
    put('_', N+1),
    list_to_atom(lists:flatten(io_lib:format("_q~w", [N])));
substitute_underscore(Var) -> Var.

gen_module_env(Defs) -> 
    [{Name, length(Args)} || {_, _, [{symbol, _, Name}|Args], _} <- Defs].

gen_def(Env, {def, _, [{symbol, _, Name}|Args], Body}) ->
    io:format("~nCompiling definition ~s~n", [Name]),
    FName = cerl:c_fname(Name, length(Args)),
    CompiledArgs = [gen_var(A) || {symbol, _, A} <- Args],
    CompiledBody = gen_expr(Env, {def_body, Args, Body}),
    {FName, cerl:c_fun(CompiledArgs, CompiledBody)};

gen_def(Env, {type, _, Name, Body}) ->
    io:format("~nCompiling type ~s with body ~p ~n", [Name, Body]),
    FName = cerl:c_fname(Name, 0),
    CompiledBody = gen_type_fun(Env, Body),
    {FName, cerl:c_fun([], CompiledBody)}.

gen_pattern_match(Env, Args, Clauses) ->
    CompiledClauses = [gen_clause(Env, Patterns, Expr) || {pattern_clause, _, Patterns, Expr} <- Clauses],
    CompiledArgs = cerl:c_values([gen_var(A) || {symbol, _, A} <- Args]),
    cerl:c_case(CompiledArgs, CompiledClauses). 

gen_clause(Env, Patterns, Expr) ->
    CompiledPatterns = [gen_expr(Env, P) || P <- Patterns],
    cerl:c_clause(CompiledPatterns, gen_expr(Env, Expr)).

gen_type_fun(_, Values) ->
    Instances = [cerl:c_map_pair(cerl:c_atom(S), cerl:c_atom(true)) ||{type_symbol, _, S} <- Values],
    cerl:c_map(Instances).

gen_var(Var) -> cerl:c_var(substitute_underscore(Var)).

gen_expr(_, {symbol, _, S}) -> gen_var(S);

gen_expr(Env, {application, _, Name, Args}) ->
    Arity = length(Args),
    FName = cerl:c_fname(Name, Arity),
    cerl:c_apply(FName, [gen_expr(Env, E) || E <- Args]);

gen_expr(_, {type_application, _, S, []}) -> cerl:c_atom(S);

gen_expr(Env, {expr_match, _, Expr, Clauses}) ->
    CompiledClauses = [gen_clause(Env, Patterns, E) || {pattern_clause, _, Patterns, E} <- Clauses],
    cerl:c_case(gen_expr(Env, Expr), CompiledClauses);

gen_expr(Env, {def_body, Args, {def_match, Clauses}}) ->
    gen_pattern_match(Env, Args, Clauses);

gen_expr(Env, {def_body, _, Body}) -> gen_expr(Env, Body).


-ifdef(TEST).

binary(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
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
        "def alwaysTrue a = True",
    RunAsserts = fun(Mod) -> ?assertEqual('True', Mod:alwaysTrue(2)) end,
    run(Code, RunAsserts).

pattern_match_test() ->
    Code = 
        "def rexor a\n"
        " | True -> False\n"
        " | False -> True",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:rexor('False')),
                         ?assertEqual('False', Mod:rexor('True'))
                 end,
    run(Code, RunAsserts).

pattern_match_multivariate_test() ->
    Code = 
        "def rexor a b\n"
        " | True False -> True\n"
        " | False True -> True\n"
        " | _ _ -> False",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:rexor('True', 'False')),
                         ?assertEqual('False', Mod:rexor('True', 'True'))
                 end,
    run(Code, RunAsserts).

pattern_match_expr_syntax1_test() ->
    Code = 
        "def test1 a = a.match(False -> True | True -> False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:test1('False'))
                 end,
    run(Code, RunAsserts).
pattern_match_expr_syntax2_test() ->
    Code = 
        "def test2 a = a.match(False -> True, True -> False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:test2('False'))
                 end,
    run(Code, RunAsserts).
pattern_match_expr_syntax3_test() ->
    Code = 
        "def test3 a = a.match(False -> True\n"
        "                      True -> False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:test3('False'))
                 end,
    run(Code, RunAsserts).
pattern_match_expr_syntax4_test() ->
    Code = 
        "def test4 a = a.match(False -> True,\n"
        "                      True -> False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:test4('False'))
                 end,
    run(Code, RunAsserts).

underscore_arg_test() ->
    Code = 
        "def blip _ _ c = c",
    RunAsserts = fun(Mod) -> ?assertEqual(blop, Mod:blip(blip, blab, blop)) end,
    run(Code, RunAsserts).

-endif.
