-module(codegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").

gen({Name, Env, AST}) ->
    io:format("Env is ~w~n", [Env]),
    Compiled = [gen_definitions(Env, D) || D <- AST],
    CompiledExports = [cerl:c_fname(N, Arity) || {N, Arity} <- Env],
    {ok, cerl:c_module(cerl:c_atom(Name), CompiledExports, [], Compiled)}.

gen_definitions(Env, {def, _, [{symbol, _, Name, _} | Args], Body}) ->
    io:format("~nCompiling definition ~s~n", [Name]),
    FName = cerl:c_fname(Name, length(Args)),
    CompiledArgs = [gen_expr(Env, A) || A <- Args],
    CompiledBody = case Body of 
                       {clauses, _, Clauses} -> gen_pattern_match(Env, Args, Clauses);
                       Expr                  -> gen_expr(Env, Expr)
                   end,
    {FName, cerl:c_fun(CompiledArgs, CompiledBody)};

gen_definitions(Env, {type, _, Name, Body}) ->
    io:format("~nCompiling type ~s with body ~p ~n", [Name, Body]),
    FName = cerl:c_fname(Name, 0),
    CompiledBody = gen_type_fun(Env, Body),
    {FName, cerl:c_fun([], CompiledBody)}.

gen_pattern_match(Env, Args, Clauses) ->
    CompiledClauses = [gen_clause(Env, Patterns, Expr) || {clause, _, Patterns, Expr} <- Clauses],
    CompiledArgs = cerl:c_values([gen_expr(Env, A) || A <- Args]),
    cerl:c_case(CompiledArgs, CompiledClauses). 

gen_clause(Env, Patterns, Expr) ->
    CompiledPatterns = [gen_expr(Env, P) || P <- Patterns],
    cerl:c_clause(CompiledPatterns, gen_expr(Env, Expr)).

gen_type_fun(_, Values) ->
    Instances = [cerl:c_map_pair(cerl:c_atom(S), cerl:c_atom(true)) ||{type_symbol, _, S, _} <- Values],
    cerl:c_map(Instances).

gen_expr(_, {symbol, _, _, Tag}) -> cerl:c_var(Tag);
gen_expr(_, {type_symbol, _, T, _}) -> cerl:c_atom(T);

gen_expr(Env, {application, _, {qualified_symbol, Symbols}, Args}) ->
    Symbols_ = [S || {symbol, _, S} <- Symbols],
    CompiledArgs = [gen_expr(Env, E) || E <- Args],
    case Symbols_ of
        ['erlang', Module, Name] -> cerl:c_call(cerl:c_atom(Module), cerl:c_atom(Name), CompiledArgs);
        ['erlang', Name]         -> cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom(Name), CompiledArgs)
    end;

gen_expr(Env, {application, _, {symbol, _, Name, Tag}, Args}) ->
    CompiledArgs = [gen_expr(Env, E) || E <- Args],
    FName = case proplists:get_value(Name, Env) of
        undefined -> cerl:c_var(Tag);
        Arity -> cerl:c_fname(Name, Arity)
    end,
    cerl:c_apply(FName, CompiledArgs);

gen_expr(Env, {match, _, Expr, {clauses, _, Clauses}}) -> gen_pattern_match(Env, [Expr], Clauses);

gen_expr(Env, {clauses, Line, Clauses}) ->
    [{clause, _, Patterns, _} | _Rest] = Clauses,
    Symbols = [symbol:id('_') || _ <- Patterns],
    Args = [{symbol, Line, S, S} || S <- Symbols],
    CompiledBody = gen_pattern_match(Env, Args, Clauses),
    CompiledArgs = [gen_expr(Env, A) || A <- Args],
    cerl:c_fun(CompiledArgs, CompiledBody);

gen_expr(_, {tuple, _, []}) -> cerl:c_atom('()');

% This makes is so we can use parenthesis to group evaluation like `(1 + 3).match( ... )`
% However, currently it only evaluates the last element of a tuple.
% In the future we'd like for it to evaluate all statements in a tuple when it's evaluated as an expression
% TODO: Don't just discard everything but the last element. That's a silly thing to do.
gen_expr(Env, {tuple, _, Expressions}) -> lists:last([gen_expr(Env, Expr) || Expr <- Expressions]).


-ifdef(TEST).

run(Code, RunAsserts) ->
    {module, Mod} = kind:compile({"test", Code}),
    RunAsserts(Mod),
    true = code:soft_purge(Mod),
    true = code:delete(Mod).

identity_run_test() ->
    Code = "def id a -> a",
    RunAsserts = fun(Mod) ->
                         ?assertEqual(2, Mod:id(2)),
                         ?assertEqual(1.3, Mod:id(1.3)),
                         ?assertEqual("string", Mod:id("string")),
                         ?assertEqual(atom, Mod:id(atom))
                 end,
    run(Code, RunAsserts).

function_call_test() ->
    Code = 
        "def id a -> a\n"
        "def callId b -> b.id",
    RunAsserts = fun(Mod) -> ?assertEqual(2, Mod:callId(2)) end,
    run(Code, RunAsserts).

function_def_newline_test() ->
    Code = 
        "def id b ->\n"
        "    b",
    RunAsserts = fun(Mod) -> ?assertEqual(2, Mod:id(2)) end,
    run(Code, RunAsserts).

function_call_multiple_args_test() ->
    Code = 
        "def firstId a b c -> a\n"
        "def callId a b -> b.firstId(b, a)",
    RunAsserts = fun(Mod) -> ?assertEqual(3, Mod:callId(2, 3)) end,
    run(Code, RunAsserts).

always_true_test() ->
    Code = 
        "def alwaysTrue a -> True",
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
        "def test2 a -> a.match(False -> True | True -> False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:test2('False'))
                 end,
    run(Code, RunAsserts).

pattern_match_expr_syntax2_test() ->
    Code = 
        "def test3 a -> a.match(False -> True\n"
        "                       True -> False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('True', Mod:test3('False'))
                 end,
    run(Code, RunAsserts).

underscore_arg_test() ->
    Code = 
        "def blip _ _ c -> c",
    RunAsserts = fun(Mod) -> ?assertEqual(blop, Mod:blip(blip, blab, blop)) end,
    run(Code, RunAsserts).

anonymous_function1_test() ->
    TestFunction = fun('True') -> 'False' end,
    Code = 
        "def blip f -> f(True)",
    RunAsserts = fun(Mod) -> ?assertEqual('False', Mod:blip(TestFunction)) end,
    run(Code, RunAsserts).

anonymous_function2_test() ->
    Code = 
        "def blip a f -> f(a)\n"
        "def blap a -> a.blip(False -> True\n"
        "                     True -> False)",
    RunAsserts = fun(Mod) -> ?assertEqual('False', Mod:blap('True')) end,
    run(Code, RunAsserts).

anonymous_function3_test() ->
    Code = 
        "def blip a f -> f(a)\n"
        "def blap a -> a.blip(arg -> arg)",
    RunAsserts = fun(Mod) -> ?assertEqual(whatevs, Mod:blap(whatevs)) end,
    run(Code, RunAsserts).

anonymous_function4_test() ->
    Code = 
        "def blip a f -> f(a, a)\n"
        "def blap a -> a.blip(arg1 False -> False\n"
        "                     arg1 True -> arg1)",
    RunAsserts = fun(Mod) -> ?assertEqual('True', Mod:blap('True')) end,
    run(Code, RunAsserts).

multiple_anonymous_functions1_test() ->
    Code = 
        "def blip a f g -> f(g(a))\n"
        "def blap a -> a.blip(_ -> False,\n"
        "                     False -> True\n"
        "                     True -> False)",
    RunAsserts = fun(Mod) -> ?assertEqual('False', Mod:blap('True')) end,
    run(Code, RunAsserts).

multiple_anonymous_functions2_test() ->
    Code = 
        "def blip a f g -> f(g(a))\n"
        "def blap a -> a.blip(True -> False\n"
        "                     False -> True,\n"
        "                     _ -> False)",
    RunAsserts = fun(Mod) -> ?assertEqual('True', Mod:blap('True')) end,
    run(Code, RunAsserts).

erlang_module_call_test() ->
    Code = "def get_last l -> l.erlang/lists/last",
    RunAsserts = fun(Mod) -> ?assertEqual('last_item', Mod:get_last(['first_item', 'last_item'])) end,
    run(Code, RunAsserts).

erlang_module_call_erlang_test() ->
    Code = "def test a -> a.erlang/atom_to_list",
    RunAsserts = fun(Mod) -> ?assertEqual("an_atom", Mod:test('an_atom')) end,
    run(Code, RunAsserts).

erlang_module_call_no_dot_notation_test() ->
    Code = "def test a -> erlang/atom_to_list(a)",
    RunAsserts = fun(Mod) -> ?assertEqual("another_atom", Mod:test('another_atom')) end,
    run(Code, RunAsserts).

shadow_variable_test() ->
    Code = 
        "def test a\n"
        " | a -> a",
    RunAsserts = fun(Mod) -> ?assertEqual(test, Mod:test(test)) end,
    run(Code, RunAsserts).

-endif.
