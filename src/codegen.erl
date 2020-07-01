-module(codegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").

gen({Module, AST}) ->
    io:format("---> AST: ~p~n", [AST]),
    Compiled = [gen_expr(D) || D <- AST],
    CompiledExports = [cerl:c_fname(Name, length(Args)) || {def, _, Name, Args, _} <- AST],
    {ok, cerl:c_module(cerl:c_atom(Module), CompiledExports, [], Compiled)}.

gen_expr({def, _, Name, Args, Expr}) ->
    FName = cerl:c_fname(Name, length(Args)),
    CompiledArgs = [gen_expr(A) || A <- Args],
    CompiledBody = gen_expr({Args, Expr}),
    {FName, cerl:c_fun(CompiledArgs, CompiledBody)};

gen_expr({type_def, _, Name, _, Body}) ->
    io:format("~nCompiling type ~s with body ~p ~n", [Name, Body]),
    FName = cerl:c_fname(Name, 0),
    CompiledBody = cerl:c_atom(Name),
    {FName, cerl:c_fun([], CompiledBody)};

gen_expr({variable, _, _, Tag}) -> cerl:c_var(Tag);
gen_expr({type, _, _, Symbols}) -> 
    Tag = list_to_atom(lists:flatten([atom_to_list(A) || A <- lists:join('/', Symbols)])),
    cerl:c_atom(Tag);

gen_expr({qualified_variable, _, Parts, S}) -> 
    Symbols = [S || {_, S} <- lists:reverse([S | lists:reverse(Parts)])],
    Tag = list_to_atom(lists:flatten([atom_to_list(A) || {_, A} <- lists:join('/', Symbols)])),
    cerl:c_atom(Tag);

gen_expr({application, _, Symbol, Args}) ->
    CompiledArgs = [gen_expr(E) || E <- Args],
    gen_apply(Symbol, CompiledArgs);

gen_expr({Args, Clauses}) when is_list(Clauses) -> gen_pattern_match(Args, Clauses);
gen_expr({_, Expr}) -> gen_expr(Expr);

gen_expr({lambda, Ctx, Clauses}) ->
    [{clause, _, Patterns, _} | _Rest] = Clauses,
    Symbols = [symbol:id(['']) || _ <- Patterns],
    Args = [{variable, Ctx, S, S} || S <- Symbols],
    CompiledBody = gen_pattern_match(Args, Clauses),
    CompiledArgs = [gen_expr(A) || A <- Args],
    cerl:c_fun(CompiledArgs, CompiledBody);

gen_expr({tuple, _, []}) -> cerl:c_atom('()');

% This makes is so we can use parenthesis to group evaluation like `(1 + 3).match( ... )`
% However, currently it only evaluates the last element of a tuple.
% In the future we'd like for it to evaluate all statements in a tuple when it's evaluated as an expression
% TODO: Don't just discard everything but the last element. That's a silly thing to do.
gen_expr({tuple, _, Expressions}) -> lists:last([gen_expr(Expr) || Expr <- Expressions]).

gen_apply({qualified_variable, _, Parts, [{_, S}]}, CompiledArgs) ->
    Symbols = [S || {_, S} <- Parts],
    case Symbols of
        ['erlang', Module] -> cerl:c_call(cerl:c_atom(Module), cerl:c_atom(S), CompiledArgs);
        ['erlang']         -> cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom(S), CompiledArgs)
    end;

gen_apply({variable, _, _, Tag}, CompiledArgs) ->
    cerl:c_apply(cerl:c_var(Tag), CompiledArgs).

gen_pattern_match(Args, Clauses) ->
    CompiledClauses = [gen_clause(Patterns, Expr) || {clause, _, Patterns, Expr} <- Clauses],
    CompiledArgs = cerl:c_values([gen_expr(A) || A <- Args]),
    cerl:c_case(CompiledArgs, CompiledClauses). 

gen_clause(Patterns, Expr) ->
    CompiledPatterns = [gen_expr(P) || P <- Patterns],
    cerl:c_clause(CompiledPatterns, gen_expr(Expr)).

gen_type_fun(Values) ->
    Instances = [cerl:c_map_pair(cerl:c_atom(S), cerl:c_atom(true)) ||{type, _, S, _} <- Values],
    cerl:c_map(Instances).

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
        "type Boolean -> True | False\n"
        "def alwaysTrue a -> Boolean/True",
    RunAsserts = fun(Mod) -> ?assertEqual('Boolean/True', Mod:alwaysTrue(2)) end,
    run(Code, RunAsserts).

pattern_match_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def rexor a\n"
        " | Boolean/True -> Boolean/False\n"
        " | Boolean/False -> Boolean/True",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('Boolean/True', Mod:rexor('Boolean/False')),
                         ?assertEqual('Boolean/False', Mod:rexor('Boolean/True'))
                 end,
    run(Code, RunAsserts).

pattern_match_multivariate_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def rexor a b\n"
        " | Boolean/True Boolean/False -> Boolean/True\n"
        " | Boolean/False Boolean/True -> Boolean/True\n"
        " | _ _ -> Boolean/False",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('Boolean/True', Mod:rexor('Boolean/True', 'Boolean/False')),
                         ?assertEqual('Boolean/False', Mod:rexor('Boolean/True', 'Boolean/True'))
                 end,
    run(Code, RunAsserts).

pattern_match_expr_syntax1_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def match a f -> f(a)\n"
        "def test2 a -> a.match(Boolean/False -> Boolean/True, Boolean/True -> Boolean/False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('Boolean/True', Mod:test2('Boolean/False'))
                 end,
    run(Code, RunAsserts).

pattern_match_expr_syntax2_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def match a f -> f(a)\n"
        "def test3 a -> a.match(Boolean/False -> Boolean/True\n"
        "                       Boolean/True -> Boolean/False)",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('Boolean/True', Mod:test3('Boolean/False'))
                 end,
    run(Code, RunAsserts).


pattern_match3_test() ->
    Code = 
        "def blah a\n"
        " | b -> b",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('Boolean/True', Mod:blah('Boolean/True'))
                 end,
    run(Code, RunAsserts).

underscore_arg_test() ->
    Code = 
        "def blip _ _ c -> c",
    RunAsserts = fun(Mod) -> ?assertEqual(blop, Mod:blip(blip, blab, blop)) end,
    run(Code, RunAsserts).

anonymous_function1_test() ->
    TestFunction = fun('Boolean/True') -> 'Boolean/False' end,
    Code = 
        "type Boolean -> True | False\n"
        "def blip f -> f(Boolean/True)",
    RunAsserts = fun(Mod) -> ?assertEqual('Boolean/False', Mod:blip(TestFunction)) end,
    run(Code, RunAsserts).

anonymous_function2_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def blip a f -> f(a)\n"
        "def blap a -> a.blip(Boolean/False -> Boolean/True\n"
        "                      Boolean/True -> Boolean/False)",
    RunAsserts = fun(Mod) -> ?assertEqual('Boolean/False', Mod:blap('Boolean/True')) end,
    run(Code, RunAsserts).

anonymous_function3_test() ->
    Code = 
        "def blip a f -> f(a)\n"
        "def blap a -> a.blip((arg -> arg))",
    RunAsserts = fun(Mod) -> ?assertEqual(whatevs, Mod:blap(whatevs)) end,
    run(Code, RunAsserts).

anonymous_function4_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def blip a f -> f(a, a)\n"
        "def blap a -> a.blip(arg1 Boolean/False -> Boolean/False\n"
        "                     arg1 Boolean/True  -> arg1)",
    RunAsserts = fun(Mod) -> ?assertEqual('Boolean/True', Mod:blap('Boolean/True')) end,
    run(Code, RunAsserts).

multiple_anonymous_functions1_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def blip a f g -> f(g(a))\n"
        "def blap a -> a.blip((_ -> Boolean/False),\n"
        "                     (Boolean/False -> Boolean/True\n"
        "                      Boolean/True -> Boolean/False))",
    RunAsserts = fun(Mod) -> ?assertEqual('Boolean/False', Mod:blap('Boolean/True')) end,
    run(Code, RunAsserts).

multiple_anonymous_functions2_test() ->
    Code = 
        "type Boolean -> True | False\n"
        "def blip a f g -> f(g(a))\n"
        "def blap a -> a.blip((Boolean/True -> Boolean/False\n"
        "                      Boolean/False -> Boolean/True),\n"
        "                     (_ -> Boolean/False))",
    RunAsserts = fun(Mod) -> ?assertEqual('Boolean/True', Mod:blap('whatevs')) end,
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
        " | b -> b",
    RunAsserts = fun(Mod) -> ?assertEqual(test, Mod:test(test)) end,
    run(Code, RunAsserts).

-endif.
