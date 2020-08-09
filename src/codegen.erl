-module(codegen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").

gen({ast, _, Modules, _, Defs}) ->
    io:format("---> Defs: ~p~n", [Defs]),
    Compiled = [gen_expr(D) || D <- maps:values(Defs)],
    [gen_module(M, Defs, Compiled) || M <- Modules].

gen_module({module, _, Path, Exports}, Defs, Compiled) ->
    CompiledExports = [cerl:c_fname(Name, length(Args)) || {def, _, Name, Args, _} <- maps:values(Defs), 
                                                           maps:is_key(Name, Exports)],
    ModuleName = module:beam_name(Path),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), CompiledExports, [], Compiled)}.



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

gen_expr({qualified_type, _, ModulePath, Name}) ->
    ModuleName = module:beam_name(ModulePath),
    gen_expr(ModuleName:Name());

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

% When we evaluate a type member, we call the corresponding type module and run
% `gen_expr` on the result. If the type member is an atom (like `Boolean/True`)
% then we generate code for it here
gen_expr(Atom) when is_atom(Atom) -> cerl:c_atom(Atom);

% This makes is so we can use parenthesis to group evaluation like `(1 + 3).match( ... )`
% However, currently it only evaluates the last element of a tuple.
% In the future we'd like for it to evaluate all statements in a tuple when it's evaluated as an expression
% TODO: Don't just discard everything but the last element. That's a silly thing to do.
gen_expr({tuple, _, Expressions}) -> lists:last([gen_expr(Expr) || Expr <- Expressions]).

gen_apply({qualified_variable, _, ModulePath, Name}, CompiledArgs) ->
    ModuleName = module:beam_name(ModulePath),
    cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), CompiledArgs);

gen_apply({variable, _, _, Tag}, CompiledArgs) ->
    cerl:c_apply(cerl:c_var(Tag), CompiledArgs).

gen_pattern_match(Args, Clauses) ->
    CompiledClauses = [gen_clause(Patterns, Expr) || {clause, _, Patterns, Expr} <- Clauses],
    CompiledArgs = cerl:c_values([gen_expr(A) || A <- Args]),
    cerl:c_case(CompiledArgs, CompiledClauses). 

gen_clause(Patterns, Expr) ->
    CompiledPatterns = [gen_expr(P) || P <- Patterns],
    cerl:c_clause(CompiledPatterns, gen_expr(Expr)).

-ifdef(TEST).

run(Code, RunAsserts) ->
    {ok, Results} = kind:load(Code),
    RunAsserts(),
    [clean(R) || R <- Results].

clean(Module) -> true = code:soft_purge(Module),
                 true = code:delete(Module).

identity_run_test() ->
    Code =
        "module kind/test { id }\n"
        "def id a -> a",
    RunAsserts = fun() ->
                         ?assertEqual(2, kind_test:id(2)),
                         ?assertEqual(1.3, kind_test:id(1.3)),
                         ?assertEqual("string", kind_test:id("string")),
                         ?assertEqual(atom, kind_test:id(atom))
                 end,
    run(Code, RunAsserts).

function_call_test() ->
    Code = 
        "module kind/test { callId }\n"
        "def id a -> a\n"
        "def callId b -> b.id",
    RunAsserts = fun() -> ?assertEqual(2, kind_test:callId(2)) end,
    run(Code, RunAsserts).

function_def_newline_test() ->
    Code = 
        "module kind/test { id }\n"
        "def id b ->\n"
        "    b",
    RunAsserts = fun() -> ?assertEqual(2, kind_test:id(2)) end,
    run(Code, RunAsserts).

function_call_multiple_args_test() ->
    Code = 
        "module kind/test { callId }\n"
        "def firstId a b c -> a\n"
        "def callId a b -> b.firstId(b, a)",
    RunAsserts = fun() -> ?assertEqual(3, kind_test:callId(2, 3)) end,
    run(Code, RunAsserts).

always_true_test() ->
    Code = 
        "module kind/test { alwaysTrue }\n"
        "def alwaysTrue a -> Boolean/True",
    RunAsserts = fun() -> ?assertEqual('Boolean/True', kind_test:alwaysTrue(2)) end,
    run(Code, RunAsserts).

pattern_match_test() ->
    Code =
        "module kind/test { rexor }\n"
        "def rexor a\n"
        " | Boolean/True -> Boolean/False\n"
        " | Boolean/False -> Boolean/True",
    RunAsserts = fun() ->
                         ?assertEqual('Boolean/True', kind_test:rexor('Boolean/False')),
                         ?assertEqual('Boolean/False', kind_test:rexor('Boolean/True'))
                 end,
    run(Code, RunAsserts).

pattern_match_multivariate_test() ->
    Code =
        "module kind/test { rexor }\n"
        "def rexor a b\n"
        " | Boolean/True Boolean/False -> Boolean/True\n"
        " | Boolean/False Boolean/True -> Boolean/True\n"
        " | _ _ -> Boolean/False",
    RunAsserts = fun() ->
                         ?assertEqual('Boolean/True', kind_test:rexor('Boolean/True', 'Boolean/False')),
                         ?assertEqual('Boolean/False', kind_test:rexor('Boolean/True', 'Boolean/True'))
                 end,
    run(Code, RunAsserts).

pattern_match_expr_syntax1_test() ->
    Code =
        "module kind/test { test2 }\n"
        "def test2 a -> a.match(Boolean/False -> Boolean/True, Boolean/True -> Boolean/False)",
    RunAsserts = fun() ->
                         ?assertEqual('Boolean/True', kind_test:test2('Boolean/False'))
                 end,
    run(Code, RunAsserts).

pattern_match_expr_syntax2_test() ->
    Code =
        "module kind/test { test3 }\n"
        "def test3 a -> a.match(Boolean/False -> Boolean/True\n"
        "                       Boolean/True -> Boolean/False)",
    RunAsserts = fun() ->
                         ?assertEqual('Boolean/True', kind_test:test3('Boolean/False'))
                 end,
    run(Code, RunAsserts).


pattern_match3_test() ->
    Code =
        "module kind/test { blah }\n"
        "def blah a\n"
        " | b -> b",
    RunAsserts = fun() ->
                         ?assertEqual('Boolean/True', kind_test:blah('Boolean/True'))
                 end,
    run(Code, RunAsserts).

underscore_arg_test() ->
    Code =
        "module kind/test { blip }\n"
        "def blip _ _ c -> c",
    RunAsserts = fun() -> ?assertEqual(blop, kind_test:blip(blip, blab, blop)) end,
    run(Code, RunAsserts).

anonymous_function1_test() ->
    TestFunction = fun('Boolean/True') -> 'Boolean/False' end,
    Code =
        "module kind/test { blip }\n"
        "def blip f -> f(Boolean/True)",
    RunAsserts = fun() -> ?assertEqual('Boolean/False', kind_test:blip(TestFunction)) end,
    run(Code, RunAsserts).

anonymous_function2_test() ->
    Code =
        "module kind/test { blap }\n"
        "def blip a f -> f(a)\n"
        "def blap a -> a.blip(Boolean/False -> Boolean/True\n"
        "                      Boolean/True -> Boolean/False)",
    RunAsserts = fun() -> ?assertEqual('Boolean/False', kind_test:blap('Boolean/True')) end,
    run(Code, RunAsserts).

anonymous_function3_test() ->
    Code = 
        "module kind/test { blap }\n"
        "def blip a f -> f(a)\n"
        "def blap a -> a.blip((arg -> arg))",
    RunAsserts = fun() -> ?assertEqual(whatevs, kind_test:blap(whatevs)) end,
    run(Code, RunAsserts).

anonymous_function4_test() ->
    Code = 
        "module kind/test { blap }\n"
        "def blip a f -> f(a, a)\n"
        "def blap a -> a.blip(arg1 Boolean/False -> Boolean/False\n"
        "                     arg1 Boolean/True  -> arg1)",
    RunAsserts = fun() -> ?assertEqual('Boolean/True', kind_test:blap('Boolean/True')) end,
    run(Code, RunAsserts).

multiple_anonymous_functions1_test() ->
    Code = 
        "module kind/test { blap }\n"
        "def blip a f g -> f(g(a))\n"
        "def blap a -> a.blip((_ -> Boolean/False),\n"
        "                     (Boolean/False -> Boolean/True\n"
        "                      Boolean/True -> Boolean/False))",
    RunAsserts = fun() -> ?assertEqual('Boolean/False', kind_test:blap('Boolean/True')) end,
    run(Code, RunAsserts).

multiple_anonymous_functions2_test() ->
    Code = 
        "module kind/test { blap }\n"
        "def blip a f g -> f(g(a))\n"
        "def blap a -> a.blip((Boolean/True -> Boolean/False\n"
        "                      Boolean/False -> Boolean/True),\n"
        "                     (_ -> Boolean/False))",
    RunAsserts = fun() -> ?assertEqual('Boolean/True', kind_test:blap('whatevs')) end,
    run(Code, RunAsserts).

erlang_module_call_test() ->
    Code = "import lists\n"
           "module kind/test { get_last }\n"
           "def get_last l -> l.lists/last",
    RunAsserts = fun() -> ?assertEqual('last_item', kind_test:get_last(['first_item', 'last_item'])) end,
    run(Code, RunAsserts).

erlang_module_call_erlang_test() ->
    Code = "import erlang\n"
           "module kind/test { test }\n"
           "def test a -> a.erlang/atom_to_list",
    RunAsserts = fun() -> ?assertEqual("an_atom", kind_test:test('an_atom')) end,
    run(Code, RunAsserts).

erlang_module_call_no_dot_notation_test() ->
    Code = "import erlang\n"
           "module kind/test { test }\n"
           "def test a -> erlang/atom_to_list(a)",
    RunAsserts = fun() -> ?assertEqual("another_atom", kind_test:test('another_atom')) end,
    run(Code, RunAsserts).

shadow_variable_test() ->
    Code = 
        "module kind/test { test }\n"
        "def test a\n"
        " | b -> b",
    RunAsserts = fun() -> ?assertEqual(test, kind_test:test(test)) end,
    run(Code, RunAsserts).

-endif.
