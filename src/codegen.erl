-module(codegen).
-export([gen/3]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").


gen({ast, _, Modules, _, Defs}, DomainDef, Types) ->
    io:format("---> Defs: ~p~n", [Defs]),
    Compiled = [gen_expr(D) || D <- maps:values(Defs)],
    {ok, [gen_module(M, Defs, Compiled, Types, DomainDef) || M <- Modules]}.

gen_module({module, _, Path, Exports}, Defs, Compiled, Types, DomainDef) ->
    CompiledTypes = [DomainDef | Types],
    {TypeExports, _} = lists:unzip(Types), 
    CompiledExports = [cerl:c_fname(Name, length(Args)) || {def, _, Name, Args, _} <- maps:values(Defs), 
                                                           maps:is_key(Name, Exports)],
    ModuleExports = TypeExports ++ CompiledExports,
    ModuleName = module:beam_name(Path),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), ModuleExports, [], Compiled ++ CompiledTypes)}.



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

-define(setup(Code, Tests), {setup, fun() -> kind:load(Code) end, fun clean/1, Tests}).

clean({error, _}) -> noop;
clean({ok, Modules}) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].


identity_run_test_() ->
    {"define and call the identity function",
     ?setup("module kind/test { id }\n"
            "def id a -> a",
            fun({ok, _}) ->
                    [?test(2, kind_test:id(2)),
                     ?test(1.3, kind_test:id(1.3)),
                     ?test("string", kind_test:id("string")),
                     ?test(atom, kind_test:id(atom))]
            end)}.

function_call_test_() ->
        ?setup("module kind/test { callId }\n"
               "def id a -> a\n"
               "def callId b -> b.id",
               fun({ok, _}) -> ?test(2, kind_test:callId(2)) end).

function_def_newline_test_() ->
        ?setup("module kind/test { id }\n"
               "def id b ->\n"
               "    b",
               fun({ok, _}) -> ?test(2, kind_test:id(2)) end).

function_call_multiple_args_test_() ->
    ?setup("module kind/test { callId }\n"
           "def firstId a b c -> a\n"
           "def callId a b -> b.firstId(b, a)",
           fun({ok, _}) -> ?test(3, kind_test:callId(2, 3)) end).

always_true_test_() ->
    ?setup("module kind/test { alwaysTrue }\n"
           "def alwaysTrue a -> True",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:alwaysTrue(2)) end).

pattern_match_test_() ->
    ?setup("module kind/test { rexor }\n"
           "def rexor a\n"
           " | True -> False\n"
           " | False -> True",
           fun({ok, _}) ->
                   [?test('Boolean/True', kind_test:rexor('Boolean/False')),
                   ?test('Boolean/False', kind_test:rexor('Boolean/True'))]
           end).

pattern_match_multivariate_test_() ->
    ?setup("module kind/test { rexor }\n"
           "def rexor a b\n"
           " | True False -> True\n"
           " | False True -> True\n"
           " | _ _ -> False",
           fun({ok, _}) ->
                   [?test('Boolean/True', kind_test:rexor('Boolean/True', 'Boolean/False')),
                    ?test('Boolean/False', kind_test:rexor('Boolean/True', 'Boolean/True'))]
           end).

pattern_match_expr_syntax1_test_() ->
    ?setup("module kind/test { test2 }\n"
           "def test2 a -> a.match(False -> True, True -> False)",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:test2('Boolean/False'))
           end).

pattern_match_expr_syntax2_test_() ->
    ?setup("module kind/test { test3 }\n"
           "def test3 a -> a.match(False -> True\n"
           "                       True -> False)",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:test3('Boolean/False'))
           end).


pattern_match3_test_() ->
    ?setup("module kind/test { blah }\n"
           "def blah a\n"
           " | b -> b",
           fun({ok, _}) ->
                   ?test('Boolean/True', kind_test:blah('Boolean/True'))
           end).

underscore_arg_test_() ->
    ?setup("module kind/test { blip }\n"
           "def blip _ _ c -> c",
           fun({ok, _}) -> ?test(blop, kind_test:blip(blip, blab, blop)) end).

anonymous_function1_test_() ->
    TestFunction = fun('Boolean/True') -> 'Boolean/False' end,
    ?setup("module kind/test { blip }\n"
           "def blip f -> f(True)",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blip(TestFunction)) end).

anonymous_function2_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a)\n"
           "def blap a -> a.blip(False -> True\n"
           "                      True -> False)",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blap('Boolean/True')) end).

anonymous_function3_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a)\n"
           "def blap a -> a.blip((arg -> arg))",
           fun({ok, _}) -> ?test(whatevs, kind_test:blap(whatevs)) end).

anonymous_function4_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f -> f(a, a)\n"
           "def blap a -> a.blip(arg1 False -> False\n"
           "                     arg1 True  -> arg1)",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:blap('Boolean/True')) end).

multiple_anonymous_functions1_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f g -> f(g(a))\n"
           "def blap a -> a.blip((_ -> False),\n"
           "                     (False -> True\n"
           "                      True -> False))",
           fun({ok, _}) -> ?test('Boolean/False', kind_test:blap('Boolean/True')) end).

multiple_anonymous_functions2_test_() ->
    ?setup("module kind/test { blap }\n"
           "def blip a f g -> f(g(a))\n"
           "def blap a -> a.blip((True -> False\n"
           "                      False -> True),\n"
           "                     (_ -> False))",
           fun({ok, _}) -> ?test('Boolean/True', kind_test:blap('whatevs')) end).

erlang_module_call_test_() ->
    ?setup("module kind/test { get_last }\n"
           "import lists/last\n"
           "def get_last l -> l.last",
           fun({ok, _}) -> ?test('last_item', kind_test:get_last(['first_item', 'last_item'])) end).

erlang_module_call_erlang_test_() ->
    ?setup("module kind/test { test }\n"
           "import erlang/atom_to_list\n"
           "def test a -> a.atom_to_list",
           fun({ok, _}) -> ?test("an_atom", kind_test:test('an_atom')) end).

erlang_module_call_no_dot_notation_test_() ->
    ?setup("module kind/test { test }\n"
           "import erlang\n"
           "def test a -> erlang/atom_to_list(a)",
           fun({ok, _}) -> ?test("another_atom", kind_test:test('another_atom')) end).

shadow_variable_test_() ->
    {"Test how we handle variable reuse",
     ?setup("module kind/test { test }\n"
            "def test a\n"
            " | a -> a",
            fun(Error) -> 
                    [?testError({symbol_in_pattern_already_defined, a}, Error)]
            end)}.

top_level_type_import_test_() ->
    {"Test if we can use Boolean as defined in the prelude",
     ?setup("module kind/test { Test }\n"
            "type Test -> Boolean | Maybe",
            fun({ok, _}) -> 
                    ?testEqual({sum, ordsets:from_list(['Test/Maybe', 'Boolean/True', 'Boolean/False'])}, kind_test:'Test'())
            end)}.

sup_level_type_import_test_() ->
    {"Test if we can use Boolean as defined in the prelude",
     ?setup("module kind/test { Test }\n"
            "type Test -> True | Maybe",
            fun({ok, _}) -> 
                    ?testEqual({sum, ordsets:from_list(['Test/Maybe', 'Boolean/True'])}, kind_test:'Test'())
            end)}.



-endif.
