-module(linearize_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

linearize(Code, Name) -> linearize(Code, Name, [source, test_code], #{}).
linearize(Code, Name, Env) when is_map(Env) -> linearize(Code, Name, [source, test_code], Env);
linearize(Code, Name, ModulePath) when is_list(ModulePath) -> linearize(Code, Name, ModulePath, #{}).
linearize(Code, Name, ModulePath, Env) ->
    case parser:parse([{text, test_code, Code}], #{include_lark_libraries => false}) of
        {error, Errs} -> {error, Errs};
        {ok, Modules} ->
            ModuleMap = maps:from_list([{module:path(M), M} || M <- Modules]),
            {module, _, _, _, _, Defs} = maps:get(ModulePath, ModuleMap),
            Term = maps:get(Name, Defs),
            linearize:raw_term(Term, ModuleMap, Env)
    end.

tree(Def, Args) -> error:flatmap(Def, fun({_, {def, _, _, {'fun', _, F}} = Term}) ->
                                              error:map(F(Args, []), fun({_, Tree}) -> setelement(4, Term, Tree) end);
                                         ({_, Tree}) -> {ok, Tree}
                                      end).

env(Def, Args) -> error:flatmap(Def, fun({_, {def, _, _, {'fun', _, F}}}) ->
                                              error:map(F(Args, []), fun({Env, _}) -> Env end);
                                        ({Env, _}) -> {ok, Env}
                                      end).
domain(Term, Args) -> error:map(tree(Term, Args), fun({def, _, _, Expr}) ->
                                                          maps:get(domain, symbol:ctx(Expr))
                                                      end).

identity_function_test_() ->
    Code = "def id a -> a",
    Res = linearize(Code, id),
    [?test({ok, {def, _, id,
                 {'fun', _, [{clause, _,
                              [{variable, _, a, A}],
                              {variable, _, a, A}}]}}}, tree(Res, [any])),
     ?test({ok, {def, _, id,
                 {'fun', _, [{clause, _,
                              [{value, _, _, 5}],
                              {value, _, _, 5}}]}}}, tree(Res, [5])),
     ?test({ok, [any, 4]}, domain(Res, [[any, 4]]))].

dynamic_function_test_() ->
    Code = "def f a -> (val g = (fn {key1: b} -> b)
                        g({key1: 6})
                        g(a))",
    Res = linearize(Code, f),
    [?test({ok, {def, _, f,
                 {'fun', _, [{clause, _,
                              [{variable, _, a, A}],
                              {'let', _, 
                               {'fun', #{domain := {sum, [3, 4, 6]}},
                                [{clause, _,
                                  [{dict, _, [{pair, _,
                                               {keyword, _, key1},
                                               {variable, _, b, B}}]}],
                                  {variable, _, b, B}}]},
                               [{clause, _, [{variable, _, g, G}],
                                 {seq, _,
                                  {application, _, {variable, _, g, G}, [_]},
                                  {application, _, {variable, _, g, G}, [{variable, _, a, A}]}}}]}}]}}},
           tree(Res, [#{key1 => {sum, [3,4]}}])),
    ?test({ok, 4}, domain(Res, [#{key1 => 4}])),
    ?testError({no_intersection_between_clauses_and_argdomains, [5]}, tree(Res, [5]))].

static_function_test_() ->
    Code = "def f a -> (g(4)
                        g(a))
            def g a -> {key: a}",
    Res = linearize(Code, f),
    [?test({ok, {def, _, f,
                 {'fun', _, [{clause, _, [{variable, _, a, A}],
                              {seq, _,
                               {qualified_application, _, [source, test_code], g, [{value, _, integer, 4}]},
                               {qualified_application, _, [source, test_code], g, [{variable, _, a, A}]}}}]}}},
          tree(Res, [{sum, [a, b]}])),
    ?test({ok, #{{[source, test_code, g], [4]} := {'fun', _, [{clause, _, [{value, _, _, 4}],
                                                                {dict, _, [{pair, _,
                                                                            {keyword, _, key},
                                                                            {value, _, _, 4}}]}}]}}},
           env(Res, [{sum, [a, b]}])),
    ?test({ok, #{{[source, test_code, g], [{sum, [a, b]}]} := {'fun', _, [{clause, _, [{variable, _, a, A}],
                                                                            {dict, _, [{pair, _,
                                                                                        {keyword, _, key},
                                                                                        {variable, _, a, A}}]}}]}}},
           env(Res, [{sum, [a, b]}])),
    ?test({ok, #{key := {sum, [a, b]}}}, domain(Res, [{sum, [a, b]}]))].

sum_clause_test_() ->
    Code = "def f (a: 1 | 2) (b: 3 | 4) -> a | b,
                  _ _ -> 0",
    Res = linearize(Code, f),
    [?test({ok, {def, _, f,
                 {'fun', _, [{clause, _, [{value, _, _, 1}, {value, _, _, 3}],
                              {sum, _, [{value, _, _, 1}, {value, _, _, 3}]}},
                             {clause, _, [{value, _, _, 1}, {value, _, _, 4}],
                              {sum, _, [{value, _, _, 1}, {value, _, _, 4}]}},
                             {clause, _, [{value, _, _, 2}, {value, _, _, 3}],
                              {sum, _, [{value, _, _, 2}, {value, _, _, 3}]}},
                             {clause, _, [{value, _, _, 2}, {value, _, _, 4}],
                              {sum, _, [{value, _, _, 2}, {value, _, _, 4}]}},
                             {clause, _, [{variable, _, '_', _}, {variable, _, '_', _}],
                              {value, _, _, 0}}]}}},
           tree(Res, [any, any])),
     ?test({ok, {sum, [0, 1, 2, 3, 4]}}, domain(Res, [any, any])),
     ?test({ok, {def, _, f,
                 {'fun', _, [{clause, _, [{value, _, _, 2}, {value, _, _, 3}],
                              {sum, _, [{value, _, _, 2}, {value, _, _, 3}]}}]}}},
           tree(Res, [2, 3]))].

clause_subset_test_() ->
    Code = "def f 1 2                   -> 3,
                  (1 | 3) (2 | 4 | 5)   -> 7,
                  1 4                   -> 5,
                  3 2                   -> 6",
    Res = linearize(Code, f),
    [?test({ok, {def, _, f,
                 {'fun', _, [{clause, _, [{value, _, _, 1}, {value, _, _, 2}],
                              {value, _, _, 3}}]}}},
           tree(Res, [1, 2])),
     ?testError({arguments_not_subset_of_clauses, [any, any], _}, tree(Res, [any, any]))].

pattern_sum_arg_test_() ->
    Code = "def a -> (T: Q)
            import a/T
            def t (T: v) -> v,
                  {a: 5} -> 5,
                  [6, 7] -> 8,
                  _      -> 99",
    Res = linearize(Code, t),
    Tag = [source, test_code, a, 'T'],
    [?test({ok, 4}, domain(Res, [{tagged, Tag, 4}])),
     ?test({ok, {sum, [4, 44, 99]}}, domain(Res, [{sum, ordsets:from_list([{tagged, Tag, 4},
                                                                           {tagged, Tag, 44},
                                                                           {tagged, [t, 'T'], 6},
                                                                           6])}])),
     ?test({ok, 5}, domain(Res, [#{a => 5}])),
     ?test({ok, {sum, [5, 99]}}, domain(Res, [{sum, ordsets:from_list([#{a => 5},
                                                                       #{b => 6},
                                                                       7])}])),
     ?test({ok, 8}, domain(Res, [[6, 7]])),
     ?test({ok, {sum, [8, 99]}}, domain(Res, [{sum, ordsets:from_list([[4, 5],
                                                                       [6, 7],
                                                                       8])}]))].

pattern_application_test_() ->
    Code = "def d a -> {b: a}
            def t (a: d(T)) -> a",
    Res = linearize(Code, t),
    [?test({ok, {def, _, t,
                 {'fun', _, [{clause, _,
                              [{dict, _, [{pair, _, {keyword, _, b}, {value, _, atom, 'source/test_code/t/T'}}]}],
                              {dict, _,
                               [{pair, _,
                                 {keyword, _, b},
                                 {value, _, atom, 'source/test_code/t/T'}}]}}]}}}, 
           tree(Res, [#{b => 'source/test_code/t/T'}]))].


pattern_application_sum_test_() ->
    Code = "def d -> X | Y
            def t (a: d) -> a",
    Res = linearize(Code, t),
    [?test({ok, {def, _, t,
                 {'fun', _, [{clause, _, [{value, _, atom, 'source/test_code/d/X'}],
                              {value, _, atom, 'source/test_code/d/X'}},
                             {clause, _, [{value, _, atom, 'source/test_code/d/Y'}],
                              {value, _, atom, 'source/test_code/d/Y'}}]}}},
           tree(Res, [{sum, ['source/test_code/d/X', 'source/test_code/d/Y']}]))].

qualified_symbol_test_() ->
    Code = "def d -> X | Y
            def t -> (val f = d
                      val q = (fn f -> d/X)
                      q(d/Y))",
    Res = linearize(Code, t),
    [?test({ok, {def, _, t,
                 {'let', _,
                  {sum, _, [{value, _, atom, 'source/test_code/d/X'},
                            {value, _, atom, 'source/test_code/d/Y'}]},
                  [{clause, _, [{variable, _, f, _F}],
                    {'let', _,
                     {'fun', _, [{clause, _, [{value, _, atom, 'source/test_code/d/Y'}],
                                  {value, _, atom, 'source/test_code/d/X'}}]},
                     [{clause, _, [{variable, _, q, Q}],
                       {application, _, {variable, _, q, Q},
                        [{value, _, atom, 'source/test_code/d/Y'}]}}]}}]}}},
           tree(Res, [])),
     ?test({ok, 'source/test_code/d/X'}, domain(Res, []))].

beam_application_test_() ->
    Code = "import beam/lists/append
            def t a b c -> (val f = append
                            val q = (fn f(a, append(b, c)) -> f)
                            q(append([a, b, c]))(a, b))",
    Res = linearize(Code, t),
    [?test({ok, {def, _, t,
                 {'fun', _, [{clause, _, [_, _, _],
                              {'let', _,
                               {'fun', _, [{clause, _, [_, _],
                                            {beam_application, _, [lists], append, [_, _]}}]},
                               [{clause, _, [{variable, _, f, F}],
                                 {'let', _,
                                  {'fun', _, [{clause, _, [{list, _, [{value, _, integer, 1},
                                                                      {value, _, integer, 2},
                                                                      {value, _, integer, 3}]}],
                                               {variable, _, f, F}}]},
                                  [{clause, _, [{variable, _, q, Q}],
                                    {application, _,
                                     {application, _, {variable, _, q, Q},
                                      [{beam_application, _, [lists], append, [{list, _, [_, _, _]}]}]},
                                     [_, _]}}]}}]}}]}}},
           tree(Res, [[1], [2], [3]])),
    ?test({ok, [1, 2]}, domain(Res, [[1], [2], [3]]))].

qualified_pattern_symbol_test_() ->
    Code = "module test (export {boolean}
                         def boolean -> True | False)
            module test2 (import test
                          def t test/boolean -> test/boolean/True)",
    Res = linearize(Code, t, [test2]),
    [?test({ok, 'test/boolean/True'}, domain(Res, ['test/boolean/False']))].

beam_symbol_in_pattern_test_() ->
    Code = "import beam/rand
            def t rand/uniform -> 0",
    Res = linearize(Code, t),
    [?testError({unapplied_beam_function_in_pattern, 'beam/rand/uniform'}, tree(Res, [1]))].

beam_symbol_test_() ->
    Code = "import beam/erlang/date
            def t -> date",
    Res = linearize(Code, t),
    [?test({ok, {def, _, t,
                 {beam_application, _, [erlang], date, []}}}, tree(Res, [])),
     ?test({ok, any}, domain(Res, []))].

% If this test starts failing, but only while the erlang debugger is also
% running, then have a look at the comment in this commit on line 79 of
% src/utils.erl detailing what might be causing the issue.
ambigious_fun_test_() ->
    Code = "import beam/rand/uniform
            def match a f -> f(a)
            def t -> (val f = uniform(2).match(1 -> (fn _ -> One),
                                               _ -> (fn _ -> Two))
                      f('_'))",
    Res = linearize(Code, t),
    [?test({ok, {def, _, t,
                 {'let', _,
                  {qualified_application, _, [source, test_code], match,
                   [{beam_application, _, [rand], uniform, [{value, _, integer, 2}]},
                    {'fun', _, [{clause, _, [{value, _, integer, 1}],
                                 {'fun', _, [{clause, _, [{value, _, atom, '_'}],
                                              {value, _, atom, 'source/test_code/t/One'}}]}},
                                {clause, _, [{variable, _, '_', _}],
                                 {'fun', _, [{clause, _, [{value, _, atom, '_'}],
                                              {value, _, atom, 'source/test_code/t/Two'}}]}}]}]},
                  [{clause, _, [{variable, _, f, F}],
                    {application, _, {variable, _, f, F}, [{value, _, atom, '_'}]}}]}}},
           tree(Res, [])),
    ?test({ok, {sum, ['source/test_code/t/One', 'source/test_code/t/Two']}}, domain(Res, []))].

wrong_arity_test_() ->
    Code = "def f a -> 1
            def t -> f(2, 3)",
    Res = linearize(Code, t),
    [?testError({wrong_function_arity, 1, 2}, tree(Res, []))].

variable_arity_test_() ->
    Code = "def t 1 2 -> 2,
                  1   -> 1",
    Res = linearize(Code, t),
    [?testError({variable_clause_arity, [2, 1]}, tree(Res, [1, 2]))].

pattern_pair_sum_type_test_() ->
    Code = "def boolean -> True | False
            def t (b: boolean) -> b",
    Res = linearize(Code, t),
    [?test({ok, 'source/test_code/boolean/True'}, domain(Res, ['source/test_code/boolean/True'])),
     ?test({ok, 'source/test_code/boolean/False'}, domain(Res, ['source/test_code/boolean/False'])),
     ?testError({no_intersection_between_clauses_and_argdomains, [otherwise]}, domain(Res, ['otherwise']))].

let_pattern_sum_type_test_() ->
    Code = "def boolean -> True | False
            def t -> (val (b: boolean) = boolean()
                      b)",
    T = 'source/test_code/boolean/True',
    F = 'source/test_code/boolean/False',
    Res = linearize(Code, t),
    [?test({ok, {def, _, t,
                 {'let', _, {sum, _, [{value, _, _, T}, {value, _, _, F}]},
                  [{clause, _, [{value, _, _, T}], {value, _, _, T}},
                   {clause, _, [{value, _, _, F}], {value, _, _, F}}]}}},
           tree(Res, []))].

pair_pattern_unpack_test_() ->
    Code = "def f -> [1, 2]
            def t ([a, b]: f) -> [b, a]",
    Res = linearize(Code, t),
    [?test({ok, [2, 1]}, domain(Res, [[1, 2]]))].

function_domain_expected_test_() ->
    Code = "def t -> 1(2)",
    Res = linearize(Code, t),
    [?testError({function_domain_expected, 1}, tree(Res, []))].

env_test_() ->
    Code = "module a (export {b}; def b a -> 'something else')
            def f -> a/b(4)",
    Env = #{{[a, b], [4]} => {value, #{domain => world}, atom, hello}},
    Res = linearize(Code, f, Env),
    [?test({ok, world}, domain(Res, [])),
     ?test({ok, {def, _, f, {value, _, atom, hello}}}, tree(Res, [])),
     ?test({ok, Env}, env(Res, []))].

tuple_test_() ->
     Code = "def t #(a, b) -> #(b, a)",
     Res = linearize(Code, t),
     [?test({ok, {b, a}}, domain(Res, [{a, b}])),
      ?test({ok, {def, _, t, {'fun', _,
                              [{clause, _,
                                [{tuple, _, [{variable, _, a, A}, {variable, _, b, B}]}],
                                {tuple, _, [{variable, _, b, B}, {variable, _, a, A}]}}]}}},
            tree(Res, [whatever]))].
