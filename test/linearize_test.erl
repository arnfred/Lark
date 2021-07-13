-module(linearize_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

linearize(Code, Name) ->
    case parser:parse([{text, test_code, Code}], #{include_kind_libraries => false}) of
        {error, Errs} -> {error, Errs};
        {ok, Modules} ->
            ModuleMap = maps:from_list([{module:path(M), M} || M <- Modules]),
            {module, _, _, _, _, Defs} = maps:get([source, test_code], ModuleMap),
            Term = maps:get(Name, Defs),
            linearize:term(Term, ModuleMap)
    end.

tree(Def, Args) -> error:flatmap(Def, fun({_, {def, _, _, F}}) ->
                                              error:map(F(Args, []), fun({_, Tree}) -> Tree end)
                                      end).

env(Def, Args) -> error:flatmap(Def, fun({_, {def, _, _, F}}) ->
                                              error:map(F(Args, []), fun({Env, _}) -> Env end)
                                      end).
domain(Term, Args) -> error:map(tree(Term, Args), fun(Tree) ->
                                                          maps:get(domain, symbol:ctx(Tree))
                                                      end).

identity_function_test_() ->
    Code = "def id a -> a",
    Res = linearize(Code, id),
    [?test({ok, {'fun', _, [{clause, _,
                             [{variable, _, a, A}],
                             {variable, _, a, A}}]}}, tree(Res, [any])),
     ?test({ok, {'fun', _, [{clause, _,
                             [{value, _, _, 5}],
                             {value, _, _, 5}}]}}, tree(Res, [5])),
     ?test({ok, [any, 4]}, domain(Res, [[any, 4]]))].

dynamic_function_test_() ->
    Code = "def f a -> (val g = (fn {key1: b} -> b)
                        g({key1: 6})
                        g(a))",
    Res = linearize(Code, f),
    [?test({ok, {'fun', _, [{clause, _,
                             [{variable, _, a, A}],
                             {'let', _, 
                              {variable, _, g, G},
                              {'fun', #{domain := {sum, [3, 4, 6]}},
                               [{clause, _,
                                 [{dict, _, [{pair, _,
                                              {keyword, _, key1},
                                              {variable, _, b, B}}]}],
                                 {variable, _, b, B}}]},
                              {seq, _,
                               {application, _, {variable, _, g, G}, [_]},
                               {application, _, {variable, _, g, G}, [{variable, _, a, A}]}}}}]}},
           tree(Res, [#{key1 => {sum, [3,4]}}])),
    ?test({ok, 4}, domain(Res, [#{key1 => 4}])),
    ?testError({no_intersection_between_clauses_and_argdomains, [5]}, tree(Res, [5]))].

static_function_test_() ->
    Code = "def f a -> (g(4)
                        g(a))
            def g a -> {key: a}",
    Res = linearize(Code, f),
    [?test({ok, {'fun', _, [{clause, _, [{variable, _, a, A}],
                             {seq, _,
                              {qualified_application, _, [source, test_code], g, [{value, _, integer, 4}]},
                              {qualified_application, _, [source, test_code], g, [{variable, _, a, A}]}}}]}},
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
    Code = "def f (a: 1 | 2) (b: 3 | 4) -> a | b
                  _ _ -> 0",
    Res = linearize(Code, f),
    [?test({ok, {'fun', _, [{clause, _, [{value, _, _, 1}, {value, _, _, 3}],
                             {sum, _, [{value, _, _, 1}, {value, _, _, 3}]}},
                            {clause, _, [{value, _, _, 1}, {value, _, _, 4}],
                             {sum, _, [{value, _, _, 1}, {value, _, _, 4}]}},
                            {clause, _, [{value, _, _, 2}, {value, _, _, 3}],
                             {sum, _, [{value, _, _, 2}, {value, _, _, 3}]}},
                            {clause, _, [{value, _, _, 2}, {value, _, _, 4}],
                             {sum, _, [{value, _, _, 2}, {value, _, _, 4}]}},
                            {clause, _, [{variable, _, '_', _}, {variable, _, '_', _}],
                             {value, _, _, 0}}]}},
           tree(Res, [any, any])),
     ?test({ok, {sum, [0, 1, 2, 3, 4]}}, domain(Res, [any, any])),
     ?test({ok, {'fun', _, [{clause, _, [{value, _, _, 2}, {value, _, _, 3}],
                             {sum, _, [{value, _, _, 2}, {value, _, _, 3}]}}]}},
           tree(Res, [2, 3]))].

clause_subset_test_() ->
    Code = "def f 1 2             -> 3
                  1 4             -> 5
                  3 2             -> 6
                  (1 | 3) (2 | 4) -> 7",
    Res = linearize(Code, f),
    [?test({ok, {'fun', _, [{clause, _, [{value, _, _, 1}, {value, _, _, 2}],
                             {value, _, _, 3}}]}},
           tree(Res, [1, 2])),
     ?testError({arguments_not_subset_of_clauses, [any, any], _}, tree(Res, [any, any]))].

pattern_sum_arg_test_() ->
    Code = "def t (T: a) -> a
                  {a: 5} -> 5
                  [6, 7] -> 8
                  _      -> 99",
    Res = linearize(Code, t),
    [?test({ok, 4}, domain(Res, [{tagged, [t, 'T'], 4}])),
     ?test({ok, {sum, [4, 44, 99]}}, domain(Res, [{sum, ordsets:from_list([{tagged, [t, 'T'], 4},
                                                                           {tagged, [t, 'T'], 44},
                                                                           {tagged, [s, 'T'], 6},
                                                                           6])}])),
     ?test({ok, 5}, domain(Res, [#{a => 5}])),
     ?test({ok, {sum, [5, 99]}}, domain(Res, [{sum, ordsets:from_list([#{a => 5},
                                                                       #{b => 6},
                                                                       7])}])),
     ?test({ok, 8}, domain(Res, [[6, 7]])),
     ?test({ok, {sum, [8, 99]}}, domain(Res, [{sum, ordsets:from_list([[4, 5],
                                                                       [6, 7],
                                                                       8])}]))].




