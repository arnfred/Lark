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
    Code = "def f a -> (val g = (| {key1: b} -> b)
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



