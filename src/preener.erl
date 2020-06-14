-module(preener).
-export([preen/1]).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

preen(ASTs) when is_list(ASTs) -> error:collect([preen(AST) || AST <- ASTs]);
preen(AST) -> case expand_tuples(AST) of
                  {error, Errs}         -> {error, Errs};
                  {ok, {_, DetupledAST}}  ->
                      case dict_keys(DetupledAST) of
                          {error, Errs}         -> {error, Errs};
                          {ok, {_, KeyedAst}}   -> {ok, KeyedAst}
                      end
              end.

expand_tuples(AST) -> ast:traverse(fun tuple_pre/3, fun tuple_post/3, AST).

tuple_pre(_, _, {type_def, _, _, _, _})   -> skip;
tuple_pre(_, _, Term)                     -> {ok, Term}.

tuple_post(expr, _, {tuple, _, Elems}) -> clean_tuple_elements(Elems);
tuple_post(_, _, _)                 -> skip.

clean_tuple_elements(Expressions) ->
    F = fun({val, Ctx, Pattern, Expr}, Acc) -> {'let', Ctx, Pattern, Expr, Acc};
           (T, Acc)                         -> {seq, ast:context(T), T, Acc} end,
    case lists:reverse(Expressions) of
        [{val, _, _, _} = T | _]    -> error:format({illegal_end_of_tuple, val}, {preener, T});
        [E | Elems]                 -> {ok, lists:foldr(F, E, lists:reverse(Elems))}
    end.

dict_keys(AST) -> ast:traverse(fun dict_pre/3, fun dict_post/3, AST).

dict_pre(Type, Scope, {dict, Ctx, Elems}) ->
    case error:collect([dict_elem(Type, Scope, E) || E <- Elems]) of
        {error, Errs} -> {error, Errs};
        {ok, TElems} -> {ok, {dict, Ctx, TElems}}
    end;
dict_pre(Type, Scope, {lookup, Ctx, Expr, Elems}) ->
    case error:collect([dict_elem(Type, Scope, E) || E <- Elems]) of
        {error, Errs} -> {error, Errs};
        {ok, TElems} -> {ok, {lookup, Ctx, Expr, TElems}}
    end;
dict_pre(_Type, _Scope, Term) -> {ok, Term}.
dict_post(_, _, _) -> skip.


dict_elem(expr, _Scope, {symbol, Ctx, variable, Name}) -> {ok, {key, Ctx, Name}};
dict_elem(pattern, _Scope, {symbol, _, variable, _} = Term) -> {ok, Term};
dict_elem(Type, Scope, {pair, Ctx, Key, Val}) ->
    error:map(dict_pair_elem(Type, Scope, Key), fun(K) -> {pair, Ctx, K, Val} end);
dict_elem(Type, _, Term) ->
    error:format({illegal_dict_element, ast:term_type(Term), Type}, {preener, Term}).

dict_pair_elem(_Type, _Scope, {symbol, Ctx, variable, Name}) -> {ok, {key, Ctx, Name}};
dict_pair_elem(Type, _, Term) ->
    error:format({illegal_dict_pair_element, ast:term_type(Term), Type}, {preener, Term}).

-ifdef(TEST).

do_preen(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
    {ok, Parsed} = parser:parse(Tokens),
    io:format("Parsed is ~p~n", [Parsed]),
    error:collect([preener:preen(P) || P <- Parsed]).

illegal_end_of_tuple_test_() ->
    Code = "def test a b -> (val a = b, val b = a)",
    ?_errorMatch({illegal_end_of_tuple, val}, do_preen(Code)).

unwrap_tuple_with_one_element_test_() ->
    Code = "def test b -> (b)",
    ?_assertMatch({ok, [{def, _, _, _,
                         {symbol, _, variable, b}}]}, do_preen(Code)).

wrap_tuple_elems_in_seq_test_() ->
    Code = "def test a b -> (a, a, b)",
    ?_assertMatch({ok, [{def, _, _, _,
                         {seq, _, {symbol, _, _, a},
                          {seq, _, {symbol, _, _, a},
                           {symbol, _, _, b}}}}]}, do_preen(Code)). 

translate_vals_to_let_statements_test_() ->
    Code = "def test a -> (val b = a, b, val c = a, c)",
    ?_assertMatch({ok, [{def, _, _, _,
                         {'let', _, {symbol, _, _, b}, {symbol, _, _, a},
                          {seq, _, {symbol, _, _, b},
                           {'let', _, {symbol, _, _, c}, {symbol, _, _, a},
                            {symbol, _, _, c}}}}}]}, do_preen(Code)).

dict_expr_test_() ->
    Code = "def test a -> {a: a, b: a}",
    ?_assertMatch({ok, [{def, _, _, _,
                         {dict, _,
                          [{pair, _, {key, _, a}, {symbol, _, _, a}},
                           {pair, _, {key, _, b}, {symbol, _, _, a}}]}}]}, do_preen(Code)).

dict_pattern_test_() ->
    Code = "def test a\n"
           " | {a: b, b: c} -> c",
    ?_assertMatch({ok, [{def, _, _, _,
                         [{clause, _,
                           [{dict, _,
                             [{pair, _, {key, _, a}, {symbol, _, _, b}},
                              {pair, _, {key, _, b}, {symbol, _, _, c}}]}],
                           {symbol, _, _, c}}]}]}, do_preen(Code)).

dict_pattern_variable_test_() ->
    Code = "def test a\n"
           " | {a: b, b} -> b",
    ?_assertMatch({ok, [{def, _, _, _,
                         [{clause, _,
                           [{dict, _,
                             [{pair, _, {key, _, a}, {symbol, _, _, b}},
                              {symbol, _, _, b}]}],
                           {symbol, _, _, b}}]}]}, do_preen(Code)).

dict_illegal_element_test_() ->
    Code = "def test a -> {{a}, {b}: b}",
    ?_errorMatch({illegal_dict_pair_element, dict, expr}, 
                 {illegal_dict_element, dict, expr}, do_preen(Code)).

lookup_expr_test_() ->
    Code = "def test a -> a {a: a, b}",
    ?_assertMatch({ok, [{def, _, _, _,
                         {lookup, _,
                          {symbol, _, _, a},
                          [{pair, _, {key, _, a}, {symbol, _, _, a}},
                           {key, _, b}]}}]}, do_preen(Code)).
lookup_pattern_test_() ->
    Code = "def test a\n"
           " | a {a: b, c} -> c",
    ?_assertMatch({ok, [{def, _, _, _,
                         [{clause, _,
                           [{lookup, _,
                             {symbol, _, _, a},
                             [{pair, _, {key, _, a}, {symbol, _, _, b}},
                              {symbol, _, _, c}]}],
                           {symbol, _, _, c}}]}]}, do_preen(Code)).

-endif.
