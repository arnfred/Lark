-module(preener).
-export([preen/2]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

preen(FileName, AST) -> 
    case expand_sums(AST) of
        {error, Errs}         -> {error, Errs};
        {ok, {_, DetupledAST}}  ->
            case dict_keys(DetupledAST) of
                {error, Errs}         -> {error, Errs};
                {ok, {_, KeyedAST}}   -> {ok, {_, PathAST}} = add_filename(FileName, KeyedAST),
                                         {ok, {_, TaggedAST}} = tagged_values(PathAST),
                                         {ok, TaggedAST}
            end
    end.

expand_sums(AST) -> ast:traverse(fun sum_post/3, AST).

sum_post(_, _, {'or', Ctx, {sum, _, E1}, {sum, _, E2}}) -> {ok, {sum, Ctx, E1 ++ E2}};
sum_post(_, _, {'or', Ctx, {sum, _, E1}, E2})           -> {ok, {sum, Ctx, E1 ++ [E2]}};
sum_post(_, _, {'or', Ctx, E1, {sum, _, E2}})           -> {ok, {sum, Ctx, [E1 | E2]}};
sum_post(_, _, {'or', Ctx, E1, E2})                     -> {ok, {sum, Ctx, [E1, E2]}};
sum_post(_, _, _)                                       -> ok.


dict_keys(AST) -> ast:traverse(fun dict_pre/3, fun dict_post/3, AST).

dict_pre(Type, Scope, {dict, Ctx, Elems}) ->
    case error:collect([dict_elem(Type, Scope, E) || E <- Elems]) of
        {error, Errs} -> {error, Errs};
        {ok, TElems} -> {ok, {dict, Ctx, TElems}}
    end;
dict_pre(_Type, _Scope, Term) -> {ok, Term}.
dict_post(_, _, _) -> ok.


dict_elem(expr, _Scope, {symbol, Ctx, variable, Name}) -> {ok, {keyword, Ctx, Name}};
dict_elem(pattern, _Scope, {symbol, _, variable, _} = Term) -> {ok, Term};
dict_elem(Type, Scope, {pair, Ctx, Key, Val}) ->
    error:map(dict_pair_elem(Type, Scope, Key), fun(K) -> {pair, Ctx, K, Val} end);
dict_elem(Type, _, Term) ->
    error:format({illegal_dict_element, ast:term_type(Term), Type}, {preener, Term}).

dict_pair_elem(_Type, _Scope, {symbol, Ctx, variable, Name}) -> {ok, {keyword, Ctx, Name}};
dict_pair_elem(Type, _, Term) ->
    error:format({illegal_dict_pair_element, ast:term_type(Term), Type}, {preener, Term}).

add_filename(FileName, AST) ->
    TagId = fun(_, _, Term) -> {ok, ast:tag(source_path, Term, FileName)} end,
    Skip = fun(_, _, _) -> ok end,
    ast:traverse(TagId, Skip, AST).

tagged_pre(top_level, _, {def, _, Name, _} = Term) -> 
    {ok, ast:tag(parent, Term, Name)};
tagged_pre(top_level, _, {macro, _, Name, _} = Term) -> 
    {ok, ast:tag(parent, Term, Name)};
tagged_pre(top_level, _, _)  -> ok;
tagged_pre(_, _, Term) -> {ok, ast:tag(parent, Term)}.
tagged_post(_, _, {pair, Ctx, {symbol, _, keyword, Name}, Val} = Term) ->
    Parent = ast:get_tag(parent, Term),
    case Parent =:= Name of
        true    -> {ok, {tagged, Ctx, [Name], Val}};
        false   -> {ok, {tagged, Ctx, [Parent, Name], Val}}
    end;
tagged_post(_, _, _) -> ok.
tagged_values(AST) ->
    ast:traverse(fun tagged_pre/3, fun tagged_post/3, AST).

-ifdef(TEST).

do_preen(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    {ok, Parsed} = syntax:parse(Tokens),
    preener:preen("some_path", Parsed).

unwrap_tuple_with_one_element_test_() ->
    Code = "def test b -> (b)",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _, _,
                    {symbol, _, variable, b}}]}}]}, do_preen(Code)).

wrap_tuple_elems_in_seq_test_() ->
    Code = "def test a b -> (a; a; b)",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _, _,
                    {seq, _, {symbol, _, _, a},
                     {seq, _, {symbol, _, _, a},
                      {symbol, _, _, b}}}}]}}]}, do_preen(Code)). 

translate_vals_to_let_statements_test_() ->
    Code = "def test a -> (val b = a; b; val c = a; c)",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _, _,
                    {'let', _, {symbol, _, _, b}, {symbol, _, _, a},
                     {seq, _, {symbol, _, _, b},
                      {'let', _, {symbol, _, _, c}, {symbol, _, _, a},
                       {symbol, _, _, c}}}}}]}}]}, do_preen(Code)).

dict_expr_test_() ->
    Code = "def test a -> {a: a, b: a}",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _, _,
                    {dict, _,
                     [{pair, _, {keyword, _, a}, {symbol, _, _, a}},
                      {pair, _, {keyword, _, b}, {symbol, _, _, a}}]}}]}}]}, do_preen(Code)).

dict_pattern_test_() ->
    Code = "def test\n"
           "    {a: b, b: c} -> c",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _,
                    [{dict, _,
                      [{pair, _, {keyword, _, a}, {symbol, _, _, b}},
                       {pair, _, {keyword, _, b}, {symbol, _, _, c}}]}],
                    {symbol, _, _, c}}]}}]}, do_preen(Code)).

dict_pattern_variable_test_() ->
    Code = "def test\n"
           "    {a: b, b} -> b",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _,
                    [{dict, _,
                      [{pair, _, {keyword, _, a}, {symbol, _, _, b}},
                       {symbol, _, _, b}]}],
                    {symbol, _, _, b}}]}}]}, do_preen(Code)).

dict_illegal_element_test_() ->
    Code = "def test a -> {{a}, {b}: b}",
    ?testError({illegal_dict_element, dict, expr},
               {illegal_dict_pair_element, dict, expr}, do_preen(Code)).

pair_expr_test_() ->
    Code = "def test a -> (A: {a: a, b})",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _, _,
                    {tagged, _, [test, 'A'],
                     {dict, _,
                      [{pair, _, {keyword, _, a}, {symbol, _, _, a}},
                       {keyword, _, b}]}}}]}}]}, do_preen(Code)).

pair_expr_homonym_test_() ->
    Code = "def A a -> (A: {a: a, b})",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _, _,
                    {tagged, _, ['A'],
                     {dict, _,
                      [{pair, _, {keyword, _, a}, {symbol, _, _, a}},
                       {keyword, _, b}]}}}]}}]}, do_preen(Code)).

pair_pattern_test_() ->
    Code = "def test\n"
           "    (a: {a: b, c}) -> c",
    ?test({ok, [{def, _, _,
                 {'fun', _,
                  [{clause, _,
                    [{pair, _,
                      {symbol, _, _, a},
                      {dict, _,
                       [{pair, _, {keyword, _, a}, {symbol, _, _, b}},
                        {symbol, _, _, c}]}}],
                    {symbol, _, _, c}}]}}]}, do_preen(Code)).

-endif.
