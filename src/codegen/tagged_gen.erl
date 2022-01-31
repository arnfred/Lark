-module(tagged_gen).
-export([term/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

term({link, _, _} = Term) -> Term;
term({keyword, _, _, _} = Term) -> Term;
term({tagged, Ctx, _, Val} = Term) ->
    Arg = {symbol, symbol:ctx(Val), variable, arg(1)},
    {def, Ctx, symbol:tag(Term), {'fun', Ctx, [{clause, Ctx, [{pair, Ctx, Arg, Val}], Term}]}}.

% When we substitute an expression for a variable, we use the expression as a
% pattern guard in the generated function. To give an example, the generated function for `S: Boolean` is:
%
% ```
% def S (substituted_1: Boolean) -> substituted_1
% ```
%
% The pattern might contain variables as part of the original term. In a
% pattern, an unbound variable is just substituted for the `any` domain.
% However, if the pattern contain a function application, the application
% arguments are treated as expressions (and not patterns). If they are not
% defined in other parts of the code, they will cause an error when tagging and
% transpiling the code.
%
% I've had a long think about how best to deal with the presence of these
% variables. The best option, I think is to replace them with a hard-coded
% `any` domain, which is what I've done below.
sanitize_pattern(Term) ->
    {ok, {_, NewTerm}} = ast:traverse_term(pattern, fun(_, _, _) -> ok end, fun sanitize_post/3, #{}, Term),
    NewTerm.

sanitize_post(expr, _, {symbol, Ctx, variable, _Name}) -> {ok, {symbol, Ctx, keyword, '_'}};
sanitize_post(expr, _, {symbol, Ctx, keyword, _Name}) -> {ok, {symbol, Ctx, keyword, '_'}};
sanitize_post(_, _, _) -> ok.


arg(N) -> list_to_atom("substituted_" ++ integer_to_list(N)).

-ifdef(TEST).

tagged_tuple_test_() ->
    Expected = {def, #{}, 'tagged/T',
                {'fun', #{},
                 [{clause, #{}, [{pair, #{},
                                  {symbol, #{}, variable, substituted_1},
                                  {tuple, #{}, [{keyword, #{}, [tagged, 'A'], 'A'}]}}],
                   {tagged, #{}, [tagged, 'T'], {tuple, #{}, [{keyword, #{}, [tagged, 'A'], 'A'}]}}}]}},
    [?test(Expected, term({tagged, #{}, [tagged, 'T'], {tuple, #{}, [{keyword, #{}, [tagged, 'A'], 'A'}]}}))].

sanitized_pattern_test_() ->
    Expected = {def, #{}, 'tagged/T',
                {'fun', #{},
                 [{clause, #{}, [{pair, #{},
                                  {symbol, #{}, variable, substituted_1},
                                  {application, #{}, {symbol, #{}, variable, test},
                                                 [{symbol, #{}, keyword, '_'}]}}],
                   {tagged, #{}, [tagged, 'T'], {application, #{}, {symbol, #{}, variable, test},
                                                 [{symbol, #{}, variable, test}]}}}]}},
    [?test(Expected, term({tagged, #{}, [tagged, 'T'], {application, #{}, {symbol, #{}, variable, test},
                                                 [{symbol, #{}, variable, test}]}}))].
-endif.
