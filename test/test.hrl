-define(TIMEOUT, 3600).
-include_lib("eunit/include/eunit.hrl").

-define(test(Expected, Actual),
        begin
            {timeout, ?TIMEOUT,
             [?_assertMatch(Expected, Actual)]}
        end).


-define(testError(Error, Expr),
        begin
            {timeout, ?TIMEOUT,
             [?_errorMatch(Error, Expr)]}
        end).
-define(testError(Error1, Error2, Expr),
        begin
            {timeout, ?TIMEOUT,
             [?_errorMatch(Error1, Error2, Expr)]}
        end).
-define(testError(Error1, Error2, Error3, Expr),
        begin
            {timeout, ?TIMEOUT,
             [?_errorMatch(Error1, Error2, Error3, Expr)]}
        end).

-define(errorMatch(Error, Expr),
        begin
            ((fun () ->
                      case (Expr) of
                          {error, [{Error, _} | _]} -> ok;
                          __V -> erlang:error({errorMatch,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {pattern, (??Error)},
                                                {value, __V}]})
                      end
              end)())
        end).
-define(_errorMatch(Error, Expr), ?_test(?errorMatch(Error, Expr))).

-define(errorMatch(Err1, Err2, Expr),
        begin
            ((fun () ->
                      case (Expr) of
                          {error, [{Err1, _} | [{Err2, _} | _]]} -> ok;
                          __V -> erlang:error({errorMatch,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {pattern, ([??Err1, ??Err2])},
                                                {value, __V}]})
                      end
              end)())
        end).
-define(_errorMatch(Err1, Err2, Expr), ?_test(?errorMatch(Err1, Err2, Expr))).

-define(errorMatch(Err1, Err2, Err3, Expr),
        begin
            ((fun () ->
                      case (Expr) of
                          {error, [{Err1, _} | [{Err2, _} | [{Err3, _} | _]]]} -> ok;
                          __V -> erlang:error({errorMatch,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {pattern, ([??Err1, ??Err2, ??Err3])},
                                                {value, __V}]})
                      end
              end)())
        end).
-define(_errorMatch(Err1, Err2, Err3, Expr), ?_test(?errorMatch(Err1, Err2, Err3, Expr))).

-define(debugTerm(Term), begin
                             Pre = fun(_, _, T) -> {ok, set_element(2, T, #{})} end,
                             Post = fun(_, _, _) -> ok end,
                             {ok, {_, NT}} = ast:traverse_term(expr, Pre, Post, #{}, T),
                             ?debugVal(NT, 100)
                         end).


