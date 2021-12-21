-define(TIMEOUT, 3600).

-define(test(Expected, Actual),
        begin
            {timeout, ?TIMEOUT,
             [?_assertMatch(Expected, Actual)]}
        end).

-define(testEqual(Expected, Actual),
        begin
            {timeout, ?TIMEOUT,
             [?_assertEqual(Expected, Actual)]}
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
                          {error, [{Err, _} | _]} -> erlang:error({errorMatch,
                                                                   [{module, ?MODULE},
                                                                    {line, ?LINE},
                                                                    {expression, (??Expr)},
                                                                    {expected, (??Error)},
                                                                    {actual, Err}]});
                          __V -> erlang:error({errorMatch,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {expected, (??Error)},
                                                {actual, __V}]})
                      end
              end)())
        end).
-define(_errorMatch(Error, Expr), ?_test(?errorMatch(Error, Expr))).

-define(errorMatch(Err1, Err2, Expr),
        begin
            ((fun () ->
                      case (Expr) of
                          {error, [{Err1, _} | [{Err2, _} | _]]} -> ok;
                          {error, [{E1, _} | [{E2, _} | _]]} -> erlang:error({errorMatch,
                                                                   [{module, ?MODULE},
                                                                    {line, ?LINE},
                                                                    {expression, (??Expr)},
                                                                    {expected, ([??Err1, ??Err2])},
                                                                    {actual, [E1, E2]}]});
                          __V -> erlang:error({errorMatch,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {expected, ([??Err1, ??Err2])},
                                                {actual, __V}]})
                      end
              end)())
        end).
-define(_errorMatch(Err1, Err2, Expr), ?_test(?errorMatch(Err1, Err2, Expr))).

-define(errorMatch(Err1, Err2, Err3, Expr),
        begin
            ((fun () ->
                      case (Expr) of
                          {error, [{Err1, _} | [{Err2, _} | [{Err3, _} | _]]]} -> ok;
                          {error, [{E1, _} | [{E2, _} | [{E3, _} | _]]]} -> erlang:error({errorMatch,
                                                                                          [{module, ?MODULE},
                                                                                           {line, ?LINE},
                                                                                           {expression, (??Expr)},
                                                                                           {expected, ([??Err1, ??Err2, ??Err3])},
                                                                                           {actual, [E1, E2, E3]}]});
                          __V -> erlang:error({errorMatch,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {expected, ([??Err1, ??Err2, ??Err3])},
                                                {actual, __V}]})
                      end
              end)())
        end).
-define(_errorMatch(Err1, Err2, Err3, Expr), ?_test(?errorMatch(Err1, Err2, Err3, Expr))).

-define(debugTerm(Term), begin
                             Pre = fun(_, _, T) -> {ok, setelement(2, T, #{})} end,
                             Post = fun(_, _, _) -> ok end,
                             {ok, {_, NT}} = ast:traverse_term(expr, Pre, Post, #{}, Term),
                             ?debugVal(NT, 100)
                         end).
