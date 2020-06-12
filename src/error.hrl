% Convenience macros for testing against error functions. They serve two
% purposes:
% 1) Declutter tests that assert that certain types of errors happen
% 2) Help encapsulate the error state to the `error` module so I can more
%    easily modify the state without having to update every single test
-define(errorMatch(Error, Expr),
        begin
            ((fun () ->
                      case (Expr) of
                          {error, [{Error, _}]} -> ok;
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
                          {error, [{Err1, _}, {Err2, _}]} -> ok;
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
                          {error, [{Err1, _}, {Err2, _}, {Err3, _}]} -> ok;
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
