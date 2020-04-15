-module(cli).

-behaviour(application).

-export([start/2, stop/1, main/1]).

start(StartType, StartArgs) ->
    io:format("StartType: ~p, StartArgs: ~p", [StartType, StartArgs]),
    kind_sup:start_link().

stop(_State) ->
    ok.

main(Args) ->
    io:format("Args: ~p", [Args]).


%% internal functions
