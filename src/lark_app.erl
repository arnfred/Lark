%%%-------------------------------------------------------------------
%% @doc lark public API
%% @end
%%%-------------------------------------------------------------------

-module(lark_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lark_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
