%%%-------------------------------------------------------------------
%% @doc kind public API
%% @end
%%%-------------------------------------------------------------------

-module(kind_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kind_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
