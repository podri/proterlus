%%%-------------------------------------------------------------------
%% @doc proterlus public API
%% @end
%%%-------------------------------------------------------------------

-module(proterlus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  proterlus_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
