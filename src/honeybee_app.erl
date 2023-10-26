%%%-------------------------------------------------------------------
%% @doc honeybee public API
%% @end
%%%-------------------------------------------------------------------

-module(honeybee_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    honeybee_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
