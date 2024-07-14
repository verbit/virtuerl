-module(virtuerl_ui_app).

-behaviour(application).

-export([start/2, stop/1]).


%% app start/stop
start(_StartType, _StartArgs) ->
    virtuerl_ui:start().


stop(_State) ->
    ok.
