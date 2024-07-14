-module(virtuerl_ghac_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).


%% app start/stop
start(_StartType, _StartArgs) ->
    virtuerl_ghac_sup:start_link().


start() ->
    application:ensure_all_started(virtuerl_ghac).
%%    exec:debug(4).


stop(_State) ->
    ok.
