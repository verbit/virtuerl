%%%-------------------------------------------------------------------
%% @doc virtuerl public API
%% @end
%%%-------------------------------------------------------------------

-module(virtuerl_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).

%% app start/stop
start(_StartType, _StartArgs) ->
%%    Dispatch = cowboy_router:compile([
%%        {'_', [
%%            {"/domains", virtuerl_api, []}
%%        ]}
%%    ]),
%%    Res = cowboy:start_clear(my_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
%%    io:format("RESULT: ~p~n", [Res]),
%%    {ok, _} = Res,
    virtuerl_sup:start_link().

start() ->
    application:ensure_all_started(virtuerl).

stop(_State) ->
    ok.
