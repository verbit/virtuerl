-module(virtuerl_mock_vm).

-behaviour(gen_server).

-export([start_link/1]).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2]).


start_link(Dom) ->
    gen_server:start_link(?MODULE, [Dom], []).


init([Dom]) ->
    io:format("MOCK/VM: ~p~n", [Dom]),
    {ok, {}}.


handle_call(Request, From, State) ->
    erlang:error(not_implemented).


handle_cast(Request, State) ->
    erlang:error(not_implemented).
