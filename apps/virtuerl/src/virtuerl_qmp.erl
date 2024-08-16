-module(virtuerl_qmp).

-behaviour(gen_server).

-export([start_link/1, exec/2, stop/1]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2]).

-include_lib("kernel/include/logger.hrl").


exec(Pid, Command) ->
    Pid ! {qmp, Command},
    receive
        {qmp, #{<<"return">> := #{}}} -> ok
    end.


start_link(QmpSocketPath) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {QmpSocketPath, self()}, []),
    receive
        {qmp, #{<<"QMP">> := #{}}} -> ok
    after
        1000 ->
            exit(qmp_not_responding)
    end,
    exec(Pid, qmp_capabilities),
    {ok, Pid}.


stop(Pid) ->
    gen_server:stop(Pid).


init({QmpSocketPath, Receiver}) ->
    {ok, QmpSocket} = gen_tcp:connect({local, QmpSocketPath}, 0, [local, {active, true}]),
    {ok, {QmpSocket, Receiver}}.


handle_call(_Request, _From, _State) ->
    erlang:error(not_implemented).


handle_cast(_Request, _State) ->
    erlang:error(not_implemented).


handle_info({tcp, _Socket, RawData}, {_QmpSocket, Receiver} = State) ->
    Lines = re:split(RawData, "\r?\n", [trim]),
    Jsons = lists:map(fun(Line) -> {ok, Json} = thoas:decode(Line), Json end, Lines),
    ?LOG_DEBUG(#{qmp_raw => RawData, qmp_parsed => Jsons}),
    [ Receiver ! {qmp, Json} || Json <- Jsons ],
    {noreply, State};

handle_info({qmp, Command}, {QmpSocket, _Receiver} = State) ->
    ?LOG_INFO(#{who => virtuerl_qmp, command => Command}),
    ok = gen_tcp:send(QmpSocket, thoas:encode(#{execute => Command})),
    {noreply, State}.


terminate(_Reason, {QmpSocket, _Receiver}) ->
    gen_tcp:close(QmpSocket).
