-module(virtuerl_qmp).

-behaviour(gen_server).

-export([start_link/2, exec/2, stop/1]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2]).

-include_lib("kernel/include/logger.hrl").


exec(Pid, Command) ->
    gen_server:call(Pid, Command).


stop(Pid) ->
    gen_server:stop(Pid).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================


start_link(QmpSocketPath, Receiver) ->
    gen_server:start_link(?MODULE, {QmpSocketPath, Receiver}, []).


init({QmpSocketPath, Receiver}) ->
    Self = self(),
    Pid = spawn_link(fun() -> qmp_translator(QmpSocketPath, Self) end),
    io:format("virtuerl_qmp: init~n"),
    receive
        {qmp, #{<<"QMP">> := #{}}} -> ok
    after
        1000 ->
            exit(qmp_not_responding)
    end,
    io:format("virtuerl_qmp: init after~n"),
    execute(Pid, qmp_capabilities),
    io:format("virtuerl_qmp: qmp caps~n"),
    {ok, {Pid, Receiver}}.


terminate(_Reason, {Pid, _}) ->
    Ref = monitor(process, Pid),
    exit(Pid, normal),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    end,
    true = demonitor(Ref),
    io:format("exiting QMP server~n").


handle_call(Command, _From, {Pid, _} = State) ->
    execute(Pid, Command),
    {reply, ok, State}.


handle_cast(Request, State) ->
    erlang:error(not_implemented).


handle_info({qmp, #{<<"event">> := _} = Event}, {_, Receiver} = State) ->
    Receiver ! {qmp, Event},
    {noreply, State}.


execute(Pid, Command) when is_pid(Pid) ->
    Pid ! {qmp, Command},
    receive
        {qmp, #{<<"return">> := #{}}} -> ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


qmp_translator(QmpSocketPath, Receiver) ->
    process_flag(trap_exit, true),
    io:format("starting QMP translator (~s)!~n", [QmpSocketPath]),
    {ok, QmpSocket} = gen_tcp:connect({local, QmpSocketPath}, 0, [local, {active, true}]),
    qmp_loop(QmpSocket, Receiver).


qmp_loop(QmpSocket, Receiver) ->
    receive
        {tcp, _Socket, RawData} ->
            Lines = re:split(RawData, "\r?\n", [trim]),
            Jsons = lists:map(fun(Line) -> {ok, Json} = thoas:decode(Line), Json end, Lines),
            ?LOG_DEBUG(#{qmp_raw => RawData, qmp_parsed => Jsons}),
            [ Receiver ! {qmp, Json} || Json <- Jsons ],
            qmp_loop(QmpSocket, Receiver);
        {qmp, Command} when is_atom(Command) ->
            io:format("qmp_loop/qmp: ~p~n", [Command]),
            ok = gen_tcp:send(QmpSocket, thoas:encode(#{execute => Command})),
            qmp_loop(QmpSocket, Receiver);
        {'EXIT', _SenderID, Reason} ->
            io:format("closing QMP socket (~p)!~n", [Reason]),
            ok = gen_tcp:close(QmpSocket),
            exit(Reason)
    end.
