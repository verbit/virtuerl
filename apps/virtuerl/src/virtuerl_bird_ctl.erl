-module(virtuerl_bird_ctl).

-behaviour(gen_server).

-export([start_link/1, cmd/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").


start_link(SocketPath) ->
    Receiver = self(),
    {ok, Pid} = gen_server:start_link(?MODULE, [Receiver, SocketPath], []),
    receive
        {birdctl, Pid, {response, [{<<"0001">>, _}]}} -> ok
    end,
    {ok, Pid}.


cmd(Pid, Cmd) ->
    gen_server:call(Pid, {cmd, Cmd}),
    receive
        {birdctl, Pid, {response, Messages}} ->
            {Code, _Data} = lists:last(Messages),
            case binary_to_integer(Code) of
                Success when Success >= 0, Success < 1000 -> {ok, Messages};
                _ -> {error, Messages}
            end
    end.


init([Receiver, SocketPath]) ->
    {ok, BirdSocket} = gen_tcp:connect({local, SocketPath}, 0, [local, {active, true}, binary]),
    {ok, {Receiver, BirdSocket}}.


handle_call({cmd, Cmd}, _From, {_Receiver, BirdSocket} = State) ->
    ok = gen_tcp:send(BirdSocket, [Cmd, $\n]),
    {reply, ok, State}.


handle_cast(_Request, _State) ->
    erlang:error(not_implemented).


handle_info({tcp, BirdSocket, Data}, {Receiver, BirdSocket} = State) ->
    Lines0 = re:split(Data, "\n", []),
    [<<>> | LinesRev] = lists:reverse(Lines0),
    Lines = lists:reverse(LinesRev),

    Messages = handle_lines(Lines),
    Self = self(),
    [ Receiver ! {birdctl, Self, Msg} || Msg <- Messages ],

    {noreply, State}.


terminate(_Reason, {_Receiver, _BirdSocket} = _State) -> ok.


handle_lines(Lines) ->
    handle_lines(Lines, [], []).


handle_lines([], Acc, []) -> lists:reverse(Acc);
handle_lines([Line | Rest], Acc, Cur) ->
    {ok, Pat} = re:compile("(?:(?:(\\d{4})([ -]))|([ +]))(.*)"),
    {match, Groups} = re:run(Line, Pat, [{capture, all_but_first, binary}]),
    case Groups of
        [Code, <<"-">>, <<>>, Data] ->
            handle_lines(Rest, Acc, [{Code, Data} | Cur]);
        [<<>>, <<>>, <<" ">>, Data] ->
            [CurLatest | CurRest] = Cur,
            {CurCode, CurData} = CurLatest,
            NewCur = [{CurCode, [CurData, $\n, Data]} | CurRest],
            handle_lines(Rest, Acc, NewCur);
        [Code, <<" ">>, <<>>, Data] ->
            NewCur = [{Code, Data} | Cur],
            handle_lines(Rest, [{response, lists:reverse(NewCur)} | Acc], []);
        [<<>>, <<>>, <<"+">>, Data] ->
            handle_lines(Rest, [{event, Data} | Acc], [])
    end.
