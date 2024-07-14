-module(virtuerl_pubsub).

-behaviour(gen_server).

-export([start_link/0, subscribe/0, subscribe/1, send/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).


subscribe() -> gen_server:call(?SERVER, subscribe).


subscribe(Pid) -> gen_server:call(?SERVER, {subscribe, Pid}).


send(Message) -> gen_server:cast(?SERVER, {send, Message}).


start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) -> {ok, []}.


handle_call(subscribe, {Pid, _Tag}, State) ->
    _Ref = monitor(process, Pid),
    {reply, ok, [Pid | State]};
handle_call({subscribe, Pid}, _From, State) ->
    _Ref = monitor(process, Pid),
    {reply, ok, [Pid | State]}.


handle_cast({send, Message}, State) ->
    [ Pid ! Message || Pid <- State ],
    {noreply, State}.


handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    ?LOG_NOTICE(#{who => ?MODULE, what => subscriber_died, pid => Pid, reason => Reason}),
    NewState = lists:delete(Pid, State),
    {noreply, NewState}.


terminate(_Reason, _State) -> ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
