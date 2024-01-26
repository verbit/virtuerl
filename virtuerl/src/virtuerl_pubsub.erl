%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_pubsub).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([subscribe/0, send/1]).

-define(SERVER, ?MODULE).
-define(APPLICATION, virtuerl).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

subscribe() ->
  gen_server:call(?SERVER, subscribe).

send(Message) ->
  gen_server:cast(?SERVER, {send, Message}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call(subscribe, {Pid, _Tag}, State) ->
  _Ref = monitor(process, Pid),
  {reply, ok, [Pid | State]}.

handle_cast({send, Message}, State) ->
  [Pid ! Message || Pid <- State],
  {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
  io:format("~p died because of ~p~n", [Pid, Reason]),
  NewState = lists:delete(Pid, State),
  {noreply, NewState}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
