%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_stor).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, Table} = dets:open_file(stors, []),
  {ok, {Table}}.

handle_call({domain_delete, #{id := DomainID}}, _From, State) ->
  {Table} = State,
  Res = dets:delete(Table, DomainID),
  dets:sync(Table),
  ok = supervisor:terminate_child(virtuerl_sup, DomainID),
  ok = supervisor:delete_child(virtuerl_sup, DomainID),
  ok = gen_server:call(virtuerl_net, {net_update}),
  {reply, Res, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, {Table}) ->
  dets:close(Table),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
