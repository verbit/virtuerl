%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_qemu).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ID) ->
%%  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
  Pid = spawn_link(fun() ->
    io:format("QEMU: Starting VM with ID ~p~n", [ID]),
    timer:sleep(20000),
    io:format("QEMU: Exiting ~p~n", [ID]),
    timer:sleep(500),
    exit(failure)

             end),
  {ok, Pid}.

init([]) ->
  {ok, Table} = dets:open_file(vms, []),
  % TODO: erlexec: spawn bird -f
  {ok, {Table}}.

terminate(_Reason, {Table}) ->
  dets:close(Table).

handle_call({vm_create, Conf}, _From, State) ->
  {reply, ok, State}.

handle_cast({net_update}, State) ->
  {Table} = State,
  update_bird_conf(Table),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_bird_conf(Table) ->
  % 1. write config
  %%  for VM in VMs:
  %%    append VM.IP to static routes: VM.IP via $VM.network.bridge
  % 2. birdc configure
  % 3. profit?
  VMs = dets:match_object(Table, '_'),
  [io:format("~p~n", [VM]) || VM <- VMs],
  reload_bird().

reload_bird() ->
  ok.
