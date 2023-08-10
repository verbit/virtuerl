%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_mgt).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([create_vm/0, domain_create/1, domain_get/1]).

-define(SERVER, ?MODULE).

create_vm() ->
  gen_server:call(?SERVER, {domain_create, {default}}).

%%create_vm(#{cpus := NumCPUs, memory := Memory}) ->
domain_create(Conf) ->
  gen_server:call(?SERVER, {domain_create, Conf}).

domain_delete(Conf) ->
  gen_server:call(?SERVER, {domain_delete, Conf}).

domain_get(Conf) ->
  gen_server:call(?SERVER, {domain_get, Conf}).

domains_list(Conf) ->
  gen_server:call(?SERVER, {domains_list, Conf}).



%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-record(domain, {id, network_id, network_addr, ipv4_addr, tap_name}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, Table} = dets:open_file(vms, []),
  virtuerl_ipam:ipam_put_net({default, <<192:8, 168:8, 10:8, 0:8>>, 28}),
  application:ensure_all_started(grpcbox),
  {ok, {Table}}.

generate_unique_tap_name(TapNames) ->
  TapName = io_lib:format("verltap~s", [binary:encode_hex(<<(rand:uniform(16#ffffff)):24>>)]),
  case sets:is_element(TapName, TapNames) of
    false ->
      TapName;
    true ->
      generate_unique_tap_name(TapNames)
  end.

handle_call({domain_create, #{network_id := NetworkID}}, _From, State) ->
  {Table} = State,
  DomainID = binary:encode_hex(<<(rand:uniform(16#FFFFFFFF)-1):32>>),
  Domain = #domain{id = DomainID, network_id = NetworkID},
  dets:insert_new(Table, {DomainID, Domain}),
  dets:sync(Table),
  {ok, Network, IP} = virtuerl_ipam:assign_next(NetworkID, DomainID),
  Domains = dets:match_object(Table, '_'),
  TapNames = sets:from_list([Tap || #domain{tap_name=Tap} <- Domains]),
  TapName = generate_unique_tap_name(TapNames),
  dets:insert(Table, {DomainID, Domain#domain{network_addr =Network, ipv4_addr=IP, tap_name = TapName}}),
  dets:sync(Table),

  gen_server:cast(virtuerl_net, {net_update}),
  supervisor:start_child(virtuerl_sup, {
    DomainID,
    {virtuerl_qemu, start_link, [DomainID]},
    permanent,
    infinity,
    worker,
    []
  }),
  {reply, {ok, #{id => DomainID, tap_name => TapName, ip_addr => IP}}, State};
handle_call({domain_get, #{id := DomainID}}, _From, State) ->
  {Table} = State,
  Reply = case dets:lookup(Table, DomainID) of
    [{_, #domain{network_id = NetworkID, ipv4_addr=IP, tap_name = TapName}}] ->
      DomRet = #{network_id => NetworkID, ipv4_addr => virtuerl_net:format_ip(IP)}, % , tap_name => TapName
      {ok, DomRet};
    [] -> notfound
  end,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, {Table}) ->
  dets:close(Table),
  ok.

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
%%  VMs = dets:match_object(Table, '_'),
%%  [io:format("~p~n", [VM]) || VM <- VMs].
  ok.
