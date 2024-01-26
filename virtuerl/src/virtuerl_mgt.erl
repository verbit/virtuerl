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
  code_change/3, handle_continue/2]).
-export([create_vm/0, domain_create/1, domain_get/1, domain_delete/1, domain_stop/1, domain_start/1, domains_list/0]).
-export([home_path/0]).

-define(SERVER, ?MODULE).
-define(APPLICATION, virtuerl).

create_vm() ->
  gen_server:call(?SERVER, {domain_create, {default}}).

%%create_vm(#{cpus := NumCPUs, memory := Memory}) ->
domain_create(Conf) ->
  gen_server:call(?SERVER, {domain_create, Conf}).

domain_delete(Conf) ->
  gen_server:call(?SERVER, {domain_delete, Conf}).

domain_get(Conf) ->
  gen_server:call(?SERVER, {domain_get, Conf}).

-spec domains_list() -> #{}.
domains_list() ->
  gen_server:call(?SERVER, domains_list).

domain_stop(Id) ->
  gen_server:call(?SERVER, {domain_update, #{id => Id, state => stopped}}).

domain_start(Id) ->
  gen_server:call(?SERVER, {domain_update, #{id => Id, state => running}}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

home_path() ->
  application:get_env(?APPLICATION, home, "var").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, Table} = dets:open_file(domains, [{file, filename:join(home_path(), "domains.dets")}]),
%%  virtuerl_ipam:ipam_put_net({default, <<192:8, 168:8, 10:8, 0:8>>, 28}),
%%  application:ensure_all_started(grpcbox),
  {ok, {Table}, {continue, setup_base}}.
%%  {ok, {Table}}.

handle_continue(setup_base, State) ->
  ok = filelib:ensure_path(filename:join(home_path(), "domains")),

%%  BaseImagePath = filename:join(home_path(), "debian-12-genericcloud-amd64-20230910-1499.qcow2"),
  BaseImagePath = filename:join(home_path(), "openSUSE-Leap-15.5.x86_64-NoCloud.qcow2"),
  case filelib:is_regular(BaseImagePath) of
    true -> ok;
    false ->
      TempImagePath = "/tmp/virtuerl/debian-12-genericcloud-amd64-20230910-1499.qcow2",
      ok = filelib:ensure_dir(TempImagePath),
      httpc:request(get, "https://cloud.debian.org/images/cloud/bookworm/20230910-1499/debian-12-genericcloud-arm64-20230910-1499.qcow2", [],
        [{stream, TempImagePath}]),
      file:rename(TempImagePath, BaseImagePath)
  end,

  {noreply, State, {continue, sync_domains}};


handle_continue(sync_domains, {Table} = State) ->
  TargetDomains = sets:from_list([Id || {Id, Domain} <- dets:match_object(Table, '_'),
    case Domain of
      #{state := stopped} -> false;
      _ -> true
    end]),
  RunningDomains = sets:from_list([Id || {Id, _, _, _} <- supervisor:which_children(virtuerl_sup), is_binary(Id)]),
  ToDelete = sets:subtract(RunningDomains, TargetDomains),
  ToAdd = sets:subtract(TargetDomains, RunningDomains),
  [supervisor:terminate_child(virtuerl_sup, Id) || Id <- sets:to_list(ToDelete)],
  [supervisor:delete_child(virtuerl_sup, Id) || Id <- sets:to_list(ToDelete)],
  ok = gen_server:call(virtuerl_net, {net_update}),
  [  supervisor:start_child(virtuerl_sup, {
    Id,
    {virtuerl_qemu, start_link, [Id]},
    transient,
    infinity,
    worker,
    []
}) || Id <- sets:to_list(ToAdd)],
  {noreply, State}.


generate_unique_tap_name(TapNames) ->
  TapName = io_lib:format("verltap~s", [binary:encode_hex(<<(rand:uniform(16#ffffff)):24>>)]),
  case sets:is_element(TapName, TapNames) of
    false ->
      TapName;
    true ->
      generate_unique_tap_name(TapNames)
  end.

handle_call({domain_create, Conf}, _From, State) ->
  {Table} = State,
  DomainID = virtuerl_util:uuid4(),
  Domain = maps:merge(#{id => DomainID, name => DomainID}, Conf),  % TODO: save ipv4 addr as well
  dets:insert_new(Table, {DomainID, Domain}),
  dets:sync(Table),

  #{network_id := NetworkID} = Domain,
  {ok, #{cidrs := Cidrs}} = virtuerl_ipam:ipam_get_net(NetworkID),
  Keys = lists:map(fun(Cidr) ->
    {Ip, _} = virtuerl_net:parse_cidr(Cidr),
    case bit_size(Ip) of
      32 -> ipv4_addr;
      128 -> ipv6_addr
    end
    end, Cidrs),

  Default = maps:from_keys(Keys, undefined),
  Conf0 = maps:merge(Default, Conf),
  Conf1 = maps:intersect(Default, Conf0),
  Conf2 = maps:to_list(Conf1),

  KeyToTag = fun(Key) -> case Key of ipv4_addr -> ipv4; ipv6_addr -> ipv6 end end,

  Addresses = [
    case Addr of
      undefined ->
        Tag = KeyToTag(Key),
        {ok, {NetAddr, Prefixlen}, Ip} = virtuerl_ipam:assign_next(NetworkID, Tag, DomainID),
        {Key, NetAddr, Ip, Prefixlen};
      Addr ->
        Addr1 = virtuerl_net:parse_ip(Addr),
        {ok, {NetAddr, Prefixlen}} = virtuerl_ipam:ipam_put_ip(NetworkID, Addr1, DomainID),
        {Key, NetAddr, Addr1, Prefixlen}
    end
    || {Key, Addr} <- Conf2
  ],
  IpCidrs = [{Ip, Prefixlen} || {_, _, Ip, Prefixlen} <- Addresses],
  AddressesMap = maps:from_list([{K, A} || {K, _, A, _} <- Addresses]),
  Ipv4Addr = maps:get(ipv4_addr, AddressesMap, undefined),
  Ipv6Addr = maps:get(ipv6_addr, AddressesMap, undefined),

  Domains = dets:match_object(Table, '_'),
  TapNames = sets:from_list([Tap || #{tap_name := Tap} <- Domains]),
  TapName = generate_unique_tap_name(TapNames),
  <<A:5, _:3, B:40>> = <<(rand:uniform(16#ffffffffffff)):48>>,
  MacAddr = <<A:5, 1:1, 2:2, B:40>>,

  dets:insert(Table, {DomainID, Domain#{network_addrs => Cidrs, mac_addr=>MacAddr, ipv4_addr=>Ipv4Addr, ipv6_addr => Ipv6Addr, cidrs => IpCidrs, tap_name => TapName}}),
  dets:sync(Table),

  ok = gen_server:call(virtuerl_net, {net_update}),

  supervisor:start_child(virtuerl_sup, {
    DomainID,
    {virtuerl_qemu, start_link, [DomainID]},
    permanent,
    infinity,
    worker,
    []
  }),
  {reply, {ok, maps:merge(#{id => DomainID, tap_name => iolist_to_binary(TapName), mac_addr => binary:encode_hex(MacAddr)}, maps:map(fun(_, V) -> iolist_to_binary(virtuerl_net:format_ip(V)) end, AddressesMap))}, State};
handle_call({domain_update, #{id := DomainID, state := RunState}}, _From, {Table} = State) ->
  Reply = case dets:lookup(Table, DomainID) of
            [{_, Domain}] ->
              ok = dets:insert(Table, {DomainID, Domain#{state => RunState}}),
              ok = dets:sync(Table),
              ok;
            [] -> notfound
          end,
  {reply, Reply, State, {continue, sync_domains}};
handle_call(domains_list, _From, State) ->
  {Table} = State,
  Domains = dets:match_object(Table, '_'),
  {reply, [maps:merge(#{state => stopped, name => Id}, Domain) || {Id, Domain} <- Domains], State};
handle_call({domain_get, #{id := DomainID}}, _From, State) ->
  {Table} = State,
  Reply = case dets:lookup(Table, DomainID) of
    [{_, #{mac_addr := MacAddr, ipv4_addr:=IP, tap_name := TapName} = Domain}] ->
      DomRet = Domain#{mac_addr := binary:encode_hex(MacAddr), ipv4_addr := virtuerl_net:format_ip_bitstring(IP), tap_name := iolist_to_binary(TapName)},
      {ok, maps:merge(#{name => DomainID}, DomRet)};
    [] -> notfound
  end,
  {reply, Reply, State};
handle_call({domain_delete, #{id := DomainID}}, _From, State) ->
  {Table} = State,
  Res = dets:delete(Table, DomainID),
  dets:sync(Table),
  io:format("terminating ~p~n", [DomainID]),
  ok = supervisor:terminate_child(virtuerl_sup, DomainID),
  io:format("done terminating ~p~n", [DomainID]),
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
