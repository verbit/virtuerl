-module(virtuerl_mgt).

-behaviour(gen_server).

-export([start_link/0,
         home_path/0,
         image_from_domain/2,
         domain_update/1,
         create_vm/0,
         domain_create/1,
         domain_get/1,
         domain_delete/1,
         domain_stop/1,
         domain_start/1,
         domains_list/0,
         add_port_fwd/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         handle_continue/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER,      ?MODULE).
-define(APPLICATION, virtuerl).

-record(state, {table, idmap}).


create_vm() ->
    gen_server:call(?SERVER, {domain_create, {default}}).


%%create_vm(#{cpus := NumCPUs, memory := Memory}) ->
domain_create(Conf) ->
    gen_server:call(?SERVER, {domain_create, Conf}, infinity).


domain_delete(Conf) ->
    gen_server:call(?SERVER, {domain_delete, Conf}, infinity).


domain_get(Conf) ->
    gen_server:call(?SERVER, {domain_get, Conf}).


-spec domains_list() -> #{}.
domains_list() ->
    gen_server:call(?SERVER, domains_list).


domain_update(Conf) ->
    gen_server:call(?SERVER, {domain_update, Conf}).


domain_stop(Id) ->
    gen_server:call(?SERVER, {domain_update, #{id => Id, state => stopped}}).


domain_start(Id) ->
    gen_server:call(?SERVER, {domain_update, #{id => Id, state => running}}).


image_from_domain(DomainId, ImageName) ->
    gen_server:call(?SERVER, {image_from_domain, #{id => DomainId, image_name => ImageName}}, infinity).


-spec add_port_fwd(binary(), #{protos := [tcp | udp], source_port := integer(), target_port := integer()}) -> term().
add_port_fwd(DomainId, PortFwd) ->
    gen_server:call(?SERVER, {add_port_fwd, DomainId, PortFwd}).


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
    net_kernel:monitor_nodes(true),
    {ok, #state{table = Table, idmap = #{}}, {continue, setup_base}}.
%%  {ok, {Table}}.


handle_continue(setup_base, State) ->
    ok = filelib:ensure_path(filename:join(home_path(), "domains")),
    {noreply, State, {continue, sync_domains}};

handle_continue(sync_domains, #state{table = Table, idmap = IdMap} = State) ->
    Domains = dets:match_object(Table, '_'),
    TargetDomains = maps:from_list(
                      [ {Id, case maps:get(host, Domain, localhost) of localhost -> node(); Else -> Else end}
                        || {Id, Domain} <- Domains,
                           case Domain of
                               #{state := stopped} -> false;
                               _ -> true
                           end ]),
    AllNodes = nodes([this, visible]),
    RunningDomains = maps:from_list(lists:flatten(
                                      [ [ {Id, Node} || {Id, _, _, _} <- supervisor:which_children({virtuerl_sup, Node}), is_binary(Id) ]
                                        || Node <- AllNodes ])),
    ToDelete = maps:without(maps:keys(TargetDomains), RunningDomains),
    ToAdd = maps:without(maps:keys(RunningDomains), TargetDomains),
    [ supervisor:terminate_child({virtuerl_sup, Node}, Id) || {Id, Node} <- maps:to_list(ToDelete) ],
    [ supervisor:delete_child({virtuerl_sup, Node}, Id) || {Id, Node} <- maps:to_list(ToDelete) ],

    % lists:foreach(fun () -> ok end, List)
    DomsByNode = maps:groups_from_list(
                   fun({_Id, Dom}) -> case maps:get(host, Dom, localhost) of localhost -> node(); Else -> Else end end,
                   fun({_Id, Dom}) -> Dom end,
                   Domains),
    [ virtuerl_net:update_net(Node, Doms) || {Node, Doms} <- maps:to_list(DomsByNode), lists:member(Node, AllNodes) ],

    VmPids = [ {Id,
                supervisor:start_child({virtuerl_sup, Node},
                                       {Id,
                                        {virtuerl_qemu, start_link, [maps:get(Id, maps:from_list(Domains))]},
                                        transient,
                                        infinity,
                                        worker,
                                        []})} || {Id, Node} <- maps:to_list(ToAdd), lists:member(Node, AllNodes) ],
    VmPidToDomId = maps:from_list([ {VmPid, DomId} || {DomId, {ok, VmPid}} <- VmPids ]),
    [ monitor(process, VmPid) || {_, {ok, VmPid}} <- VmPids ],

    {noreply, State#state{idmap = maps:merge(IdMap, VmPidToDomId)}}.


generate_unique_tap_name(TapNames) ->
    TapName = io_lib:format("verltap~s", [binary:encode_hex(<<(rand:uniform(16#ffffff)):24>>)]),
    case sets:is_element(TapName, TapNames) of
        false ->
            TapName;
        true ->
            generate_unique_tap_name(TapNames)
    end.


handle_call({domain_create, Conf}, _From, State) ->
    #state{table = Table} = State,
    DomainID = virtuerl_util:uuid4(),
    Domain0 = maps:merge(#{
                           id => DomainID,
                           node => localhost,
                           name => DomainID,
                           vcpu => 4,
                           memory => 4096,
                           os_type => "linux",
                           created_at => erlang:system_time(millisecond)
                          },
                         Conf),  % TODO: save ipv4/6 addr as well
    Domain = case Domain0 of
                 #{os_type := "linux"} ->
                     maps:merge(#{base_image => "debian-12-genericcloud-amd64-20240211-1654.qcow2"}, Domain0);
                 #{os_type := "windows"} ->
                     maps:merge(#{setup_iso => "Win11_23H2_EnglishInternational_x64v2_noprompt.iso"}, Domain0)
             end,
    dets:insert_new(Table, {DomainID, Domain}),
    dets:sync(Table),
    ?LOG_NOTICE(#{event => domain_requested, domain => Domain}),

    #{network_id := NetworkID} = Domain,
    {ok, #{cidrs := Cidrs}} = virtuerl_ipam:ipam_get_net(NetworkID),
    Keys = lists:map(fun(Cidr) ->
                             {Ip, _} = virtuerl_net:parse_cidr(Cidr),
                             case bit_size(Ip) of
                                 32 -> ipv4_addr;
                                 128 -> ipv6_addr
                             end
                     end,
                     Cidrs),

    Default = maps:from_keys(Keys, undefined),
    Conf0 = maps:merge(Default, Conf),
    Conf1 = maps:intersect(Default, Conf0),
    Conf2 = maps:to_list(Conf1),

    KeyToTag = fun(Key) -> case Key of ipv4_addr -> ipv4; ipv6_addr -> ipv6 end end,

    Addresses = [ case Addr of
                      undefined ->
                          Tag = KeyToTag(Key),
                          {ok, {NetAddr, Prefixlen}, Ip} = virtuerl_ipam:assign_next(NetworkID, Tag, DomainID),
                          {Key, NetAddr, Ip, Prefixlen};
                      Addr ->
                          Addr1 = virtuerl_net:parse_ip(Addr),
                          {ok, {NetAddr, Prefixlen}} = virtuerl_ipam:ipam_put_ip(NetworkID, Addr1, DomainID),
                          {Key, NetAddr, Addr1, Prefixlen}
                  end
                  || {Key, Addr} <- Conf2 ],
    IpCidrs = [ {Ip, Prefixlen} || {_, _, Ip, Prefixlen} <- Addresses ],
    AddressesMap = maps:from_list([ {K, A} || {K, _, A, _} <- Addresses ]),
    Ipv4Addr = maps:get(ipv4_addr, AddressesMap, undefined),
    Ipv6Addr = maps:get(ipv6_addr, AddressesMap, undefined),

    Domains = dets:match_object(Table, '_'),
    TapNames = sets:from_list([ Tap || #{tap_name := Tap} <- Domains ]),
    TapName = generate_unique_tap_name(TapNames),  % TODO: TapName should be generated on a per-deployment basis
    <<A:5, _:3, B:40>> = <<(rand:uniform(16#ffffffffffff)):48>>,
    MacAddr = <<A:5, 1:1, 2:2, B:40>>,

    DomainWithIps = Domain#{
                      network_addrs => Cidrs,
                      mac_addr => MacAddr,
                      ipv4_addr => Ipv4Addr,
                      ipv6_addr => Ipv6Addr,
                      cidrs => IpCidrs,
                      tap_name => TapName
                     },
    dets:insert(Table, {DomainID, DomainWithIps}),
    dets:sync(Table),
    ?LOG_NOTICE(#{event => domain_ready, domain => DomainWithIps}),
    virtuerl_pubsub:send({domain_created, DomainID}),

    {reply,
     {ok, maps:merge(#{id => DomainID, tap_name => iolist_to_binary(TapName), mac_addr => binary:encode_hex(MacAddr)},
                     maps:map(fun(_, V) -> iolist_to_binary(virtuerl_net:format_ip(V)) end, AddressesMap))},
     State,
     {continue, sync_domains}};
handle_call({domain_update, #{id := DomainID} = DomainUpdate0}, _From, #state{table = Table} = State) ->
    DomainUpdate = maps:remove(id, DomainUpdate0),
    Reply = case dets:lookup(Table, DomainID) of
                [{_, Domain}] ->
                    ok = dets:insert(Table, {DomainID, maps:merge(Domain, DomainUpdate)}),
                    ok = dets:sync(Table),
                    virtuerl_pubsub:send({domain_updated, DomainID}),
                    ok;
                [] -> {error, notfound}
            end,
    {reply, Reply, State, {continue, sync_domains}};
handle_call({add_port_fwd, DomId, PortFwd}, _From, #state{table = Table} = State) ->
    Reply = case dets:lookup(Table, DomId) of
                [{_, Domain}] ->
                    NewFwds = case Domain of
                                  #{port_fwds := Fwds} -> Fwds;
                                  _ -> []
                              end,
                    DomainUpdate = #{port_fwds => [PortFwd | NewFwds]},
                    ok = dets:insert(Table, {DomId, maps:merge(Domain, DomainUpdate)}),
                    ok = dets:sync(Table),
                    virtuerl_pubsub:send({domain_updated, DomId}),
                    ok;
                [] -> {error, notfound}
            end,
    {reply, Reply, State, {continue, sync_domains}};
handle_call(domains_list, _From, State) ->
    #state{table = Table} = State,
    Domains = dets:match_object(Table, '_'),
    {reply, [ maps:merge(#{node => localhost, state => running, name => Id, vcpu => 1, memory => 512}, Domain) || {Id, Domain} <- Domains ], State};
handle_call({domain_get, #{id := DomainID}}, _From, State) ->
    #state{table = Table} = State,
    Reply = case dets:lookup(Table, DomainID) of
                [{_, #{mac_addr := MacAddr, tap_name := TapName} = Domain}] ->
                    DomRet = Domain#{
                               mac_addr := binary:encode_hex(MacAddr),
                               tap_name := iolist_to_binary(TapName)
                              },
                    {ok, maps:merge(#{node => localhost, state => running, name => DomainID, vcpu => 1, memory => 512}, DomRet)};
                [] -> notfound
            end,
    {reply, Reply, State};
handle_call({domain_delete, #{id := DomainID}}, _From, State) ->
    #state{table = Table} = State,
    Res = case dets:lookup(Table, DomainID) of
              [] -> {error, notfound};
              [{_, Domain}] ->
                  dets:insert(Table, {DomainID, Domain#{state => deleting}}),
                  dets:sync(Table),
                  spawn_link(fun() ->
                                     TargetNode = case Domain of
                                                      #{host := localhost} -> node();
                                                      #{host := Else} -> Else;
                                                      _ -> node()
                                                  end,
                                     io:format("terminating ~p~n", [DomainID]),
                                     supervisor:terminate_child({virtuerl_sup, TargetNode}, DomainID),
                                     io:format("done terminating ~p~n", [DomainID]),
                                     supervisor:delete_child({virtuerl_sup, TargetNode}, DomainID),
                                     DomainHomePath = filename:join([virtuerl_mgt:home_path(), "domains", DomainID]),
                                     file:del_dir_r(DomainHomePath),
                                     ok = virtuerl_ipam:unassign(DomainID),
                                     % TODO: we don't really have to delete unused tap here as they will be deleted before the next domain is created
                                     %   however, it's of course cleaner to do so here, so consider adding it back
                                     % ok = gen_server:call(virtuerl_net, {net_update}),
                                     dets:delete(Table, DomainID),
                                     virtuerl_pubsub:send({domain_deleted, DomainID})
                             end),
                  ok
          end,
    {reply, Res, State};
handle_call({image_from_domain, #{id := DomainID, image_name := ImageName}}, _From, #state{table = Table} = State) ->
    case dets:lookup(Table, DomainID) of
        [] -> gen_server:reply(_From, {error, notfound});
        [{_, Domain}] ->
            spawn(fun() ->
                          #{state := DomState} = Domain,
                          case DomState of
                              running -> domain_stop(DomainID);
                              _ -> ok
                          end,

                          ImgPath = filename:join([virtuerl_mgt:home_path(), "domains", DomainID, "root.qcow2"]),
                          ImgName = string:concat(ImageName, ".qcow2"),
                          DestPath = filename:join(["/tmp/virtuerl", ImgName]),
                          ok = filelib:ensure_dir(DestPath),
                          {ok, _} = file:copy(ImgPath, DestPath),
                          ok = file:rename(DestPath, filename:join([virtuerl_mgt:home_path(), ImgName])),

                          case DomState of
                              running -> domain_start(DomainID);
                              _ -> ok
                          end,

                          gen_server:reply(_From, ok)
                  end)
    end,
    {noreply, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({nodedown, _Node}, State) ->
    {noreply, State};
handle_info({nodeup, _Node}, State) ->
    {noreply, State, {continue, sync_domains}};
handle_info({'DOWN', _, process, Pid, Reason}, #state{table = Table, idmap = IdMap} = State) ->
    NewIdMap = case IdMap of
                   #{Pid := DomId} ->
                       [{DomId, Domain}] = dets:lookup(Table, DomId),
                       ok = dets:insert(Table, {DomId, Domain#{state => stopped}}),
                       ok = dets:sync(Table),
                       maps:remove(Pid, IdMap);
                   #{} ->
                       ?LOG_WARNING(#{module => ?MODULE, msg => "process down but not in registry", pid => Pid, reason => Reason}),
                       IdMap
               end,
    {noreply, State#state{idmap = NewIdMap}};
handle_info(Info, State) ->
    ?LOG_NOTICE(#{module => ?MODULE, msg => "unhandled info message", info => Info}),
    {noreply, State}.


terminate(_Reason, #state{table = Table}) ->
    dets:close(Table),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
