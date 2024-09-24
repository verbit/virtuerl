-module(virtuerl_mgt).

-behaviour(gen_server).

-export([start/1,
         start_link/1,
         home_path/0,
         notify/2,
         image_from_domain/2,
         domain_update/1,
         create_vm/0,
         domain_create/1,
         domain_get/1,
         domain_delete/1,
         domain_stop/1,
         domain_start/1,
         domains_list/0, domains_list/1, domains_list/2,
         add_port_fwd/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         handle_continue/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("khepri/include/khepri.hrl").
-include_lib("khepri/src/khepri_error.hrl").

-define(APPLICATION, virtuerl).

-record(state, {cluster, table, idmap, workers}).


notify(Pid, Msg) ->
    gen_server:call(Pid, {notify, Msg}).


create_vm() -> create_vm({default, ?MODULE}).


create_vm(Ref) ->
    gen_server:call(?MODULE, {domain_create, {default}}).


domain_create(Conf) -> domain_create({default, ?MODULE}, Conf).


domain_create(Ref, Conf) ->
    gen_server:call(?MODULE, {domain_create, Conf}, infinity).


domain_delete(Conf) -> domain_delete({default, ?MODULE}, Conf).


domain_delete(Ref, Conf) ->
    gen_server:call(?MODULE, {domain_delete, Conf}, infinity).


domain_get(Conf) -> domain_get({default, ?MODULE}, Conf).


domain_get(Ref, Conf) ->
    gen_server:call(?MODULE, {domain_get, Conf}).


domains_list() -> domains_list({default, ?MODULE}).


domains_list(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, domains_list);
domains_list(Ref) ->
    gen_server:call(?MODULE, domains_list).


domains_list(Ref, WorkerName) ->
    gen_server:call(?MODULE, {domains_list, WorkerName}).


domain_update(Conf) -> domain_update({default, ?MODULE}, Conf).


domain_update(Ref, Conf) ->
    gen_server:call(?MODULE, {domain_update, Conf}).


domain_stop(Id) -> domain_stop({default, ?MODULE}, Id).


domain_stop(Pid, Id) when is_pid(Pid) ->
    gen_server:call(Pid, {domain_update, #{id => Id, state => stopped}});
domain_stop(Ref, Id) ->
    gen_server:call(?MODULE, {domain_update, #{id => Id, state => stopped}}).


domain_start(Id) -> domain_start({default, ?MODULE}, Id).


domain_start(Ref, Id) ->
    gen_server:call(?MODULE, {domain_update, #{id => Id, state => running}}).


image_from_domain(DomainId, ImageName) -> image_from_domain({default, ?MODULE}, DomainId, ImageName).


image_from_domain(Ref, DomainId, ImageName) ->
    gen_server:call(?MODULE, {image_from_domain, #{id => DomainId, image_name => ImageName}}, infinity).


add_port_fwd(DomainId, PortFwd) -> add_port_fwd({default, ?MODULE}, DomainId, PortFwd).


add_port_fwd(Ref, DomainId, PortFwd) ->
    gen_server:call(?MODULE, {add_port_fwd, DomainId, PortFwd}).


-spec domains_list() -> #{}.


-spec add_port_fwd(binary(), #{protos := [tcp | udp], source_port := integer(), target_port := integer()}) -> term().

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

home_path() ->
    application:get_env(?APPLICATION, home, "var").


start(Cluster) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Cluster], []).


start_link(Cluster) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Cluster], []).


init([Cluster]) ->
    pg:monitor(Cluster),
    Pids = pg:get_members(Cluster),
    [ Pid ! {enslave, self()} || Pid <- Pids ],
    {ok, #state{cluster = Cluster, workers = #{}}, {continue, sync_domains}}.


handle_continue(sync_domains, #state{cluster = Cluster} = State) ->
    Pids = pg:get_members(Cluster),
    [ virtuerl_host:sync(Pid) || Pid <- Pids ],
    {noreply, State}.


handle_call({notify, Msg}, _From, State) ->
    virtuerl_pubsub:send(Msg),
    {reply, ok, State};
handle_call({register_name, Name, Pid}, _From, State) ->
    #state{workers = Workers} = State,
    {Res, NewState} = case Workers of
                          #{Name := _} -> {no, State};
                          _ -> {yes, State#state{workers = Workers#{Name => Pid}}}
                      end,
    {reply, Res, NewState, {continue, sync_domains}};

handle_call({domain_create, Conf}, _From, State) ->
    DomainID = virtuerl_util:uuid4(),
    Domain0 = maps:merge(#{
                           id => DomainID,
                           host => localhost,
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
    ok = khepri:create([domain, DomainID], Domain),
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

    {ok, DomainsMap} = khepri:get_many([domain, ?KHEPRI_WILDCARD_STAR]),
    Domains = maps:values(DomainsMap),
    <<A:5, _:3, B:40>> = <<(rand:uniform(16#ffffffffffff)):48>>,
    MacAddr = <<A:5, 1:1, 2:2, B:40>>,

    DomainWithIps = Domain#{
                      network_addrs => Cidrs,
                      mac_addr => MacAddr,
                      ipv4_addr => Ipv4Addr,
                      ipv6_addr => Ipv6Addr,
                      cidrs => IpCidrs
                     },
    ok = khepri:put([domain, DomainID], DomainWithIps),
    ?LOG_NOTICE(#{event => domain_ready, domain => DomainWithIps}),
    virtuerl_pubsub:send({domain_created, DomainID}),

    {reply,
     {ok, maps:merge(#{id => DomainID, mac_addr => binary:encode_hex(MacAddr)},
                     maps:map(fun(_, V) -> iolist_to_binary(virtuerl_net:format_ip(V)) end, AddressesMap))},
     State,
     {continue, sync_domains}};
handle_call({domain_update, #{id := DomainID} = DomainUpdate0}, _From, State) ->
    DomainUpdate = maps:remove(id, DomainUpdate0),
    Reply = case khepri:get([domain, DomainID]) of
                {ok, Domain} ->
                    ok = khepri:put([domain, DomainID], maps:merge(Domain, DomainUpdate)),
                    virtuerl_pubsub:send({domain_updated, DomainID}),
                    ok;
                _ -> {error, notfound}
            end,
    {reply, Reply, State, {continue, sync_domains}};
handle_call({add_port_fwd, DomId, PortFwd}, _From, State) ->
    Reply = case khepri:get([domain, DomId]) of
                {ok, Domain} ->
                    NewFwds = case Domain of
                                  #{port_fwds := Fwds} -> Fwds;
                                  _ -> []
                              end,
                    DomainUpdate = #{port_fwds => [PortFwd | NewFwds]},
                    ok = khepri:put([domain, DomId], maps:merge(Domain, DomainUpdate)),
                    virtuerl_pubsub:send({domain_updated, DomId}),
                    ok;
                _ -> {error, notfound}
            end,
    {reply, Reply, State, {continue, sync_domains}};
handle_call(domains_list, _From, State) ->
    {ok, DomainsMap} = khepri:get_many([domain, ?KHEPRI_WILDCARD_STAR]),
    Domains = maps:values(DomainsMap),
    {reply, [ maps:merge(#{host => localhost, state => running, name => Id, vcpu => 1, memory => 512}, Domain) || #{id := Id} = Domain <- Domains ], State};
handle_call({domains_list, WorkerName}, _From, State) ->
    {ok, DomainsMap} = khepri:get_many([domain, ?KHEPRI_WILDCARD_STAR]),
    Domains0 = maps:values(DomainsMap),
    Domains1 = [ maps:merge(#{host => localhost, state => running, name => Id, vcpu => 1, memory => 512}, Domain) || #{id := Id} = Domain <- Domains0 ],

    FilteredDomains = [ Dom || #{host := Host} = Dom <- Domains1,
                               case {WorkerName, Host} of
                                   {default, localhost} -> true;
                                   {Name, Name} -> true;
                                   _ -> false
                               end ],

    {reply, FilteredDomains, State};
handle_call({domain_get, #{id := DomainID}}, _From, State) ->
    Reply = case khepri:get([domain, DomainID]) of
                {ok, #{mac_addr := MacAddr} = Domain} ->
                    DomRet = Domain#{
                               mac_addr := binary:encode_hex(MacAddr)
                              },
                    {ok, maps:merge(#{host => localhost, state => running, name => DomainID, vcpu => 1, memory => 512}, DomRet)};
                _ -> notfound
            end,
    {reply, Reply, State};
handle_call({domain_delete, #{id := DomainID}}, _From, State) ->
    case khepri:get([domain, DomainID]) of
        {ok, Domain} ->
            ok = khepri:put([domain, DomainID], Domain#{state => deleting}),
            ok = virtuerl_ipam:unassign(DomainID),  % TODO: would be nice to keep networking as long as the node is shutting down (for debugging)
            ok = khepri:delete([domain, DomainID]),
            virtuerl_pubsub:send({domain_deleted, DomainID}),
            {reply, ok, State, {continue, sync_domains}};
        _ -> {reply, {error, notfound}, State}

    end;
handle_call({image_from_domain, #{id := DomainID, image_name := ImageName}}, _From, State) ->
    case khepri:get([domain, DomainID]) of
        {ok, Domain} ->
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
                  end);
        _ -> gen_server:reply(_From, {error, notfound})
    end,
    {noreply, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({_Ref, join, Cluster, Pids}, #state{cluster = Cluster} = State) ->
    ?LOG_NOTICE(#{joined => Pids}),
    [ Pid ! {enslave, self()} || Pid <- Pids ],
    {noreply, State};
handle_info({_Ref, leave, Cluster, Pids}, #state{cluster = Cluster} = State) ->
    ?LOG_NOTICE(#{left => Pids}),
    #state{workers = Workers} = State,
    PidToName = maps:from_list([ {Pid, Name} || {Name, Pid} <- maps:to_list(Workers) ]),
    NewWorkers = maps:without(Pids, PidToName),
    {noreply, State#state{workers = NewWorkers}};

handle_info(Info, State) ->
    ?LOG_NOTICE(#{module => ?MODULE, msg => "unhandled info message", info => Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
