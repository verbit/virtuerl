-module(virtuerl_ipam).

-behavior(gen_server).

-export([init/1,
         handle_call/3,
         subnet/1,
         get_range/1,
         get_next/6,
         terminate/2,
         ipam_next_ip/1,
         start_server/1,
         stop_server/1,
         ipam_put_net/1,
         start_link/0,
         handle_cast/2,
         ipam_delete_net/1,
         ipam_create_net/1,
         ipam_list_nets/0,
         ipam_get_net/1,
         ipam_put_ip/3,
         assign_next/3,
         unassign/1]).

-include_lib("khepri/include/khepri.hrl").
-include_lib("khepri/src/khepri_error.hrl").

-record(network, {last_insert, from, to, address, prefixlen}).


-spec ipam_create_net([{binary(), integer()}]) -> {ok, binary()}.
ipam_create_net(NetworkDef) ->
    ID = virtuerl_util:uuid4(),
    ipam_put_net({ID, NetworkDef}).


ipam_put_net(NetworkDef) ->
    case gen_server:call(ipam, {net_put, NetworkDef}) of
        ok ->
            {ok, NetworkDef};
        Other ->
            Other
    end.


-spec ipam_list_nets() -> {ok, #{binary() => #{cidr4 | cidr6 := #{address := nonempty_binary(), prefixlen := integer()}}}}.
ipam_list_nets() ->
    gen_server:call(ipam, net_list).


ipam_delete_net(ID) when is_list(ID) ->
    ipam_delete_net(list_to_binary(ID));
ipam_delete_net(ID) ->
    case gen_server:call(ipam, {net_delete, ID}) of
        {ok, Res} ->
            Res;
        Other ->
            Other
    end.


ipam_get_net(Id) ->
    case gen_server:call(ipam, {net_get, Id}) of
        {ok, Pools} ->
            io:format("POOLS: ~p~n", [Pools]),
            Cidrs = [ iolist_to_binary([Address, "/", integer_to_binary(Prefixlen)])
                      || #{address := Address, prefixlen := Prefixlen} <- maps:values(Pools) ],
            Res = #{id => Id, cidrs => Cidrs},
            {ok, Res};
        Other ->
            io:format("Error ~p~n", [Other]),
            Other
    end.


ipam_put_ip(NetworkName, IP, DomainId) ->
    gen_server:call(ipam, {ip_put, NetworkName, IP, DomainId}).


assign_next(NetworkID, Tag, VMID) ->
    case gen_server:call(ipam, {ip_next, NetworkID, Tag, VMID}) of
        {ok, Res} ->
            Res;
        Other ->
            Other
    end.


unassign(DomainId) ->
    gen_server:call(ipam, {unassign, DomainId}).


ipam_next_ip(NetworkName) ->
    case gen_server:call(ipam, {ip_next, NetworkName}) of
        {ok, Res} ->
            Res;
        Other ->
            Other
    end.


%% entry point by child spec
start_link() ->
    io:format("IPAM: start_link~n"),
    gen_server:start_link({local, ipam}, ?MODULE, [], []).


init([]) ->
    io:format("starting IPAM service~n"),
    StoreId = khepri_cluster:get_default_store_id(),
    {ok, StoreId} = khepri:start(filename:join(virtuerl_mgt:home_path(), "khepri")),
    case application:get_env(khepri_bootstrap) of
        {ok, Node} -> ok = khepri_cluster:join({StoreId, Node});
        undefined -> ok
    end,
    init([StoreId]);
init([StoreId]) ->
    {ok, StoreId}.


terminate(_Reason, StoreId) ->
    khepri:stop(StoreId).


handle_call(net_list, _From, StoreId) ->
    case khepri:get_many(StoreId, [network, ?KHEPRI_WILDCARD_STAR, ?KHEPRI_WILDCARD_STAR]) of
        {ok, Map} ->
            ToMapKey = fun(Tag) -> case Tag of ipv4 -> cidr4; ipv6 -> cidr6 end end,
            Res0 = [ {NetworkId, ToMapKey(Tag), #{address => virtuerl_net:format_ip_bitstring(Address), prefixlen => PrefixLen}}
                     || {[network, NetworkId, Tag], #network{address = Address, prefixlen = PrefixLen}} <- maps:to_list(Map) ],
            Res1 = lists:foldl(
                     fun({NetworkId, Tag, Def}, MapAcc) ->
                             maps:update_with(NetworkId, fun(L) -> [{Tag, Def} | L] end, [{Tag, Def}], MapAcc)
                     end,
                     #{},
                     Res0),
            %% Res1 = maps:groups_from_list(fun({NetworkId, _, _}) -> NetworkId end, fun({_, Tag, Def}) -> {Tag, Def} end, Res0),
            Res = maps:map(fun(_, V) -> maps:from_list(V) end, Res1),
            {reply, {ok, Res}, StoreId};
        Res -> {reply, Res, StoreId}
    end;
handle_call({net_get, NetworkId}, _From, StoreId) ->
    case khepri:get_many(StoreId, [network, NetworkId, ?KHEPRI_WILDCARD_STAR]) of
        {ok, Map} ->
            ToMapKey = fun(Tag) -> case Tag of ipv4 -> cidr4; ipv6 -> cidr6 end end,
            Res0 = [ {NetworkId, ToMapKey(Tag), #{address => virtuerl_net:format_ip_bitstring(Address), prefixlen => PrefixLen}}
                     || {[network, NetworkId, Tag], #network{address = Address, prefixlen = PrefixLen}} <- maps:to_list(Map) ],
            Res1 = lists:foldl(fun({NetworkId, Tag, Def}, MapAcc) ->
                                       maps:update_with(NetworkId, fun(L) -> [{Tag, Def} | L] end, [{Tag, Def}], MapAcc)
                               end,
                               #{},
                               Res0),
            %% Res1 = maps:groups_from_list(fun({NetworkId, _, _}) -> NetworkId end, fun({_, Tag, Def}) -> {Tag, Def} end, Res0),
            #{NetworkId := Res} = maps:map(fun(_, V) -> maps:from_list(V) end, Res1),
            {reply, {ok, Res}, StoreId};
        Res -> {reply, Res, StoreId}
    end;
handle_call({net_put, Network}, _From, StoreId) ->
    {ID, Defs} = Network,
    Ranges = [ get_range({Address, PrefixLen}) || {Address, PrefixLen} <- Defs ],
    TooSmallNets = [ too_small || {From, To} <- Ranges, To - From =< 8 ],
    case TooSmallNets of
        [] ->
            InsertNet = fun({Address, Prefixlen}) ->
                                {From, To} = get_range({Address, Prefixlen}),
                                BitLength = bit_size(Address),
                                Tag = case BitLength of
                                          32 -> ipv4;
                                          128 -> ipv6
                                      end,
                                ok = khepri:put(StoreId,
                                                [network, ID, Tag],
                                                #network{
                                                  address = Address,
                                                  prefixlen = Prefixlen,
                                                  from = <<(From + 8):BitLength>>,
                                                  to = <<To:BitLength>>,
                                                  last_insert = <<(From + 8):BitLength>>
                                                 })
                        end,
            lists:foreach(InsertNet, Defs),
            {reply, {ok, ID}, StoreId};
        _ ->
            {reply, {error, network_too_small}, StoreId}
    end;
handle_call({net_delete, ID}, _From, StoreId) ->
    Ret = khepri:delete(StoreId, [network, ID]),
    {reply, Ret, StoreId};

handle_call({ip_next, NetworkID, Tag, DomainID}, _From, StoreId) ->
    R = khepri:transaction(
          StoreId,
          fun() ->
                  case khepri_tx:get([network, NetworkID, Tag]) of
                      {ok, Network} ->
                          #network{
                            address = Address,
                            prefixlen = PrefixLen,
                            last_insert = LastInsertBin,
                            from = FromBin,
                            to = ToBin
                           } = Network,
                          BitLength = bit_size(LastInsertBin),
                          <<LastInsert:BitLength>> = LastInsertBin,
                          <<From:BitLength>> = FromBin,
                          <<To:BitLength>> = ToBin,
                          {ok, Map} = khepri_tx:get_many([network, NetworkID, Tag, #if_name_matches{regex = any}]),
                          NextIP = case LastInsert of
                                       undefined ->
                                           get_next(Map, [network, NetworkID, Tag], BitLength, From, To, From);
                                       Payload ->
                                           get_next(Map, [network, NetworkID, Tag], BitLength, From, To, Payload)
                                   end,
                          case NextIP of
                              {ok, IP} ->
                                  khepri_tx:put([network, NetworkID, Tag, <<IP:BitLength>>], DomainID),
                                  {ok, {Address, PrefixLen}, <<IP:BitLength>>};
                              Other ->
                                  Other
                          end;
                      {error, ?khepri_error(node_not_found, _)} ->
                          {error, network_not_found}
                  end
          end),
    {reply, R, StoreId};

handle_call({ip_put, NetworkId, IpAddr, DomainId}, _From, StoreId) ->
    Tag = case bit_size(IpAddr) of
              32 -> ipv4;
              128 -> ipv6
          end,
    R = case khepri:get([network, NetworkId, Tag]) of
            {ok, Network} ->
                #network{address = Address, prefixlen = PrefixLen} = Network,
                case khepri:create(StoreId, [network, NetworkId, Tag, IpAddr], DomainId) of
                    ok ->
                        {ok, {Address, PrefixLen}};
                    Other -> Other
                end;
            {error, ?khepri_error(node_not_found, _)} ->
                {error, network_not_found}
        end,
    {reply, R, StoreId};

handle_call({ip_delete, NetworkId, IpAddr}, _From, StoreId) ->
    R = khepri:delete(StoreId, [network, NetworkId, ?KHEPRI_WILDCARD_STAR, IpAddr]),
    {reply, R, StoreId};

handle_call({unassign, DomainId}, _From, StoreId) ->
    ok = khepri:delete_many(StoreId, [network, ?KHEPRI_WILDCARD_STAR, ?KHEPRI_WILDCARD_STAR, #if_data_matches{pattern = DomainId}]),
    {reply, ok, StoreId};

handle_call({ip_clear}, _From, StoreId) ->
    R = khepri:delete(StoreId, [network]),
    {reply, R, StoreId}.


handle_cast(Request, State) ->
    erlang:error(not_implemented).


start_server(StoreId) ->
    gen_server:start_link({local, ipam}, ?MODULE, [StoreId], []).


stop_server(Pid) ->
    exit(Pid, normal).


%% internal functions
get_next(Map, Path, BitLength, From, To, Start) ->
    get_next(Map, Path, BitLength, From, To, Start, Start).


get_next(Map, Path, BitLength, From, To, OriginalStart, Start) ->
    case maps:is_key(Path ++ [<<Start:BitLength>>], Map) of
        true ->
            Next = case Start of
                       To -> From;  % wrap around
                       _ -> Start + 1
                   end,
            case Next of
                OriginalStart ->
                    {error, no_ip_available};
                _ ->
                    get_next(Map, Path, BitLength, From, To, OriginalStart, Next)
            end;
        false ->
            {ok, Start}
    end.


get_range({_Address, PrefixLen}) ->
    case _Address of
        <<Address:128>> ->
            io:format("IPv6~n", []),
            get_range({Address, 128, PrefixLen});
        <<Address:32>> ->
            io:format("IPv4~n", []),
            get_range({Address, 32, PrefixLen})
    end;

get_range({Address, AddressLength, PrefixLen}) ->
    ShiftAmount = AddressLength - PrefixLen,
    From = (Address bsr ShiftAmount) bsl ShiftAmount,
    To = From + (1 bsl ShiftAmount) - 1,
    io:format("From ~p to ~p~n", [From, To]),
    io:format("From ~p to ~p~n", [<<From:AddressLength>>, <<To:AddressLength>>]),
    {From, To}.


subnet({_Address, PrefixLen}) ->
    get_range({_Address, PrefixLen}).
