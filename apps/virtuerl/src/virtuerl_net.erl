-module(virtuerl_net).

-behaviour(gen_server).

-export([start_link/0, update_net/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([parse_cidr/1, format_cidr/1, parse_ip/1, format_ip/1, format_ip_bitstring/1, bridge_addr/1, bridge_addr/2, normalize_net/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


update_net(Node, Domains) ->
    gen_server:call({?SERVER, Node}, {net_update, Domains}, infinity).


init([]) ->
    ?LOG_INFO(#{what => "Started", who => virtuerl_net}),
    process_flag(trap_exit, true),
    {ok, {}}.


terminate(_Reason, _State) ->
    {ok, IfAddrs} = inet:getifaddrs(),
    Ifnames = [ Name || {Name, _} <- IfAddrs ],
    DeleteCmds = [ ["link del ", Ifname, "\n"] || Ifname <- Ifnames, startswith(Ifname, "verl") ],
    run_batch(DeleteCmds),
    ok.


handle_call({net_update, Domains}, _From, State) ->
    reload_net(Domains),
    {reply, ok, State}.


handle_cast({net_update, Domains}, State) ->
    reload_net(Domains),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


startswith(Str, Pre) ->
    case string:prefix(Str, Pre) of
        nomatch -> false;
        _ -> true
    end.


-spec get_cidrs(term) -> {[], binary()}.
get_cidrs(If) ->
    Addrs = maps:get(<<"addr_info">>, If, []),
    Ifname = maps:get(<<"ifname">>, If),
    case Addrs of
        [] -> {unset, Ifname};
        AddrInfos when is_list(AddrInfos) ->
            Cidrs = [ iolist_to_binary([Ip, "/", integer_to_binary(Prefixlen)])
                      || #{<<"local">> := Ip, <<"prefixlen">> := Prefixlen, <<"scope">> := <<"global">>} <- AddrInfos ],
            {lists:sort(Cidrs), Ifname}
    end.


reload_net(Domains0) ->
    Domains = [ {Id, Dom} || #{id := Id} = Dom <- Domains0 ],
    Output = os:cmd("ip -j addr"),
    {ok, JSON} = thoas:decode(Output),
    % io:format("~p~n", [JSON]),
    Matched = maps:from_list([ get_cidrs(L) || L <- JSON, startswith(maps:get(<<"ifname">>, L), <<"verlbr">>) ]),
    %%  lists:foreach(fun(L) -> handle_interface(L, Table) end, Matched),
    % io:format("Actual: ~p~n", [Matched]),

    TargetAddrs = sets:from_list([ lists:sort(network_cidrs_to_bride_cidrs(Cidrs)) || {_, #{network_addrs := Cidrs}} <- Domains ]),
    % io:format("Target: ~p~n", [sets:to_list(TargetAddrs)]),
    update_nftables(Domains),
    sync_taps(Matched, TargetAddrs, Domains),

    ok.


update_nftables(Domains) ->
    BridgeAddrs = lists:uniq(lists:flatten([ network_cidrs_to_bride_addrs(Cidrs) || {_, #{network_addrs := Cidrs}} <- Domains ])),
    BridgeAddrsTyped = lists:map(fun(Addr) ->
                                         case binary:match(Addr, <<":">>) of
                                             nomatch -> {ipv4, Addr};
                                             _ -> {ipv6, Addr}
                                         end
                                 end,
                                 BridgeAddrs),

    DnsRules = [ case Family of
                     ipv4 ->
                         ["        ip daddr ", Addr, " ", Prot, " dport 53 dnat to ", Addr, ":5354\n"];
                     ipv6 ->
                         ["        ip6 daddr ", Addr, " ", Prot, " dport 53 dnat to [", Addr, "]:5354\n"]
                 end
                 || {Family, Addr} <- BridgeAddrsTyped, Prot <- ["tcp", "udp"] ],

    ToIpString =
        fun(#{source_port := SourcePort, target_port := TargetPort, protos := Protos}, {Ip, _}) ->
                {Tag, IpStr} = case bit_size(Ip) of
                                   32 -> {"ipv4", ["ip to ", virtuerl_net:format_ip(Ip)]};
                                   128 -> {"ipv6", ["ip6 to [", virtuerl_net:format_ip(Ip), "]"]}
                               end,
                ["        iifname != \"verlbr*\" oifname != \"verlbr*\" meta nfproto ", Tag, " meta l4proto {", string:join(lists:map(fun atom_to_list/1, Protos), ", "), "} th dport ", integer_to_list(SourcePort), " dnat ", IpStr, ":", integer_to_list(TargetPort), "\n"]
        end,
    PortFwdRules = [ [ ToIpString(PortFwd, Cidr)
                       || PortFwd <- PortFwds, Cidr <- Cidrs ]
                     || {_Id, #{cidrs := Cidrs, port_fwds := PortFwds}} <- Domains ],
    io:format("PortFwd: ~p~n", [PortFwdRules]),

    ToRule =
        fun(#{protocols := Protos, target_ports := Ports}, Cidrs) ->
                ProtosStr = string:join(Protos, ","),
                Ports0 = [ case Port of
                               Num when is_integer(Num) ->
                                   integer_to_list(Num);
                               Str when is_list(Str) orelse is_binary(Str) ->
                                   Str
                           end
                           || Port <- Ports ],  % FIXME: validate ports?
                PortsStr = ["{", string:join(Ports0, ","), "}"],

                TagIp = fun({Ip, _}) ->
                                Tag = case bit_size(Ip) of
                                          32 ->
                                              "ip";
                                          128 ->
                                              "ip6"
                                      end,
                                {Tag, Ip}
                        end,
                TaggedIps = lists:map(TagIp, Cidrs),

                [ ["        meta l4proto {", ProtosStr, "} ", Tag, " daddr ", virtuerl_net:format_ip(Ip), " th dport ", PortsStr, " accept\n"]
                  || {Tag, Ip} <- TaggedIps ]
        end,
    ForwardRules = [ ToRule(InboundRule, Cidrs) || {_Id, #{cidrs := Cidrs, inbound_rules := InboundRules0}} <- Domains, InboundRule <- InboundRules0 ],
    % lists:flatten(List),

    IoList =
        ["table inet virtuerl\ndelete table inet virtuerl\n\n",
         "table inet virtuerl {\n",
         "    chain input {\n",
         "        type filter hook input priority filter; policy accept;\n",
         "        ct state established,related accept\n",
         ForwardRules,
         "        oifname \"verlbr*\" reject\n",
         "    }\n",
         "\n",
         "    chain forward {\n",
         "        type filter hook forward priority filter; policy accept;\n",
         "        iifname \"verlbr*\" accept\n",
         "        ct state established,related accept\n",
         ForwardRules,
         "        oifname \"verlbr*\" reject\n",
         "    }\n",
         "\n",
         "\n",
         "\n",
         "\n",
         "    chain output {\n",
         "        type nat hook output priority -105; policy accept;\n",
         DnsRules,
         "    }\n",

         "    chain prerouting {\n",
         "        type nat hook prerouting priority dstnat - 5; policy accept;\n",
         DnsRules,
         PortFwdRules,
         "    }\n",

         "    chain postrouting {\n",
         "        type nat hook postrouting priority -5; policy accept;\n",
         "        iifname \"verlbr*\" ip saddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } ip daddr != { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } masquerade\n",
         "        iifname \"verlbr*\" ip6 saddr fc00::/7 ip6 daddr != fc00::/7 masquerade\n",
         "    }\n",
         "}\n"],
    % io:format("~s~n", [IoList]),

    run_nft(IoList).


network_cidrs_to_bride_cidrs(Cidrs) ->
    lists:map(fun(Cidr) ->
                      {Addr, Prefixlen} = parse_cidr(Cidr),
                      iolist_to_binary(io_lib:format("~s/~B", [format_ip(bridge_addr(Addr)), Prefixlen]))
              end,
              Cidrs).


network_cidrs_to_bride_addrs(Cidrs) ->
    lists:map(fun(Cidr) ->
                      {Addr, _} = parse_cidr(Cidr),
                      iolist_to_binary(io_lib:format("~s", [format_ip(bridge_addr(Addr))]))
              end,
              Cidrs).


bridge_addr(<<Addr/binary>>) ->
    BitSize = bit_size(Addr),
    <<AddrInt:BitSize>> = Addr,
    <<(AddrInt + 1):BitSize>>.


bridge_addr(<<Addr/binary>>, Prefixlen) ->
    <<Prefix:Prefixlen, Rest/bits>> = Addr,
    <<Prefix:Prefixlen, 1:(bit_size(Rest))>>.


normalize_net({<<Addr/binary>>, Prefixlen}) ->
    <<Prefix:Prefixlen, Rest/bits>> = Addr,
    {<<Prefix:Prefixlen, 0:(bit_size(Rest))>>, Prefixlen}.


parse_cidr(<<CIDR/binary>>) -> parse_cidr(binary_to_list(CIDR));
parse_cidr(CIDR) ->
    [IP, Prefixlen] = string:split(CIDR, "/", trailing),
    {I, _} = string:to_integer(Prefixlen),
    {parse_ip(IP), I}.


parse_ip(<<IP/binary>>) -> parse_ip(binary_to_list(IP));
parse_ip(IP) ->
    {ok, Res} = inet:parse_address(IP),
    case Res of
        {A, B, C, D} -> <<A:8, B:8, C:8, D:8>>;
        {A, B, C, D, E, F, G, H} -> <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>
    end.


format_ip(<<A:8, B:8, C:8, D:8>>) ->
    inet:ntoa({A, B, C, D});
format_ip(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    inet:ntoa({A, B, C, D, E, F, G, H}).


format_ip_bitstring(IP) ->
    list_to_binary(format_ip(IP)).


format_cidr({<<Addr/binary>>, Prefixlen}) ->
    io_lib:format("~s/~B", [format_ip(Addr), Prefixlen]).


generate_unique_bridge_name(Ifnames) ->
    Ifname = io_lib:format("verlbr~s", [binary:encode_hex(<<(rand:uniform(16#ffffff)):24>>)]),
    case sets:is_element(Ifname, Ifnames) of
        false ->
            Ifname;
        true ->
            generate_unique_bridge_name(Ifnames)
    end.


to_vtap_mac(MacAddr) ->
    <<A:5, _:1, 2:2, B:40>> = MacAddr,
    <<A:5, 0:1, 2:2, B:40>>.


-spec sync_taps(#{[term()] => binary()}, term(), term()) -> ok.
sync_taps(ActualAddrs, TargetAddrs, Domains) ->
    Ifnames = sets:from_list([ Name || {_, Name} <- maps:to_list(ActualAddrs) ]),
    ToDelete = maps:without(sets:to_list(TargetAddrs), ActualAddrs),
    ToAdd = sets:subtract(TargetAddrs, sets:from_list(maps:keys(ActualAddrs))),
    ?LOG_DEBUG("TO DELETE: ~p~n", [ToDelete]),
    ?LOG_DEBUG("TO ADD: ~p~n", [sets:to_list(ToAdd)]),
    BrDeleteCmds = [ io_lib:format("link del ~s~n", [BridgeName]) || BridgeName <- maps:values(ToDelete) ],
    BrNameCidrsMap = maps:from_list([ {Cidrs, generate_unique_bridge_name(Ifnames)} || Cidrs <- sets:to_list(ToAdd) ]),
    BrAddCmds = maps:values(maps:map(fun(Cidrs, Ifname) ->
                                             AddrAddCmd = [ io_lib:format("addr add ~s dev ~s~n", [Cidr, Ifname]) || Cidr <- Cidrs ],
                                             [io_lib:format("link add name ~s type bridge~nlink set ~s up~n", [Ifname, Ifname]), AddrAddCmd]
                                     end,
                                     BrNameCidrsMap)),

    Bridges = maps:merge(maps:with(sets:to_list(TargetAddrs), ActualAddrs), BrNameCidrsMap),

    OutputTaps = os:cmd("ip -j link"),
    {ok, JSONTaps} = thoas:decode(OutputTaps),
    % io:format("TAPS: ~p~n", [JSONTaps]),
    TapsActual = sets:from_list([ maps:get(<<"ifname">>, L) || L <- JSONTaps, startswith(maps:get(<<"ifname">>, L), <<"verltap">>) ]),
    TapsTarget = sets:from_list([ iolist_to_binary(TapName) || {_, #{tap_name := TapName}} <- Domains ]),  % TODO: persist tap_name as binary
    % io:format("Taps Target: ~p~n", [sets:to_list(TapsTarget)]),
    % io:format("Taps Actual: ~p~n", [sets:to_list(TapsActual)]),
    TapsMap = maps:from_list([ {iolist_to_binary(Tap), {to_vtap_mac(MacAddr), network_cidrs_to_bride_cidrs(Cidrs)}}
                               || {_, #{network_addrs := Cidrs, tap_name := Tap, mac_addr := MacAddr}} <- Domains ]),
    % io:format("TapsMap: ~p~n", [TapsMap]),

    TapsToDelete = sets:subtract(TapsActual, TapsTarget),
    DeleteCmds = [ io_lib:format("link del ~s~n", [TapName]) || TapName <- sets:to_list(TapsToDelete) ],

    TapsToAdd = sets:subtract(TapsTarget, TapsActual),

    TapsMapsToAdd = maps:map(fun(_, {MacAddr, Net}) -> {MacAddr, maps:get(Net, Bridges)} end, maps:with(sets:to_list(TapsToAdd), TapsMap)),
    AddCmds = add_taps(TapsMapsToAdd),

    BatchFileContents = [BrDeleteCmds, BrAddCmds, DeleteCmds, AddCmds],

    run_batch(BatchFileContents).


run_nft(IoList) ->
    ?LOG_DEBUG(#{what => run_batch, batch => IoList}),

    Path = iolist_to_binary(["/tmp/virtuerl/", "nftables_", virtuerl_util:uuid4(), ".conf"]),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, IoList),

    NftOut = os:cmd(io_lib:format("nft -f ~s", [Path])),
    case NftOut of
        "" -> ok;
        _ -> error({nft_error, NftOut})
    end,
    file:delete(Path),
    ok.


run_batch(Contents) ->
    ?LOG_DEBUG(#{what => run_batch, batch => Contents}),

    Path = iolist_to_binary(["/tmp/virtuerl/", "iproute2_batch_", virtuerl_util:uuid4()]),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Contents),

    IpRoute2 = os:cmd(io_lib:format("ip -b ~s", [Path])),
    case IpRoute2 of
        "" -> ok;
        _ -> error({iproute2_error, IpRoute2})
    end,
    file:delete(Path),
    ok.


add_taps(M) when is_map(M) -> add_taps(maps:to_list(M));
add_taps([]) -> "";
add_taps([{Tap, {Mac, Bridge}} | T]) ->
    MacAddrString = virtuerl_util:mac_to_str(Mac),
    Cmd = io_lib:format("tuntap add dev ~s mode tap~nlink set dev ~s address ~s master ~s~nlink set ~s up~n",
                        [Tap, Tap, MacAddrString, Bridge, Tap]),
    [Cmd, add_taps(T)].
