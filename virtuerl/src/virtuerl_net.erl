%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_net).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([parse_cidr/1, format_ip/1, format_ip_bitstring/1, parse_ip/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  ?LOG_INFO(#{what => "Started", who => virtuerl_net}),
  {ok, Table} = dets:open_file(vms, []),
  update_net(Table),
  % TODO: erlexec: spawn bird -f
  {ok, {Table}}.

terminate(_Reason, {Table}) ->
  dets:close(Table).

handle_call({vm_create, Conf}, _From, State) ->
  {reply, ok, State}.

handle_cast({net_update}, State) ->
  {Table} = State,
  update_net(Table),
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

update_net(Table) ->
  % 1. write config
  %%  for VM in VMs:
  %%    append VM.IP to static routes: VM.IP via $VM.network.bridge
  % 2. birdc configure
  % 3. profit?
  Domains = dets:match_object(Table, '_'),
  [io:format("~p~n", [Domain]) || Domain <- Domains],
  reload_net(Table).

-record(domain, {id, network_id, network_addr, ipv4_addr, tap_name}).

handle_interface(If, Table) ->
  %% 1. Delete all devices without an address set
  Addrs = maps:get(<<"addr_info">>, If, []),
  case Addrs of
    [] -> os:cmd(io_lib:format("ip addr del ~p", [maps:get(<<"ifname">>, If)]))
  end.

get_cidr(If) ->
  Addrs = maps:get(<<"addr_info">>, If, []),
  Ifname = maps:get(<<"ifname">>, If),
  case Addrs of
    [] -> {unset, Ifname};
    [Something] -> {{parse_ip(maps:get(<<"local">>, Something)), maps:get(<<"prefixlen">>, Something)}, Ifname}
  end.

reload_net(Table) ->
  Output = os:cmd("ip -j addr"),
  {ok, JSON} = thoas:decode(Output),
  io:format("~p~n", [JSON]),
  Matched = maps:from_list([get_cidr(L) || L <- JSON, startswith(maps:get(<<"ifname">>, L), <<"verlbr">>)]),
%%  lists:foreach(fun(L) -> handle_interface(L, Table) end, Matched),
  io:format("Actual: ~p~n", [Matched]),
  Domains = dets:match_object(Table, '_'),

  TargetAddrs = sets:from_list([{bridge_addr(Addr), Prefixlen} || {_, #domain{network_addr = {Addr, Prefixlen}}} <- Domains]),
  io:format("Target: ~p~n", [sets:to_list(TargetAddrs)]),
  sync_networks(Matched, TargetAddrs),
  sync_taps(Domains),

  update_bird_conf(Domains),
  ok.

bridge_addr(<<Addr/binary>>) ->
  BitSize = bit_size(Addr),
  <<AddrInt:BitSize>> = Addr,
  <<(AddrInt+1):BitSize>>.

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

format_bird_route(<<IP/binary>>) ->
  io_lib:format("~s/~B", [format_ip(IP), bit_size(IP)]).

build_routes(M) when is_map(M) -> build_routes(maps:to_list(M));
build_routes([]) -> [];
build_routes([{Addr, Bridge}|L]) ->
  [io_lib:format("  route ~s via \"~s\";", [format_bird_route(Addr), Bridge]) | build_routes(L)].

update_bird_conf(Domains) ->
  Output = os:cmd("ip -j addr"),
  {ok, JSON} = thoas:decode(Output),
  Bridges = maps:from_list([get_cidr(L) || L <- JSON, startswith(maps:get(<<"ifname">>, L), <<"verlbr">>)]),
  AddrMap = maps:from_list([{Addr, {bridge_addr(NetAddr), Prefixlen}} || {_, #domain{network_addr = {NetAddr, Prefixlen}, ipv4_addr = Addr}} <- Domains]),
  AddrToBridgeMap = maps:map(fun (_, Net) -> maps:get(Net, Bridges) end, AddrMap),

  io:format("DOMAINS: ~p~n", [Domains]),
  io:format("AddrToBridgeMap: ~p~n", [AddrToBridgeMap]),
  file:write_file("birderl.conf", lists:join("\n",
    ["protocol static {", "  ipv4;"] ++ build_routes(AddrToBridgeMap) ++ ["}\n"] )),
  reload_bird(Domains).

reload_bird(Domains) ->
  ok.

sync_networks(ActualAddrs, TargetAddrs) ->
  Ifnames = sets:from_list([Name || {_, Name} <- maps:to_list(ActualAddrs)]),
  ToDelete = maps:without(sets:to_list(TargetAddrs), ActualAddrs),
  ToAdd = sets:subtract(TargetAddrs, sets:from_list(maps:keys(ActualAddrs))),
  io:format("TO DELETE: ~p~n", [ToDelete]),
  io:format("TO ADD: ~p~n", [sets:to_list(ToAdd)]),
  maps:foreach(fun (_, V) ->
    Cmd = io_lib:format("ip link del ~s~n", [V]),
    io:format(Cmd),
    os:cmd(Cmd)
               end, ToDelete),
  add_bridges(sets:to_list(ToAdd), Ifnames).

add_bridges([], _) ->
  ok;
add_bridges([{Addr,Prefixlen}|T], Ifnames) ->
  Ifname = generate_unique_bridge_name(Ifnames),
  Cmd = io_lib:format("ip link add name ~s type bridge~nip addr add ~s/~B dev ~s~n", [Ifname, format_ip(Addr), Prefixlen, Ifname]),
  io:format(Cmd),
  os:cmd(Cmd),
  add_bridges(T, Ifnames),
  ok.


generate_unique_bridge_name(Ifnames) ->
  Ifname = io_lib:format("verlbr~s", [binary:encode_hex(<<(rand:uniform(16#ffffff)):24>>)]),
  case sets:is_element(Ifname, Ifnames) of
    false ->
      Ifname;
    true ->
      generate_unique_bridge_name(Ifnames)
  end.


sync_taps(Domains) ->
  Output = os:cmd("ip -j addr"),
  {ok, JSON} = thoas:decode(Output),
  Bridges = maps:from_list([get_cidr(L) || L <- JSON, startswith(maps:get(<<"ifname">>, L), <<"verlbr">>)]),

  OutputTaps = os:cmd("ip -j link"),
  {ok, JSONTaps} = thoas:decode(OutputTaps),
  io:format("TAPS: ~p~n", [JSONTaps]),
  TapsActual = sets:from_list([maps:get(<<"ifname">>, L) || L <- JSONTaps, startswith(maps:get(<<"ifname">>, L), <<"verltap">>)]),
  TapsTarget = sets:from_list([TapName || {_, #domain{tap_name = TapName}} <- Domains]),
  io:format("Taps to add: ~p~n", [sets:to_list(TapsTarget)]),
  TapsMap = maps:from_list([{Tap, {bridge_addr(Addr), Prefixlen}} || {_, #domain{network_addr = {Addr, Prefixlen}, tap_name = Tap}} <- Domains]),

  TapsToDelete = sets:subtract(TapsActual, TapsTarget),
  lists:foreach(fun (E) ->
    Cmd = io_lib:format("ip link del ~s~n", [E]),
    io:format(Cmd),
    os:cmd(Cmd)
end, sets:to_list(TapsToDelete)),

  TapsToAdd = sets:subtract(TapsTarget, TapsActual),

  TapsMapsToAdd = maps:map(fun (_, Net) -> maps:get(Net, Bridges) end, maps:with(sets:to_list(TapsToAdd), TapsMap)),
  add_taps(TapsMapsToAdd),

  ok.


add_taps(M) when is_map(M) -> add_taps(maps:to_list(M));
add_taps([]) -> ok;
add_taps([{Tap, Bridge}|T]) ->
  Cmd = io_lib:format("ip tuntap add dev ~s mode tap~nip link set dev ~s master ~s~n", [Tap, Tap, Bridge]),
  io:format(Cmd),
  os:cmd(Cmd),
  add_taps(T).
