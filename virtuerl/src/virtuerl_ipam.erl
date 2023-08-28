%%%-------------------------------------------------------------------
%% @doc virtuerl public API
%% @end
%%%-------------------------------------------------------------------

-module(virtuerl_ipam).

-behavior(gen_server).

-export([req/1, init/1, handle_call/3, subnet/1, get_range/1, get_next/6, terminate/2, ipam_put_ip/2, ipam_next_ip/1, start_server/1, stop_server/1, ipam_put_net/1, start_link/0, handle_cast/2, assign_next/2, ipam_delete_net/1, ipam_create_net/1, ipam_list_nets/0]).

-include_lib("khepri/include/khepri.hrl").
-include_lib("khepri/src/khepri_error.hrl").

req(Msg) ->
	%kh ! {self(), Msg},
	%receive
	%	Res ->
	%		io:format("Received ~p~n", [Res])
	%end.
	%gen_server:call(virtuerl, {ip_put, default, <<"192.168.122.0/24">>, <<"192.168.122.10">>}).
	%gen_server:call(virtuerl, {ip_next, default, <<"192.168.122.0/24">>}).
	case Msg of
		net_put ->
			gen_server:call(ipam, {net_put, default, {<<"abcdef">>, <<192:8,168:8,122:8,0:8>>, 24}});
		ip_put ->
			gen_server:call(ipam, {ip_put, default, <<"192.168.122.0/24">>, <<"192.168.122.0/28">>, <<"192.168.122.12">>});
		ip_next ->
			gen_server:call(ipam, {ip_next, <<"abcdef">>});
		ip_clear ->
			gen_server:call(ipam, {ip_clear}),
			io:format("something~p~n", ['t'])
	end.


-record(network, {last_insert, from, to, address, prefixlen}).

ipam_create_net(NetworkDef) ->
	{Address, Prefixlen} = NetworkDef,
	ID = binary:encode_hex(<<(rand:uniform(16#FFFFFFFF)-1):32>>),
	ipam_put_net({ID, Address, Prefixlen}).

ipam_put_net(NetworkDef) ->
	case gen_server:call(ipam, {net_put, NetworkDef}) of
		{ok, Res} ->
			Res;
		Other ->
			Other
	end.

ipam_list_nets() ->
	gen_server:call(ipam, net_list).

ipam_delete_net(ID) ->
	case gen_server:call(ipam, {net_delete, ID}) of
		{ok, Res} ->
			Res;
		Other ->
			Other
	end.

ipam_put_ip(NetworkName, IP) ->
	gen_server:call(ipam, {ip_put, NetworkName, IP}).

assign_next(NetworkID, VMID) ->
	case gen_server:call(ipam, {ip_next, NetworkID, VMID}) of
		{ok, Res} ->
			Res;
		Other ->
			Other
	end.

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
	{ok, StoreId} = khepri:start(),
	init([StoreId]);
init([StoreId]) ->
	{ok, StoreId}.

terminate(_Reason, StoreId) ->
	DefaultStoreId = khepri_cluster:get_default_store_id(),
	case StoreId of
		DefaultStoreId ->
			khepri:stop(StoreId);
		_ ->
		ok
	end.

handle_call(net_list, _From, StoreId) ->
	case khepri:get_many(StoreId, [network, ?KHEPRI_WILDCARD_STAR]) of
		{ok, Map} ->
			Res = maps:from_list([{NetworkId, #{address => virtuerl_net:format_ip_bitstring(Address), prefixlen => PrefixLen}} || {[network, NetworkId], #network{address = Address, prefixlen = PrefixLen}} <- maps:to_list(Map)]),
			{reply, {ok, Res}, StoreId};
		Res -> {reply, Res, StoreId}
	end;
handle_call({net_put, Network}, _From, StoreId) ->
	{ID, Address, PrefixLen} = Network,
	{From, To} = get_range({Address, PrefixLen}),
	case To - From =< 8 of
	false ->
		BitLength = bit_size(Address),
		ok = khepri:put(StoreId, [network, ID], #network{address = Address, prefixlen = PrefixLen, from= <<(From+8):BitLength>>, to= <<To:BitLength>>, last_insert= <<(From+8):BitLength>>}),
		{reply, ok, StoreId};
		true ->
			{reply, {error, network_too_small}, StoreId}
	end;
handle_call({net_delete, ID}, _From, StoreId) ->
	Ret = khepri:delete(StoreId, [network, ID]),
	{reply, Ret, StoreId};

handle_call({ip_next, NetworkID}, _From, StoreId) -> handle_call({ip_next, NetworkID, ""}, _From, StoreId);
handle_call({ip_next, NetworkID, DomainID}, _From, StoreId) ->
	R = khepri:transaction(StoreId, fun() ->
		case khepri_tx:get([network, NetworkID]) of
			{ok, Network} ->
				#network{address = Address, prefixlen = PrefixLen, last_insert= LastInsertBin, from= FromBin, to= ToBin} = Network,
				BitLength = bit_size(LastInsertBin),
				<<LastInsert:BitLength>> = LastInsertBin,
				<<From:BitLength>> = FromBin,
				<<To:BitLength>> = ToBin,
				{ok, Map} = khepri_tx:get_many([network, NetworkID, #if_name_matches{regex = any}]),
				NextIP = case LastInsert of
									 undefined ->
										 get_next(Map, [network, NetworkID], BitLength, From, To, From);
									 Payload ->
										 get_next(Map, [network, NetworkID], BitLength, From, To, Payload)
								 end,
				case NextIP of
					{ok, IP} ->
						khepri_tx:put([network, NetworkID, <<IP:BitLength>>], DomainID),
						{ok, {Address, PrefixLen}, <<IP:BitLength>>};
					Other ->
						Other
				end;
			{error, ?khepri_error(node_not_found, _)} ->
				{error, network_not_found}
		end
	end),
	{reply, R, StoreId};

handle_call({ip_put, NetworkName, IPBlock, IPAddress}, _From, StoreId) ->
	R = khepri:put(StoreId, [network, NetworkName, IPBlock, IPAddress], khepri_payload:none()),
	{reply, R, StoreId};

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
				To -> From; % wrap around
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
