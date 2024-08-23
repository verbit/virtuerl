-module(virtuerl_bird).

-behaviour(gen_server).

-export([start_link/0, reload/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").


start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec reload(pid(), [{binary(), iolist()}]) -> ok.
reload(Pid, AddrToBridge) ->
    gen_server:call(Pid, {reload, AddrToBridge}).


init([]) ->
    {Spawn, ConfigPath, SocketPath} = case application:get_env(bird_config) of
                                          undefined ->
                                              ConfigPath0 = "bird.conf",
                                              SocketPath0 = "bird.ctl",
                                              {spawn, ConfigPath0, SocketPath0};
                                          {ok, Path1} ->
                                              SocketPath0 = case application:get_env(bird_ctl) of
                                                                undefined -> "/run/bird/bird.ctl";
                                                                {ok, Path2} -> Path2
                                                            end,
                                              {control, Path1, SocketPath0}
                                      end,

    {ok, {Spawn, ConfigPath, SocketPath}, {continue, setup}}.


handle_continue(setup, State) ->
    BirdPath = case application:get_env(bird_path) of
                   undefined -> "/usr/sbin/bird";
                   {ok, Path} -> Path
               end,

    case State of
        {spawn, ConfigPath0, SocketPath0} ->
            file:delete(SocketPath0),

            Tpl = "
router id 198.51.100.10;

protocol static { ipv4; }

template bgp up4 {
    #source address 2a0a:4580:1029::1;
    #local {{ in_berlin_local_ip4 }} port 179 as {{ in_berlin_local_as }};
    port 179;
    neighbor as 29670;
    multihop;
    ipv4 {
        add paths on;
        import none;
        export none;
    };
}",
            file:write_file(ConfigPath0, <<"router id 198.51.100.10;\nprotocol static { ipv4; }\n">>),
            {ok, _, _} = exec:run_link([BirdPath, "-f", "-c", lists:flatten(ConfigPath0), "-s", SocketPath0], []),
            ok = wait_for_socket(SocketPath0);
        _ -> ok
    end,
    {_, ConfigPath, SocketPath} = State,
    {ok, BirdCtlPid} = virtuerl_bird_ctl:start_link(SocketPath),
    process_flag(trap_exit, true),
    {noreply, {BirdCtlPid, ConfigPath}}.


handle_call({reload, AddrToBridge}, _From, {BirdCtlPid, ConfigPath} = State) ->
    AddrToBridgeName4 = lists:filter(fun({Addr, _}) -> bit_size(Addr) == 32 end, AddrToBridge),
    AddrToBridgeName6 = lists:filter(fun({Addr, _}) -> bit_size(Addr) == 128 end, AddrToBridge),

    ok = file:write_file(ConfigPath,
                         lists:join("\n",
                                    ["protocol static {", "  ipv4;"] ++ build_routes(AddrToBridgeName4) ++ ["}\n"] ++
                                    ["protocol static {", "  ipv6;"] ++ build_routes(AddrToBridgeName6) ++ ["}\n"])),
    {ok, _Messages} = virtuerl_bird_ctl:cmd(BirdCtlPid, "configure"),
    {reply, ok, State}.


handle_cast(Request, State) ->
    erlang:error(not_implemented).


terminate(Reason, {BirdCtlPid, ConfigPath} = State) ->
    ok = file:write_file(ConfigPath, ""),
    virtuerl_bird_ctl:cmd(BirdCtlPid, "configure").


format_bird_route(<<IP/binary>>) ->
    io_lib:format("~s/~B", [virtuerl_net:format_ip(IP), bit_size(IP)]).


build_routes(M) when is_map(M) -> build_routes(maps:to_list(M));
build_routes([]) -> [];
build_routes([{Addr, Bridge} | L]) ->
    [io_lib:format("  route ~s via \"~s\";", [format_bird_route(Addr), Bridge]) | build_routes(L)].


wait_for_socket(SocketPath) ->
    Self = self(),
    WaiterPid = spawn(fun() ->
                              do_wait_for_socket(SocketPath, Self)
                      end),
    receive
        {virtuerl, socket_available} ->
            ?LOG_DEBUG("done waiting for ~s ~p~n", [SocketPath, erlang:timestamp()]),
            ok
    after
        2000 ->
            ?LOG_DEBUG("failed waiting"),
            exit(WaiterPid, kill),
            timeout
    end.


do_wait_for_socket(SocketPath, Requester) ->
    ?LOG_DEBUG("checking...~n"),
    case filelib:last_modified(SocketPath) of
        0 ->
            timer:sleep(20),
            do_wait_for_socket(SocketPath, Requester);
        _ ->
            Requester ! {virtuerl, socket_available}
    end.
