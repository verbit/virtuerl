%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_qemu).

-behaviour(gen_server).

-export([start_link/1, callback_mode/0]).
-export([init/1, terminate/2]).
-export([handle_continue/2, handle_info/2]).
-export([handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ID) ->
%%  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%  Pid = spawn_link(fun() ->
%%    io:format("QEMU: Starting VM with ID ~p~n", [ID]),
%%    timer:sleep(20000),
%%    io:format("QEMU: Exiting ~p~n", [ID]),
%%    timer:sleep(500),
%%    exit(failure)
%%
%%             end),
%%  {ok, Pid}.

  gen_server:start_link(?MODULE, [ID], []).

callback_mode() ->
  handle_event_function.

init([ID]) ->
  {ok, Table} = dets:open_file(domains, [{file, filename:join(virtuerl_mgt:home_path(), "domains.dets")}]),
  [{DomainId, #{mac_addr:=MacAddr, tap_name := TapName} = DomainRaw}] = dets:lookup(Table, ID),
  Domain = maps:merge(#{user_data => ""}, DomainRaw),
  DomainHomePath = filename:join([virtuerl_mgt:home_path(), "domains", DomainId]),
  ok = filelib:ensure_path(DomainHomePath),
  RootVolumePath = filename:join(DomainHomePath, "root.qcow2"),
  case filelib:is_regular(RootVolumePath) of
    false ->
%%      BaseImagePath = filename:join([virtuerl_mgt:home_path(), "debian-12-genericcloud-amd64-20230910-1499.qcow2"]),
      BaseImagePath = filename:join([virtuerl_mgt:home_path(), "openSUSE-Leap-15.5.x86_64-NoCloud.qcow2"]),
      exec:run(lists:flatten(io_lib:format("qemu-img create -f qcow2 -b ~s -F qcow2 ~s", [filename:absname(BaseImagePath), RootVolumePath])), [sync]);
    _ -> noop
  end,
  process_flag(trap_exit, true),
  ensure_cloud_config(Domain),
  file:delete(filename:join(DomainHomePath, "qmp.sock")),
  file:delete(filename:join(DomainHomePath, "serial.sock")),
  Cmd = iolist_to_binary(["kvm -no-shutdown -S -nic tap,ifname=",TapName,",script=no,downscript=no,model=virtio-net-pci,mac=",virtuerl_util:mac_to_str(MacAddr), " -vnc :1 -display none -serial none -m 512 -drive file=root.qcow2,if=virtio -drive driver=raw,file=cloud_config.iso,if=virtio -qmp unix:qmp.sock,server=on,wait=off"]), % -serial unix:serial.sock,server=on,wait=off
  io:format("QEMU cmdline: ~s~n", [Cmd]),
  {ok, Pid, OsPid} = exec:run_link(Cmd, [{cd, DomainHomePath}]),
  State = #{table => Table, id => ID, domain => Domain, qemu_pid => {Pid, OsPid}, qmp_pid => undefined},
  {ok, State, {continue, setup_qmp}}.

handle_continue(setup_qmp, #{id := ID} = State) ->
  QmpSocketPath = filename:join([virtuerl_mgt:home_path(), "domains", ID, "qmp.sock"]),
  io:format("waiting for qmp.sock ~p~n", [erlang:timestamp()]),
%%  {ok, _} = exec:run(iolist_to_binary(["inotifywait -e create --include 'qmp\\.sock' ", ID]), [sync]),
  case wait_for_socket(QmpSocketPath) of
    ok ->
      {ok, QmpPid} = virtuerl_qmp:start_link(QmpSocketPath, self()),
      virtuerl_qmp:exec(QmpPid, cont),
      {noreply, State#{qmp_pid => QmpPid}, {continue, setup_serial}};
    timeout ->
      {stop, failure, State}
  end;
handle_continue(setup_serial, #{id := ID} = State) ->
  {noreply, State}.
%%  SerialSocketPath = filename:join([virtuerl_mgt:home_path(), "domains", ID, "serial.sock"]),
%%  io:format("waiting for serial.sock ~p~n", [erlang:timestamp()]),
%%%%  {ok, _} = exec:run(iolist_to_binary(["inotifywait -e create --include 'qmp\\.sock' ", ID]), [sync]),
%%  case wait_for_socket(SerialSocketPath) of
%%    ok ->
%%      % TODO: shall this be its own process instead?
%%      {ok, SerialSocket} = gen_tcp:connect({local, SerialSocketPath}, 0, [local, {active, true}, {packet, line}, binary]),
%%      {noreply, State#{serial_socket => SerialSocket}};
%%    timeout ->
%%      {stop, failure, State}
%%  end.

handle_info({qmp, Event}, #{table := Table, id := ID, domain := Domain} = State) ->
  io:format("QMP: ~p~n", [Event]),
  case Event of
    #{<<"event">> := <<"STOP">>} ->
      [{DomainId, Domain}] = dets:lookup(Table, ID),
      DomainUpdated = Domain#{state => stopped},
      ok = dets:insert(Table, {DomainId, DomainUpdated}),
      ok = dets:sync(Table),
      {stop, normal, State#{domain => DomainUpdated}};
    _ -> {noreply, State}
  end;
handle_info({tcp, SerialSocket, Data}, #{table := Table, id := ID, domain := Domain, serial_socket := SerialSocket} = State) ->
  virtuerl_pubsub:send({domain_out, ID, Data}),
  {noreply, State}.

shutdown_events() ->
  receive
    {qmp, Event} ->
      io:format("shutdown QMP: ~p~n", [Event]),
      shutdown_events()
  after 5000 -> ok
  end.

exit_events() ->
  receive
    {'EXIT', _Pid, _Reason} ->
      io:format("EXIT ~p ~p!~n", [_Pid, _Reason]),
      exit_events()
  after 10000 -> ok
  end.

terminate(_Reason, #{table := Table, id := ID, domain := Domain, qemu_pid := {Pid, OsPid}, qmp_pid := QmpPid}) ->
  io:format("GRACEFUL SHUTDOWN: ~s (~p)~n", [ID, _Reason]),
  case _Reason of
    normal -> ok;
    _ -> % supervisor sends "shutdown"
      virtuerl_qmp:exec(QmpPid, system_powerdown),
%%  shutdown_events(),
      receive
        {qmp, #{<<"event">> := <<"STOP">>}} -> ok
      after 5000 -> timeout
      end
  end,
%%  {ok, #{<<"return">> := #{}}} = thoas:decode(PowerdownRes),
  ok = virtuerl_qmp:stop(QmpPid),
  ok = exec:stop(Pid),
  receive
    {'EXIT', Pid, _} ->
      io:format("QEMU OS process stopped!~n"),
      ok;
    {'EXIT', OsPid, _} ->
      io:format("QEMU OS process stopped (OsPid)!~n"),
      ok
  after 5000 -> timeout
  end,
%%  exit_events(),
  dets:close(Table).

%%%===================================================================
%%% Internal functions
%%%===================================================================

wait_for_socket(SocketPath) ->
  Self = self(),
  WaiterPid = spawn(fun () ->
    do_wait_for_socket(SocketPath, Self)
                    end),
  receive
    {virtuerl, socket_available} ->
      io:format("done waiting for ~s ~p~n", [SocketPath, erlang:timestamp()]),
      ok
  after 2000 ->
    io:format("failed waiting"),
    exit(WaiterPid, kill),
    timeout
  end.

do_wait_for_socket(SocketPath, Requester) ->
  io:format("checking...~n"),
  case filelib:last_modified(SocketPath) of
    0 ->
      timer:sleep(20),
      do_wait_for_socket(SocketPath, Requester);
    _ ->
      Requester ! {virtuerl, socket_available}
  end.

ensure_cloud_config(#{id := DomainID} = Domain) ->
  case filelib:is_regular(filename:join([virtuerl_mgt:home_path(), "domains", DomainID, "cloud_config.iso"])) of
    true -> ok;
    false -> create_cloud_config(Domain)
  end.

create_cloud_config(#{id := DomainID, mac_addr := MacAddr, cidrs := Cidrs, user_data := UserData}) ->
  NetConf = [
    "version: 2\n",
    "ethernets:\n",
    "  primary:\n",
    "    match:\n",
    "      macaddress: \"", virtuerl_util:mac_to_str(MacAddr), "\"\n",
    "    set-name: ens2\n",
    "    dhcp4: false\n",
    "    dhcp6: false\n",
    "    addresses:\n", [[
    "      - ", virtuerl_net:format_ip(IpAddr), "/", integer_to_binary(Prefixlen), "\n"] || {IpAddr, Prefixlen} <- Cidrs],
    "    routes:\n", [[
    "      - to: default\n",
    "        via: ", virtuerl_net:format_ip(virtuerl_net:bridge_addr(IpAddr, Prefixlen)), "\n"] || {IpAddr, Prefixlen} <- Cidrs],
    ""
  ],
  MetaData = [
    "instance-id: ", DomainID, "\n",
    "local-hostname: ", DomainID, "\n"
  ],
  DomainBasePath = filename:join([virtuerl_mgt:home_path(), "domains", DomainID]),
  IsoBasePath = filename:join(DomainBasePath, "iso"),
  ok = filelib:ensure_path(IsoBasePath),
  NetConfPath = filename:join(IsoBasePath, "network-config"),
  ok = file:write_file(NetConfPath, NetConf),
  MetaDataPath = filename:join(IsoBasePath, "meta-data"),
  ok = file:write_file(MetaDataPath, MetaData),
  UserDataPath = filename:join(IsoBasePath, "user-data"),
  ok = file:write_file(UserDataPath, UserData),
  IsoCmd = ["genisoimage -output ", filename:join(DomainBasePath, "cloud_config.iso"), " -volid cidata -joliet -rock ", UserDataPath, " ", MetaDataPath, " ", NetConfPath],
  os:cmd(binary_to_list(iolist_to_binary(IsoCmd))),
  ok = file:del_dir_r(IsoBasePath),
  ok.

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).
