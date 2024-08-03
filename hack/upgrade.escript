#!/usr/bin/env escript

%%! -name virtuerl_adm -proto_dist inet6_tcp -hidden


main(_Args) ->
    io:format("~p~n", [_Args]),
    [NodeStr, Cookie, Tar] = _Args,

    erlang:set_cookie(list_to_atom(Cookie)),

    SudoPass = io:get_line(""),

    [Name, Host] = string:split(NodeStr, "@"),
    ssh:start(),
    {ok, Conn} = ssh:connect(Host, 22, [inet6]),

    Node = list_to_atom(NodeStr),
    case net_adm:ping(Node) of
        pong ->
            io:format("Shutting down Erlang sytem ..."),
            OsPid = erpc:call(Node, os, getpid, []),
            start_waiter_for_os_proc(Conn, OsPid),
            erpc:call(Node, init, stop, []),
            read_result(),
            io:format(" done~n");
        pang ->
            io:format("Couldn't connect to a remote system. Double-checking ...~n"),
            {Status, Output} = check_beam_proc(Conn, NodeStr),
            case Status of
                0 ->
                    #{0 := StdOut} = Output,
                    io:format("Following process seems to be running:~n~n~s~nRefusing to continue. Either kill that process or run this script with the -f option.~n", [StdOut]),
                    halt(10);
                1 -> ok
            end
    end,

    ScriptDir = filename:dirname(escript:script_name()),
    io:format("Repacking ~s ...~n", [Tar]),
    RepackOut = os:cmd([ScriptDir, "/repack ", Tar]),
    io:format("~ts~n", [RepackOut]),
    {ok, SftpChan} = ssh_sftp:start_channel(Conn),
    TarFilename = filename:basename(Tar),
    io:format("Uploading ~s ...", [TarFilename]),
    {ok, TarData} = file:read_file(TarFilename),
    ok = ssh_sftp:write_file(SftpChan, TarFilename, TarData),
    {ok, HelperBin} = file:read_file("helper/virtuerl_helper"),
    ok = ssh_sftp:write_file(SftpChan, "virtuerl_helper", HelperBin),
    io:format(" done~n"),
    % io:format("Deleting old release tar"),
    % {ok, FileNames} = ssh_sftp:list_dir(Chan, "."),
    % io:format("ls: ~p~n", [FileNames]),

    setuid_helper(Conn, SudoPass),
    extract_tar_target(Conn, TarFilename),

    io:format("Starting Erlang system ...~n"),
    % io:setopts([{echo, false}, binary]),
    {ok, Chan} = ssh_connection:session_channel(Conn, infinity),
    Term = os:getenv("TERM", "xterm"),
    CmdLine = ["dtach -n virtuerl_dtach virtuerl/bin/start_erl $PWD/virtuerl $PWD/virtuerl/releases $PWD/virtuerl/releases/start_erl.data -name ", NodeStr, " -proto_dist inet6_tcp -kernel inet_dist_listen_min 4370 inet_dist_listen_max 4400 -setcookie ", Cookie],
    io:format("cmd: ~s~n", [CmdLine]),
    ssh_connection:exec(Conn, Chan, ["TERM=", Term, " ", CmdLine], infinity),
    read_result(),
    io:format("Erlang system started~n").


check_beam_proc(Conn, Node) ->
    {ok, Chan} = ssh_connection:session_channel(Conn, infinity),
    ssh_connection:exec(Conn, Chan, ["pgrep -af 'beam\\.smp.*\\-name ", string:replace(Node, ".", "\\.", all), "'"], infinity),
    read_finished(Conn, Chan).


start_waiter_for_os_proc(Conn, OsPid) ->
    {ok, Chan} = ssh_connection:session_channel(Conn, infinity),
    ssh_connection:exec(Conn, Chan, ["tail --pid=", OsPid, " -f /dev/null"], infinity),
    Chan.


read_finished(Conn, Chan) ->
    {Status, Res0} = read_finished(Conn, Chan, #{}, undefined),
    Res = maps:map(fun(_Key, Val) -> binary_to_list(iolist_to_binary(Val)) end, Res0),
    io:format("Read: ~p ~p~n", [Status, Res]),
    {Status, Res}.


read_finished(Conn, Chan, Acc, Status) ->
    receive
        {ssh_cm, Conn, {data, Chan, Type, Data}} ->
            PreEx = maps:get(Type, Acc, ""),
            read_finished(Conn, Chan, Acc#{Type => [PreEx, Data]}, undefined);
        {ssh_cm, Conn, {eof, Chan}} -> read_finished(Conn, Chan, Acc, undefined);
        {ssh_cm, Conn, {exit_status, Chan, ExitStatus}} -> read_finished(Conn, Chan, Acc, ExitStatus);
        {ssh_cm, Conn, {closed, Chan}} -> {Status, Acc};
        _Else -> io:format("Got else: ~p~n", [_Else])
    end.


read_result() ->
    receive
        {ssh_cm, _Conn, {data, _Chan, _Type, Data}} ->
            io:format(Data),
            read_result();
        {ssh_cm, _Conn, {eof, _Chan}} -> read_result();
        {ssh_cm, _Conn, {exit_status, _Chan, _Res}} -> read_result();
        {ssh_cm, _Conn, {closed, _Chan}} -> ok;
        _Else -> io:format("Got else: ~p~n", [_Else])
    end.


setuid_helper(Conn, SudoPass) ->
    io:format("Setuid  ..."),

    {ok, Chan} = ssh_connection:session_channel(Conn, infinity),
    ssh_connection:exec(Conn, Chan, ["sudo -S chown root:root virtuerl_helper"], infinity),
    ssh_connection:send(Conn, Chan, SudoPass),
    ssh_connection:send_eof(Conn, Chan),
    {0, _} = read_finished(Conn, Chan),

    {ok, Chan1} = ssh_connection:session_channel(Conn, infinity),
    ssh_connection:exec(Conn, Chan1, ["sudo -S chmod +xs virtuerl_helper"], infinity),
    ssh_connection:send(Conn, Chan1, SudoPass),
    ssh_connection:send_eof(Conn, Chan1),
    {0, _} = read_finished(Conn, Chan1),

    io:format(" done~n").


extract_tar_target(Conn, TarFilename) ->
    io:format("Extracting ~s ...", [TarFilename]),
    {ok, Chan} = ssh_connection:session_channel(Conn, infinity),
    ssh_connection:exec(Conn, Chan, ["rm -rf virtuerl/ && tar xzf ", TarFilename, " --one-top-level=virtuerl"], infinity),
    {0, _} = read_finished(Conn, Chan),
    io:format(" done~n").
