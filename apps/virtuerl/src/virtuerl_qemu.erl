-module(virtuerl_qemu).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, terminate/2, handle_continue/2, handle_info/2, handle_call/3, handle_cast/2]).

-include_lib("kernel/include/logger.hrl").


start_link(Dom) ->
    gen_server:start_link(?MODULE, [Dom], []).


init([Dom]) ->
    #{id := DomainId} = Dom,
    Domain = maps:merge(#{user_data => "", vcpu => 2, memory => 4096}, Dom),
    State = #{id => DomainId, domain => Domain, qemu_pid => undefined, qmp_pid => undefined},
    {ok, State, {continue, setup_swtpm}}.


handle_continue(setup_swtpm, #{id := DomainId, domain := Domain} = State) ->
    DomainHomePath = filename:join([virtuerl_mgt:home_path(), "domains", DomainId]),
    ok = filelib:ensure_path(DomainHomePath),

    process_flag(trap_exit, true),
    file:delete(filename:join(DomainHomePath, "qmp.sock")),
    file:delete(filename:join(DomainHomePath, "serial.sock")),
    file:delete(filename:join(DomainHomePath, "vnc.sock")),
    file:delete(filename:join(DomainHomePath, "swtpm.sock")),

    {ok, _Pid, _OsPid} = exec:run_link("swtpm socket --tpmstate dir=./ --ctrl type=unixio,path=swtpm.sock --tpm2", [{cd, DomainHomePath}]),
    case wait_for_socket(filename:join(DomainHomePath, "swtpm.sock")) of
        ok ->
            {noreply, State, {continue, setup_base}};
        timeout ->
            {stop, failure, State}
    end;
handle_continue(setup_base, #{id := DomainId, domain := Domain} = State) ->
    DomainHomePath = filename:join([virtuerl_mgt:home_path(), "domains", DomainId]),

    OvmfVarsPath = filename:join(DomainHomePath, "OVMF_VARS_4M.ms.fd"),
    case filelib:is_regular(OvmfVarsPath) of
        false ->
            file:copy("/usr/share/OVMF/OVMF_VARS_4M.ms.fd", OvmfVarsPath);
        _ -> ok
    end,

    RootVolumePath = filename:join(DomainHomePath, "root.qcow2"),
    case filelib:is_regular(RootVolumePath) of
        false ->
            BaseImageOpts = case Domain of
                                #{base_image := BaseImage} ->
                                    {ok, BaseImagePath} = virtuerl_img:ensure_image(BaseImage),
                                    ["-b ", filename:absname(BaseImagePath), " -F qcow2 "];
                                _ -> []
                            end,
            {ok, _} = exec:run(iolist_to_binary(["qemu-img create -f qcow2 ", BaseImageOpts, RootVolumePath, " 70G"]), [sync, stderr, stdout]);
        _ -> noop
    end,
    ensure_cloud_config(Domain),
    #{mac_addr := MacAddr, tap_name := TapName, vcpu := Vcpu, memory := Memory} = Domain,
    CdromOpts = case Domain of
                    #{setup_iso := SetupIso} ->
                        ensure_autounattend(Domain),
                        [" -drive file=",
                         filename:join("/tmp/virtuerl/cache", SetupIso),
                         ",if=ide,index=0,media=cdrom ",
                         " -drive file=/tmp/virtuerl/cache/virtio-win-0.1.248.iso,if=ide,index=1,media=cdrom ",
                         " -drive file=autounattend.iso,if=ide,index=2,media=cdrom "];
                    _ -> []
                end,
    Cmd = iolist_to_binary(
            ["kvm -machine type=q35 -no-shutdown -S -nic tap,ifname=",
             TapName,
             ",script=no,downscript=no,model=virtio-net-pci,mac=",
             virtuerl_util:mac_to_str(MacAddr),
             " -vnc unix:vnc.sock -display none -cpu host -smp ",
             integer_to_binary(Vcpu),
             " -m ",
             integer_to_binary(Memory),
             " -chardev socket,id=chrtpm,path=swtpm.sock -tpmdev emulator,id=tpm0,chardev=chrtpm -device tpm-tis,tpmdev=tpm0",
             " -drive if=pflash,format=raw,file=/usr/share/OVMF/OVMF_CODE_4M.ms.fd,readonly=on",
             " -drive if=pflash,format=raw,file=OVMF_VARS_4M.ms.fd",
             " -drive file=root.qcow2,if=virtio -drive driver=raw,file=cloud_config.iso,if=virtio -qmp unix:qmp.sock,server=on,wait=off -serial unix:serial.sock,server=on,wait=off",
             CdromOpts]),  % -serial unix:serial.sock,server=on,wait=off
    ?LOG_INFO(#{domain => DomainId, qemu_cmd => Cmd}),
    {ok, Pid, OsPid} = exec:run_link(Cmd, [{cd, DomainHomePath}]),
    {noreply, State#{qemu_pid => {Pid, OsPid}}, {continue, setup_qmp}};
handle_continue(setup_qmp, #{id := ID} = State) ->
    QmpSocketPath = filename:join([virtuerl_mgt:home_path(), "domains", ID, "qmp.sock"]),
    io:format("waiting for qmp.sock ~p~n", [erlang:timestamp()]),
    %%  {ok, _} = exec:run(iolist_to_binary(["inotifywait -e create --include 'qmp\\.sock' ", ID]), [sync]),
    case wait_for_socket(QmpSocketPath) of
        ok ->
            % TODO: start_link might be too strong here, consider attaching it to a supervisor instead (or sth like gen_tcp's design)
            {ok, QmpPid} = virtuerl_qmp:start_link(QmpSocketPath),
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


handle_info({qmp, Event}, #{id := ID, domain := Domain} = State) ->
    ?LOG_INFO(#{domain => ID, qmp => Event}),
    case Event of
        #{<<"event">> := <<"STOP">>} ->
            DomainUpdated = Domain#{state => stopped},
            {stop, normal, State#{domain => DomainUpdated}};
        _ -> {noreply, State}
    end;
handle_info({tcp, SerialSocket, Data}, #{id := ID, domain := Domain, serial_socket := SerialSocket} = State) ->
    virtuerl_pubsub:send({domain_out, ID, Data}),
    {noreply, State};

handle_info({'EXIT', Port, normal}, State) when is_port(Port) ->
    % We need this so we don't crash when open_port(...) finishes
    % TODO: replace the above virtuerl_util:cmd with erlexec
    {noreply, State}.


handle_call(Request, From, State) ->
    erlang:error(not_implemented).


handle_cast(Request, State) ->
    erlang:error(not_implemented).


terminate(_Reason, #{id := ID, domain := Domain, qemu_pid := {Pid, OsPid}, qmp_pid := QmpPid}) ->
    ?LOG_DEBUG(#{domain => ID, event => graceful_shutdown, reason => _Reason}),
    case _Reason of
        normal -> ok;
        _ ->  % supervisor sends "shutdown"
            virtuerl_qmp:exec(QmpPid, system_powerdown),
            ?LOG_DEBUG(#{domain => ID, message => "waiting for guest to shutdown"}),
            receive
                {qmp, #{<<"event">> := <<"STOP">>}} -> ok
            after
                5000 ->
                    ?LOG_WARNING(#{domain => ID, message => "timed-out waiting for guest to shutdown"}),
                    {error, timeout}
            end
    end,
    %%  {ok, #{<<"return">> := #{}}} = thoas:decode(PowerdownRes),
    ok = virtuerl_qmp:stop(QmpPid),
    ok = exec:stop(Pid),
    ?LOG_DEBUG(#{domain => ID, message => "waiting for QEMU process to stop"}),
    receive
        {'EXIT', Pid, _} ->
            ?LOG_DEBUG("QEMU OS process stopped! (~p)~n", [Pid]),
            ok;
        {'EXIT', OsPid, _} ->
            ?LOG_DEBUG("QEMU OS process stopped (OsPid)!~n"),
            ok
    after
        5000 ->
            ?LOG_WARNING(#{domain => ID, message => "timed-out waiting for QEMU process to stop"}),
            {error, timeout}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


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


ensure_cloud_config(#{id := DomainID} = Domain) ->
    case filelib:is_regular(filename:join([virtuerl_mgt:home_path(), "domains", DomainID, "cloud_config.iso"])) of
        true -> ok;
        false -> create_cloud_config(Domain)
    end.


create_cloud_config(#{id := DomainID, name := DomainName, mac_addr := MacAddr, cidrs := Cidrs, user_data := UserData}) ->
    NetConf = ["version: 2\n",
               "ethernets:\n",
               "  primary:\n",
               "    match:\n",
               "      macaddress: \"",
               virtuerl_util:mac_to_str(MacAddr),
               "\"\n",
               "    set-name: ens2\n",
               "    dhcp4: false\n",
               "    dhcp6: false\n",
               "    nameservers:\n",
               "      addresses: [8.8.8.8, 8.8.4.4, 2001:4860:4860::8888, 2001:4860:4860::8844]\n",
               "    addresses:\n",
               [ ["      - ", virtuerl_net:format_ip(IpAddr), "/", integer_to_binary(Prefixlen), "\n"] || {IpAddr, Prefixlen} <- Cidrs ],
               "    routes:\n",
               [ ["      - to: default\n",
                  "        via: ",
                  virtuerl_net:format_ip(virtuerl_net:bridge_addr(IpAddr, Prefixlen)),
                  "\n"] || {IpAddr, Prefixlen} <- Cidrs ],
               ""],
    MetaData = ["instance-id: ", DomainID, "\n",
                "local-hostname: ", DomainName, "\n"],
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
    ok = virtuerl_util:cmd(binary_to_list(iolist_to_binary(IsoCmd))),
    ok = file:del_dir_r(IsoBasePath),
    ok.


ensure_autounattend(#{id := DomainID} = Domain) ->
    case filelib:is_regular(filename:join([virtuerl_mgt:home_path(), "domains", DomainID, "autounattend.iso"])) of
        true -> ok;
        false -> create_unattend(Domain)
    end.


create_unattend(#{id := DomainID} = Domain) ->
    DomainBasePath = filename:join([virtuerl_mgt:home_path(), "domains", DomainID]),
    AutounattendPath = filename:join(DomainBasePath, "Autounattend.xml"),
    UnattendPath = filename:join(DomainBasePath, "Unattend.xml"),
    ok = write_unattend(Domain, AutounattendPath),
    ok = write_unattend(Domain, UnattendPath),
    IsoCmd = ["genisoimage -R -iso-level 4 -o ", filename:join(DomainBasePath, "autounattend.iso"), " ", AutounattendPath, " ", UnattendPath],
    ok = virtuerl_util:cmd(binary_to_list(iolist_to_binary(IsoCmd))),
    ok.


write_unattend(Domain, Filename) ->
    Xml = gen_unattend(Domain),
    Export = xmerl:export_simple([Xml], xmerl_xml, [{prolog, "<?xml version=\"1.0\" encoding=\"utf-8\"?>"}]),
    file:write_file(Filename, iolist_to_binary(Export)).


gen_unattend(#{id := DomainID, name := DomainName, mac_addr := MacAddr, cidrs := Cidrs}) ->
    MacIdentifier = string:uppercase(string:replace(virtuerl_util:mac_to_str(MacAddr), ":", "-", all)),
    GatewaysXml = [ {'Route', [{'wcm:action', "add"}],
                              [{'Identifier', [integer_to_list(Idx)]},
                               {'NextHopAddress', [virtuerl_net:format_ip(virtuerl_net:bridge_addr(IpAddr, Prefixlen))]},
                               {'Prefix', [virtuerl_net:format_cidr(virtuerl_net:normalize_net({IpAddr, 0}))]}]}
                    || {Idx, {IpAddr, Prefixlen}} <- lists:enumerate(Cidrs) ],
    AddressesXml = [ {'IpAddress', [{'wcm:action', "add"}, {'wcm:keyValue', integer_to_list(Idx)}], [io_lib:format("~s/~B", [virtuerl_net:format_ip(IpAddr), Prefixlen])]}
                     || {Idx, {IpAddr, Prefixlen}} <- lists:enumerate(Cidrs) ],

    DriverPaths = ["E:\\amd64\\w11",
                   "E:\\viostor\\w11\\amd64",
                   "E:\\NetKVM\\w11\\amd64",
                   "E:\\Balloon\\w11\\amd64",
                   "E:\\pvpanic\\w11\\amd64",
                   "E:\\qemupciserial\\w11\\amd64",
                   "E:\\qxldod\\w10\\amd64",
                   "E:\\vioinput\\w11\\amd64",
                   "E:\\viorng\\w11\\amd64",
                   "E:\\vioscsi\\w11\\amd64",
                   "E:\\vioserial\\w11\\amd64",
                   "E:\\vioserial\\w11\\amd64"],
    DriverPathsXml = [ {'PathAndCredentials', [{'wcm:action', "add"}, {'wcm:keyValue', integer_to_list(Index)}],
                                              [{'Path', [Path]}]}
                       || {Index, Path} <- lists:enumerate(DriverPaths) ],

    Commands =
        [io_lib:format("reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\Cryptography\" /v \"MachineGuid\" /t REG_SZ /d \"~s\" /f", [DomainID]),

         "reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\OOBE\" /v BypassNRO /t REG_DWORD /d 1 /f",

         "reg.exe load \"HKU\\mount\" \"C:\\Users\\Default\\NTUSER.DAT\"",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\Runonce\" /v \"UninstallCopilot\" /t REG_SZ /d \"powershell.exe -NoProfile -Command \\\"Get-AppxPackage -Name 'Microsoft.Windows.Ai.Copilot.Provider' | Remove-AppxPackage;\\\"\" /f",
         "reg.exe add \"HKU\\mount\\Software\\Policies\\Microsoft\\Windows\\WindowsCopilot\" /v TurnOffWindowsCopilot /t REG_DWORD /d 1 /f",
         "reg.exe unload \"HKU\\mount\"",

         "reg.exe delete \"HKLM\\SOFTWARE\\Microsoft\\WindowsUpdate\\Orchestrator\\UScheduler_Oobe\\DevHomeUpdate\" /f",
         "cmd.exe /c \"del \"C:\\Users\\Default\\AppData\\Roaming\\Microsoft\\Windows\\Start Menu\\Programs\\OneDrive.lnk\"\"",
         "cmd.exe /c \"del \"C:\\Windows\\System32\\OneDriveSetup.exe\"\"",
         "cmd.exe /c \"del \"C:\\Windows\\SysWOW64\\OneDriveSetup.exe\"\"",

         "reg.exe load \"HKU\\mount\" \"C:\\Users\\Default\\NTUSER.DAT\"",
         "reg.exe delete \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\Run\" /v OneDriveSetup /f",
         "reg.exe unload \"HKU\\mount\"",

         "reg.exe delete \"HKLM\\SOFTWARE\\Microsoft\\WindowsUpdate\\Orchestrator\\UScheduler_Oobe\\OutlookUpdate\" /f",
         "reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Communications\" /v ConfigureChatAutoInstall /t REG_DWORD /d 0 /f",
         "powershell.exe -NoProfile -Command \"$xml = [xml]::new(); $xml.Load('C:\\Windows\\Panther\\unattend.xml'); $sb = [scriptblock]::Create( $xml.unattend.Extensions.ExtractScript ); Invoke-Command -ScriptBlock $sb -ArgumentList $xml;\"",
         "powershell.exe -NoProfile -Command \"Get-Content -LiteralPath '%TEMP%\\remove-packages.ps1' -Raw | Invoke-Expression;\"",
         "powershell.exe -NoProfile -Command \"Get-Content -LiteralPath '%TEMP%\\remove-caps.ps1' -Raw | Invoke-Expression;\"",
         "powershell.exe -NoProfile -Command \"Get-Content -LiteralPath '%TEMP%\\remove-features.ps1' -Raw | Invoke-Expression;\"",
         "reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\PolicyManager\\current\\device\\Start\" /v ConfigureStartPins /t REG_SZ /d \"{ \\\"pinnedList\\\": [] }\" /f",
         "reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\PolicyManager\\current\\device\\Start\" /v ConfigureStartPins_ProviderSet /t REG_DWORD /d 1 /f",
         "reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\PolicyManager\\current\\device\\Start\" /v ConfigureStartPins_WinningProvider /t REG_SZ /d B5292708-1619-419B-9923-E5D9F3925E71 /f",
         "reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\PolicyManager\\providers\\B5292708-1619-419B-9923-E5D9F3925E71\\default\\Device\\Start\" /v ConfigureStartPins /t REG_SZ /d \"{ \\\"pinnedList\\\": [] }\" /f",
         "reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\PolicyManager\\providers\\B5292708-1619-419B-9923-E5D9F3925E71\\default\\Device\\Start\" /v ConfigureStartPins_LastWrite /t REG_DWORD /d 1 /f",

         "reg.exe load \"HKU\\mount\" \"C:\\Users\\Default\\NTUSER.DAT\"",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\Runonce\" /v \"GeoLocation\" /t REG_SZ /d \"powershell.exe -NoProfile -Command \\\"Set-WinHomeLocation -GeoId 94;\\\"\" /f",
         "reg.exe unload \"HKU\\mount\"",

         "net.exe accounts /maxpwage:UNLIMITED",
         "regini.exe \"%TEMP%\\disable-defender.ini\"",
         "netsh.exe advfirewall firewall set rule group=\"Remote Desktop\" new enable=Yes",
         "reg.exe add \"HKLM\\SYSTEM\\CurrentControlSet\\Control\\Terminal Server\" /v fDenyTSConnections /t REG_DWORD /d 0 /f",
         "powershell.exe -NoProfile -Command \"Set-ExecutionPolicy -Scope 'LocalMachine' -ExecutionPolicy 'RemoteSigned' -Force;\"",
         "reg.exe add \"HKLM\\SOFTWARE\\Policies\\Microsoft\\Dsh\" /v AllowNewsAndInterests /t REG_DWORD /d 0 /f",

         "reg.exe load \"HKU\\mount\" \"C:\\Users\\Default\\NTUSER.DAT\"",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"ContentDeliveryAllowed\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"FeatureManagementEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"OEMPreInstalledAppsEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"PreInstalledAppsEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"PreInstalledAppsEverEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SilentInstalledAppsEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SoftLandingEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SubscribedContentEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SubscribedContent-310093Enabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SubscribedContent-338387Enabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SubscribedContent-338388Enabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SubscribedContent-338389Enabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SubscribedContent-338393Enabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SubscribedContent-353698Enabled\" /t REG_DWORD /d 0 /f",
         "reg.exe add \"HKU\\mount\\Software\\Microsoft\\Windows\\CurrentVersion\\ContentDeliveryManager\" /v \"SystemPaneSuggestionsEnabled\" /t REG_DWORD /d 0 /f",
         "reg.exe unload \"HKU\\mount\"",

         "reg.exe add \"HKLM\\Software\\Policies\\Microsoft\\Windows\\CloudContent\" /v \"DisableWindowsConsumerFeatures\" /t REG_DWORD /d 0 /f",
         "C:\\Windows\\Setup\\Scripts\\unattend-01.cmd"],
    ToCommands = fun(Cmds) ->
                         [ {'RunSynchronousCommand', [{'wcm:action', "add"}],
                                                     [{'Order', [integer_to_list(Idx)]}, {'Path', [Cmd]}]}
                           || {Idx, Cmd} <- lists:enumerate(Cmds) ]
                 end,

    CommandsXml = ToCommands(Commands),

    % based on: https://schneegans.de/windows/unattend-generator/?LanguageMode=Unattended&UILanguage=en-GB&UserLocale=en-GB&KeyboardLayout=0409%3A00000409&GeoLocation=94&ProcessorArchitecture=amd64&BypassNetworkCheck=true&ComputerNameMode=Custom&ComputerName=XXZZCCVVasdf&TimeZoneMode=Implicit&PartitionMode=Unattended&PartitionLayout=GPT&EspSize=300&RecoveryMode=Partition&RecoverySize=1000&WindowsEditionMode=Unattended&WindowsEdition=pro_workstations_n&UserAccountMode=Unattended&AccountName0=Admin&AccountPassword0=password&AccountGroup0=Administrators&AccountName1=User&AccountPassword1=password&AccountGroup1=Users&AccountName2=&AccountName3=&AccountName4=&AutoLogonMode=Own&PasswordExpirationMode=Unlimited&LockoutMode=Default&DisableDefender=true&DisableSystemRestore=true&EnableRemoteDesktop=true&AllowPowerShellScripts=true&DisableAppSuggestions=true&DisableWidgets=true&WifiMode=Skip&ExpressSettings=DisableAll&Remove3DViewer=true&RemoveCamera=true&RemoveClipchamp=true&RemoveClock=true&RemoveCopilot=true&RemoveCortana=true&RemoveDevHome=true&RemoveFamily=true&RemoveFeedbackHub=true&RemoveGetHelp=true&RemoveInternetExplorer=true&RemoveMailCalendar=true&RemoveMaps=true&RemoveMathInputPanel=true&RemoveZuneVideo=true&RemoveNews=true&RemoveNotepadClassic=true&RemoveOffice365=true&RemoveOneDrive=true&RemoveOneNote=true&RemoveOutlook=true&RemovePaint=true&RemovePaint3D=true&RemovePeople=true&RemovePhotos=true&RemovePowerAutomate=true&RemoveQuickAssist=true&RemoveSkype=true&RemoveSnippingTool=true&RemoveSolitaire=true&RemoveStepsRecorder=true&RemoveStickyNotes=true&RemoveTeams=true&RemoveGetStarted=true&RemoveToDo=true&RemoveVoiceRecorder=true&RemoveWeather=true&RemoveWindowsMediaPlayer=true&RemoveZuneMusic=true&RemoveWordPad=true&RemoveXboxApps=true&RemoveYourPhone=true&SystemScript0=powercfg.exe+%2FHIBERNATE+OFF&SystemScriptType0=Cmd&SystemScript1=&SystemScriptType1=Ps1&SystemScript2=&SystemScriptType2=Reg&SystemScript3=&SystemScriptType3=Vbs&DefaultUserScript0=&DefaultUserScriptType0=Reg&FirstLogonScript0=&FirstLogonScriptType0=Cmd&FirstLogonScript1=&FirstLogonScriptType1=Ps1&FirstLogonScript2=&FirstLogonScriptType2=Reg&FirstLogonScript3=&FirstLogonScriptType3=Vbs&UserOnceScript0=&UserOnceScriptType0=Cmd&UserOnceScript1=&UserOnceScriptType1=Ps1&UserOnceScript2=&UserOnceScriptType2=Reg&UserOnceScript3=&UserOnceScriptType3=Vbs&WdacMode=Skip&Microsoft-Windows-PnpCustomizationsWinPE=windowsPE&Microsoft-Windows-TCPIP=windowsPE&Microsoft-Windows-TCPIP=specialize
    {unattend,
     [{xmlns, "urn:schemas-microsoft-com:unattend"},
      {'xmlns:wcm', "http://schemas.microsoft.com/WMIConfig/2002/State"}],
     [{settings, [{pass, "offlineServicing"}], []},
      {settings, [{pass, "windowsPE"}],
                 [{component,
                   [{name, "Microsoft-Windows-International-Core-WinPE"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'SetupUILanguage', [{'UILanguage', ["en-GB"]}]},
                    {'InputLocale', ["0409:00000409"]},
                    {'SystemLocale', ["en-GB"]},
                    {'UILanguage', ["en-GB"]},
                    {'UserLocale', ["en-GB"]}]},
                  {component,
                   [{name, "Microsoft-Windows-Setup"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'ImageInstall',
                     [{'OSImage',
                       [{'InstallTo',
                         [{'DiskID', ["0"]},
                          {'PartitionID', ["3"]}]}]}]},
                    {'UserData',
                     [{'ProductKey',
                       [{'Key', ["WYPNQ-8C467-V2W6J-TX4WX-WT2RQ"]}]},
                      {'AcceptEula', ["true"]}]},
                    {'DiskConfiguration',
                     [{'DisableEncryptedDiskProvisioning', ["true"]},
                      {'Disk', [{'wcm:action', "add"}],
                               [{'CreatePartitions',
                                 [{'CreatePartition', [{'wcm:action', "add"}],
                                                      [{'Order', ["1"]},
                                                       {'Type', ["EFI"]},
                                                       {'Size', ["400"]}]},
                                  {'CreatePartition', [{'wcm:action', "add"}],
                                                      [{'Order', ["2"]},
                                                       {'Type', ["MSR"]},
                                                       {'Size', ["100"]}]},
                                  {'CreatePartition', [{'wcm:action', "add"}],
                                                      [{'Order', ["3"]},
                                                       {'Type', ["Primary"]},
                                                       {'Extend', ["true"]}]}]},
                                {'ModifyPartitions',
                                 [{'ModifyPartition', [{'wcm:action', "add"}],
                                                      [{'Format', ["NTFS"]},
                                                       {'Letter', ["C"]},
                                                       {'Order', ["1"]},
                                                       {'PartitionID', ["3"]},
                                                       {'Label', ["Windows 11"]}]}]},
                                {'DiskID', ["0"]},
                                {'WillWipeDisk', ["true"]}]},
                      {'WillShowUI', ["OnError"]}]}]},
                  {component,
                   [{name, "Microsoft-Windows-PnpCustomizationsWinPE"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'DriverPaths', DriverPathsXml}]}]},
      {settings, [{pass, "generalize"}], []},
      {settings, [{pass, "specialize"}],
                 [{component,
                   [{name, "Microsoft-Windows-Deployment"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'RunSynchronous', CommandsXml}]},
                  {component,
                   [{name, "Microsoft-Windows-TCPIP"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'Interfaces', [{'Interface', [{'wcm:action', "add"}],
                                                  [{'Identifier', [MacIdentifier]},
                                                   {'Ipv4Settings', [{'DhcpEnabled', ["false"]}]},
                                                   {'Ipv6Settings', [{'DhcpEnabled', ["false"]}]},
                                                   {'UnicastIpAddresses', AddressesXml},
                                                   {'Routes', GatewaysXml}]}]}]},
                  {component,
                   [{name, "Microsoft-Windows-DNS-Client"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'Interfaces', [{'Interface', [{'wcm:action', "add"}],
                                                  [{'Identifier', [MacIdentifier]},
                                                   {'DNSServerSearchOrder', [{'IpAddress', [{'wcm:action', "add"}, {'wcm:keyValue', "1"}], ["8.8.8.8"]},
                                                                             {'IpAddress', [{'wcm:action', "add"}, {'wcm:keyValue', "2"}], ["8.8.4.4"]},
                                                                             {'IpAddress', [{'wcm:action', "add"}, {'wcm:keyValue', "3"}], ["2001:4860:4860::8888"]},
                                                                             {'IpAddress', [{'wcm:action', "add"}, {'wcm:keyValue', "4"}], ["2001:4860:4860::8844"]}]}]}]}]},
                  {component,
                   [{name, "Microsoft-Windows-Shell-Setup"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'ComputerName', [DomainName]}]}]},
      {settings, [{pass, "auditSystem"}], []},
      {settings, [{pass, "auditUser"}],
                 [
                  % {component,
                  %       [{name,"Microsoft-Windows-Deployment"},
                  %        {processorArchitecture,"amd64"},
                  %        {publicKeyToken,"31bf3856ad364e35"},
                  %        {language,"neutral"},
                  %        {versionScope,"nonSxS"}],
                  %       [{'Generalize',[
                  %          {'Mode', ["OOBE"]},
                  %          {'ForceShutdownNow', ["true"]}
                  %        ]}]}
                 ]},
      {settings, [{pass, "oobeSystem"}],
                 [{component,
                   [{name, "Microsoft-Windows-International-Core"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'InputLocale', ["0409:00000409"]},
                    {'SystemLocale', ["en-GB"]},
                    {'UILanguage', ["en-GB"]},
                    {'UserLocale', ["en-GB"]}]},
                  % {component,
                  %     [{name,"Microsoft-Windows-Deployment"},
                  %      {processorArchitecture,"amd64"},
                  %      {publicKeyToken,"31bf3856ad364e35"},
                  %      {language,"neutral"},
                  %      {versionScope,"nonSxS"}],
                  %     [{'ExtendOSPartition',[{'Extend', ["true"]}]},
                  %      {'Reseal',[
                  %        {'Mode', ["Audit"]},
                  %        {'ForceShutdownNow', ["false"]}
                  %      ]}]},
                  {component,
                   [{name, "Microsoft-Windows-Shell-Setup"},
                    {processorArchitecture, "amd64"},
                    {publicKeyToken, "31bf3856ad364e35"},
                    {language, "neutral"},
                    {versionScope, "nonSxS"}],
                   [{'UserAccounts',
                     [{'LocalAccounts',
                       [{'LocalAccount',
                         [{'wcm:action', "add"}],
                         [{'Name', ["Admin"]},
                          {'Group', ["Administrators"]},
                          {'Password',
                           [{'Value', ["SoftwarePatchenIstFamos"]},
                            {'PlainText', ["true"]}]}]},
                        {'LocalAccount',
                         [{'wcm:action', "add"}],
                         [{'Name', ["User"]},
                          {'Group', ["Users"]},
                          {'Password',
                           [{'Value', ["SoftwarePatchenIstFamos"]},
                            {'PlainText', ["true"]}]}]}]}]},
                    {'AutoLogon',
                     [{'Username', ["Admin"]},
                      {'Enabled', ["true"]},
                      {'LogonCount', ["1"]},
                      {'Password',
                       [{'Value', ["SoftwarePatchenIstFamos"]},
                        {'PlainText', ["true"]}]}]},
                    {'OOBE',
                     [{'ProtectYourPC', ["3"]},
                      {'HideEULAPage', ["true"]},
                      {'HideWirelessSetupInOOBE', ["true"]}]},
                    {'FirstLogonCommands',
                     [
                      % {'SynchronousCommand',
                      %     [{'wcm:action',"add"}],
                      %     [{'Order',["1"]},
                      %      {'CommandLine',
                      %          ["del C:\\Windows\\Panther\\unattend.xml"]}]},
                      % {'SynchronousCommand',
                      %     [{'wcm:action',"add"}],
                      %     [{'Order',["2"]},
                      %      {'CommandLine',
                      %          ["C:\\Windows\\System32\\Sysprep\\Sysprep.exe /generalize /oobe /shutdown"]}]},
                      {'SynchronousCommand',
                       [{'wcm:action', "add"}],
                       [{'Order', ["1"]},
                        {'CommandLine',
                         ["reg.exe add \"HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Winlogon\" /v AutoLogonCount /t REG_DWORD /d 0 /f"]}]},
                      {'SynchronousCommand',
                       [{'wcm:action', "add"}],
                       [{'Order', ["2"]},
                        {'CommandLine',
                         ["powershell.exe -NoProfile -Command \"Disable-ComputerRestore -Drive 'C:\\';\""]}]}]}]}]},
      {'Extensions',
       [{xmlns, "https://schneegans.de/windows/unattend-generator/"}],
       [{'ExtractScript',
         ["param(
  [xml] $Document
);

$scriptsDir = 'C:\\Windows\\Setup\\Scripts\\';
foreach( $file in $Document.unattend.Extensions.File ) {
  $path = [System.Environment]::ExpandEnvironmentVariables(
    $file.GetAttribute( 'path' )
  );
  if( $path.StartsWith( $scriptsDir ) ) {
    mkdir -Path $scriptsDir -ErrorAction 'SilentlyContinue';
  }
  $encoding = switch( [System.IO.Path]::GetExtension( $path ) ) {
    { $_ -in '.ps1', '.xml' } { [System.Text.Encoding]::UTF8; }
    { $_ -in '.reg', '.vbs', '.js' } { [System.Text.UnicodeEncoding]::new( $false, $true ); }
    default { [System.Text.Encoding]::Default; }
  };
  [System.IO.File]::WriteAllBytes( $path, ( $encoding.GetPreamble() + $encoding.GetBytes( $file.InnerText ) ) );
}"]},
        {'File',
         [{path, "%TEMP%\\remove-packages.ps1"}],
         ["Get-AppxProvisionedPackage -Online |
Where-Object -Property 'DisplayName' -In -Value @(
  'Microsoft.Microsoft3DViewer';
  'Microsoft.WindowsCamera';
  'Clipchamp.Clipchamp';
  'Microsoft.WindowsAlarms';
  'Microsoft.549981C3F5F10';
  'MicrosoftCorporationII.MicrosoftFamily';
  'Microsoft.WindowsFeedbackHub';
  'Microsoft.GetHelp';
  'microsoft.windowscommunicationsapps';
  'Microsoft.WindowsMaps';
  'Microsoft.ZuneVideo';
  'Microsoft.BingNews';
  'Microsoft.MicrosoftOfficeHub';
  'Microsoft.Office.OneNote';
  'Microsoft.Paint';
  'Microsoft.MSPaint';
  'Microsoft.People';
  'Microsoft.Windows.Photos';
  'Microsoft.PowerAutomateDesktop';
  'MicrosoftCorporationII.QuickAssist';
  'Microsoft.SkypeApp';
  'Microsoft.ScreenSketch';
  'Microsoft.MicrosoftSolitaireCollection';
  'Microsoft.MicrosoftStickyNotes';
  'Microsoft.Getstarted';
  'Microsoft.Todos';
  'Microsoft.WindowsSoundRecorder';
  'Microsoft.BingWeather';
  'Microsoft.ZuneMusic';
  'Microsoft.Xbox.TCUI';
  'Microsoft.XboxApp';
  'Microsoft.XboxGameOverlay';
  'Microsoft.XboxGamingOverlay';
  'Microsoft.XboxIdentityProvider';
  'Microsoft.XboxSpeechToTextOverlay';
  'Microsoft.GamingApp';
  'Microsoft.YourPhone';
) | Remove-AppxProvisionedPackage -AllUsers -Online *>&1 >> \"$env:TEMP\\remove-packages.log\";
"]},
        {'File',
         [{path, "%TEMP%\\remove-caps.ps1"}],
         ["Get-WindowsCapability -Online |
Where-Object -FilterScript {
  ($_.Name -split '~')[0] -in @(
  'Browser.InternetExplorer';
  'MathRecognizer';
  'Microsoft.Windows.Notepad';
  'Microsoft.Windows.MSPaint';
  'App.Support.QuickAssist';
  'App.StepsRecorder';
  'Media.WindowsMediaPlayer';
  'Microsoft.Windows.WordPad';
  );
} | Remove-WindowsCapability -Online *>&1 >> \"$env:TEMP\\remove-caps.log\";
"]},
        {'File',
         [{path, "%TEMP%\\remove-features.ps1"}],
         ["Get-WindowsOptionalFeature -Online |
Where-Object -Property 'FeatureName' -In -Value @(
  'Microsoft-SnippingTool';
) | Disable-WindowsOptionalFeature -Online -Remove -NoRestart *>&1 >> \"$env:TEMP\\remove-features.log\";
"]},
        {'File',
         [{path, "C:\\Users\\Default\\AppData\\Local\\Microsoft\\Windows\\Shell\\LayoutModification.xml"}],
         ["<LayoutModificationTemplate Version=\"1\" xmlns=\"http://schemas.microsoft.com/Start/2014/LayoutModification\">
  <LayoutOptions StartTileGroupCellWidth=\"6\" />
  <DefaultLayoutOverride>
    <StartLayoutCollection>
      <StartLayout GroupCellWidth=\"6\" xmlns=\"http://schemas.microsoft.com/Start/2014/FullDefaultLayout\" />
    </StartLayoutCollection>
  </DefaultLayoutOverride>
</LayoutModificationTemplate>
"]},
        {'File',
         [{path, "%TEMP%\\disable-defender.ini"}],
         ["HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Services\\Sense
  \"Start\" = REG_DWORD 4
HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Services\\WdBoot
  \"Start\" = REG_DWORD 4
HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Services\\WdFilter
  \"Start\" = REG_DWORD 4
HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Services\\WdNisDrv
  \"Start\" = REG_DWORD 4
HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Services\\WdNisSvc
  \"Start\" = REG_DWORD 4
HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Services\\WinDefend
  \"Start\" = REG_DWORD 4
"]},
        {'File',
         [{path, "C:\\Windows\\Setup\\Scripts\\unattend-01.cmd"}],
         ["powercfg.exe /HIBERNATE OFF"]}]}]}.
