-module(virtuerl_ghac).

-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([init/1,
         handle_call/3,
         handle_continue/2,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).


base_url() ->
    {ok, Org} = application:get_env(gh_org),
    {ok, Repo} = application:get_env(gh_repo),
    ["https://api.github.com/repos/", Org, "/", Repo].


headers() ->
    {ok, Token} = application:get_env(gh_pat),
    [{"User-Agent", "terstegen"}, {"Authorization", ["Bearer ", Token]}].


pending_jobs() ->
    {ok, {{_, 200, _}, _Headers, RunsRaw}} = httpc:request(get, {[base_url(), "/actions/runs?status=queued"], headers()}, [], []),
    {ok, RunsJson} = thoas:decode(RunsRaw),
    #{<<"workflow_runs">> := WorkflowRuns} = RunsJson,
    RunIds = [ RunId || #{<<"id">> := RunId} = Run <- WorkflowRuns ],
    Jobs = lists:map(fun list_jobs/1, RunIds),
    NumJobs = lists:foldl(fun(#{<<"total_count">> := Count}, Acc) -> Acc + Count end, 0, Jobs),
    NumJobs.


list_jobs(RunId) ->
    {ok, {{_, 200, _}, _Headers, JobsRaw}} = httpc:request(get, {[base_url(), "/actions/runs/", integer_to_list(RunId), "/jobs"], headers()}, [], []),
    {ok, JobsJson} = thoas:decode(JobsRaw),
    JobsJson.


list_runners() ->
    {ok, {{_, 200, _}, _Headers, RunnersRaw}} = httpc:request(get, {[base_url(), "/actions/runners"], headers()}, [], []),
    {ok, RunnersJson} = thoas:decode(RunnersRaw),
    #{<<"runners">> := Runners} = RunnersJson,
    [ Runner || #{<<"status">> := Status} = Runner <- Runners, Status == <<"online">> ].


list_domains() ->
    Domains = virtuerl_mgt:domains_list(),
    [ Domain || #{name := Name} = Domain <- Domains,
                string:prefix(Name, "actions-") /= nomatch,
                string:prefix(Name, "actions-base") == nomatch ].


create_runner(NetId) ->
    {ok, {{_, 201, _}, _Headers, TokenRaw}} = httpc:request(post, {[base_url(), "/actions/runners/registration-token"], headers(), "application/json", ""}, [], []),
    {ok, #{<<"token">> := Token}} = thoas:decode(TokenRaw),

    {ok, Org} = application:get_env(gh_org),
    {ok, Repo} = application:get_env(gh_repo),

    {ok, _} =
        virtuerl_mgt:domain_create(
          #{
            network_id => NetId,
            name => io_lib:format("actions-~s", [virtuerl_util:uuid4()]),
            vcpu => 2,
            memory => 8192,
            base_image => "actions-base.qcow2",
            user_data =>
                ["#cloud-config\n",
                 "\n",
                 "users:\n",
                 "  - default\n",
                 "  - name: ilya\n",
                 "    shell: /bin/bash\n",
                 "    sudo: ALL=(ALL) NOPASSWD:ALL\n",
                 "    ssh_authorized_keys:\n",
                 "      - ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDCKxHNpVg1whPegPv0KcRQTOfyVIqLwvMfVLyT9OpBPXHDudsFz9soOgMUEyWm8ZJ+pJ9fRCg66B+D5/ZRTwJCBpyNncfXCwu8xEJgEeoIubObh6t6dHWqqxX/yhHAS5GIRUSypm78qg6V+SQ6SeJXSjOCLAbZmhyWgJrlDm9M6GTPQhPAztrgsCUrzxIpZ5el5BwJXrm3I+LOmofAUqgbLQz9HuGJzPpnfABDa9WoVfI0L7oTr0qGpWwx8l71b2s8AYl7GMD/bEkZKyi9SSwEVCHA88F7dYYrZ3+fMXE/mJf+v0ece2lIDT7Te1gtqiLu/izJNmqD+b6mtnnXxVxNOtynhv3t6uLE9kBX22SBCCRqPJzETGNXvYH6fATEe88dhLh8kTppLRB5UGUd/zztxuNBSpMwFXaq8SlTKURxvF8BuFIPCz0FW8fq+TA/xZfBYsiVt59jXgl6BQyEGY4bMuMtT2nD8QXwZ5vsj52mzKGJwBwduiaX302brHYUyQkuyLII5iqmCNZ5YLlMY76a61Yg9pWMeRwQscSO2k4a18GOo+sIrQVTyUQiT3KhRRaDNrZuCPicQRgkJuiS1fKt1cWjnOlyweLxSYbpKnoS0H7vt+NrtbU1u9FPknXQPQ0pxixPpV3zgUdfOLmisFH7WGVjwNVvZAlNc5uyqm0fbw== ilya@verbit.io\n",
                 "\n",
                 "runcmd:\n",
                 "  - apt install -y git\n",
                 "  - chown -R ilya:ilya /opt/actions-runner\n",
                 "  - cd /opt/actions-runner && sudo -H -u ilya ./config.sh --unattended --url https://github.com/", Org, "/", Repo, " --token ", Token, " --ephemeral\n",
                 "  - /opt/actions-runner/svc.sh install ilya\n",
                 "  - /opt/actions-runner/svc.sh start\n"]
           }).


sync_runners(NetId, DomainList, RunnerList) ->
    ?LOG_DEBUG(#{msg => "syncing domains <-> runners"}),

    Domains = maps:from_list([ {DomId, Domain} || #{id := DomId} = Domain <- DomainList ]),
    Runners = maps:from_list([ {Name, Runner} || #{<<"name">> := Name} = Runner <- RunnerList ]),

    Now = erlang:system_time(millisecond),

    ToDeleteIds = [ DomId || #{id := DomId, name := Name, created_at := CreatedAt} = _Domain <- DomainList,
                             Now - CreatedAt > 5 * 60 * 1000,
                             not maps:is_key(iolist_to_binary(Name), Runners) ],
    [ virtuerl_mgt:domain_delete(#{id => DomId}) || DomId <- ToDeleteIds ],

    RemDomains = maps:without(ToDeleteIds, Domains),

    RequiredRunners = max(0, pending_jobs() + 2 - maps:size(RemDomains)),
    case RequiredRunners of
        0 -> ok;
        _ -> ?LOG_DEBUG(#{msg => "new runners required", domains => DomainList, runners => RunnerList, required => RequiredRunners})
    end,
    [ create_runner(NetId) || _ <- lists:seq(1, RequiredRunners) ],

    ok.


start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    {ok, {}, {continue, ensure_net}}.


-spec wait_for_domain_shutdown(binary(), integer()) -> ok | {error, term()}.
wait_for_domain_shutdown(DomId, Timeout) ->
    timer:sleep(2000),
    Deadline = erlang:system_time(millisecond) + Timeout,
    do_wait_for_domain_shutdown(DomId, Deadline).


-spec do_wait_for_domain_shutdown(binary(), integer()) -> ok | {error, term()}.
do_wait_for_domain_shutdown(DomId, Deadline) ->
    case virtuerl_mgt:domain_get(#{id => DomId}) of
        {ok, #{state := stopped}} -> ok;
        {error, Error} -> {error, Error};
        _ ->
            case erlang:system_time(millisecond) > Deadline of
                true ->
                    {error, timeout};
                false ->
                    timer:sleep(2000),
                    do_wait_for_domain_shutdown(DomId, Deadline)
            end
    end.


handle_continue(ensure_net, {}) ->
    {ok, Nets} = virtuerl_ipam:ipam_list_nets(),
    Filtered = maps:filter(
                 fun(NetId, NetConf) ->
                         Cidrs = [ lists:flatten(io_lib:format("~s/~B", [Addr, Prefixlen])) || #{address := Addr, prefixlen := Prefixlen} <- maps:values(maps:with([cidr4, cidr6], NetConf)) ],
                         Res = lists:sort(Cidrs) == lists:sort(["192.168.24.0/24", "fdbe:3c6a:3aef::/48"]),
                         Res
                 end,
                 Nets),

    {ok, NetId} = case maps:keys(Filtered) of
                      [Res] -> {ok, Res};
                      [] ->
                          ?LOG_NOTICE(#{msg => "ghac: creating network for actions worker"}),
                          {ok, Id} = virtuerl_ipam:ipam_create_net([{<<192:8, 168:8, 24:8, 0:8>>, 24}, {<<16#fdbe:16, 16#3c6a:16, 16#3aef:16, 0:80>>, 48}]),
                          {ok, Id};
                      _ -> error
                  end,

    self() ! sync,
    {noreply, {NetId}, {continue, ensure_base}};
handle_continue(ensure_base, {NetId} = State) ->
    ok = case lists:filter(fun(ImgName) -> string:equal(ImgName, "actions-base.qcow2") end, virtuerl_img:list_images()) of
             [_ImgFound] -> ok;
             [] ->
                 ?LOG_NOTICE(#{msg => "ghac: creating base image domain"}),
                 % create image
                 {ok, #{id := DomId}} =
                     virtuerl_mgt:domain_create(
                       #{
                         network_id => NetId,
                         name => "actions-base",
                         vcpu => 2,
                         memory => 8192,
                         base_image => "debian-12-genericcloud-amd64-20240507-1740.qcow2",
                         user_data =>
                             ["#cloud-config\n",
                              "\n",
                              % "# apt_get_command: ["apt-get", "--option=Dpkg::Options::=--force-confold", "--option=Dpkg::options::=--force-unsafe-io", "--assume-yes", "--quiet", "--no-install-recommends"]\n",
                              "package_upgrade: true\n",
                              "# packages:\n",
                              "#   - libvirt-daemon-system\n",
                              "#   - libvirt-dev\n",
                              "#   - qemu-kvm\n",
                              "#   - qemu-utils\n",
                              "#   - dnsmasq-base\n",

                              "runcmd:\n",
                              "  - apt install -y git gcc g++ pkg-config\n",
                              "  - apt install -y --no-install-recommends rebar3 qemu-kvm qemu-utils dnsmasq-base gcc\n",
                              "  - mkdir -p /opt/actions-runner\n",
                              "  - curl -O -L https://github.com/actions/runner/releases/download/v2.316.1/actions-runner-linux-x64-2.316.1.tar.gz\n",
                              "  - tar xzf ./actions-runner-linux-x64-2.316.1.tar.gz -C /opt/actions-runner\n",
                              "  - rm ./actions-runner-linux-x64-2.316.1.tar.gz\n",

                              "apt:\n",
                              "  primary:\n",
                              "    - arches: [default]\n",
                              "      search:\n",
                              "        - http://mirror.ipb.de/debian/\n",
                              "        - http://security.debian.org/debian-security\n",

                              "power_state:\n",
                              "  mode: poweroff\n",
                              "  condition: test -d /opt/actions-runner\n"]
                        }),

                 Res = wait_for_domain_shutdown(DomId, 10 * 60 * 1000),  % 10 minutes
                 case Res of
                     ok -> virtuerl_mgt:image_from_domain(DomId, "actions-base");
                     _ -> Res
                 end,
                 virtuerl_mgt:domain_delete(#{id => DomId}),

                 Res;
             _ -> error
         end,

    {noreply, State}.


handle_call(_Req, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(sync, {NetId} = State) ->
    DomainList = list_domains(),
    RunnerList = list_runners(),
    sync_runners(NetId, DomainList, RunnerList),
    erlang:send_after(30 * 1000, self(), sync),
    {noreply, State}.


terminate(_Reason, State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
