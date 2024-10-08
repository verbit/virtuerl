-module(virtuerl_img).

-behaviour(gen_server).

-export([start_link/1,
         list_images/0, list_images/1,
         ensure_image/1, ensure_image/2,
         build_image/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([]).
-export([sample_cloud_config/0]).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================


list_images() -> list_images(default).


list_images(ServerId) ->
    gen_server:call({via, virtuerl_reg, {ServerId, ?MODULE}}, list_images).


ensure_image(ImageName) -> ensure_image(default, ImageName).


ensure_image(ServerId, ImageName) ->
    gen_server:call({via, virtuerl_reg, {ServerId, ?MODULE}}, {ensure_image, ImageName}, infinity).


start_link(ServerId) ->
    gen_server:start_link({via, virtuerl_reg, {ServerId, ?MODULE}}, ?MODULE, [], []).


init([]) ->
    {ok, []}.


-spec wait_for_domain_shutdown(DomainId :: binary(), TimeoutMsec :: integer()) -> ok | {error, term()}.
wait_for_domain_shutdown(DomId, Timeout) ->
    timer:sleep(2000),
    Deadline = erlang:system_time(millisecond) + Timeout,
    do_wait_for_domain_shutdown(DomId, Deadline).


-spec do_wait_for_domain_shutdown(DomainId :: binary(), Deadline :: integer()) -> ok | {error, term()}.
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


sample_cloud_config() ->
    ["#cloud-config\n",
     "\n",
     % "package_upgrade: true\n",
     "# packages:\n",
     "#   - libvirt-daemon-system\n",
     "#   - libvirt-dev\n",
     "#   - qemu-kvm\n",
     "#   - qemu-utils\n",
     "#   - dnsmasq-base\n",

     "runcmd:\n",
     % "  - apt install -y git gcc g++ pkg-config\n",
     % "  - apt install -y --no-install-recommends rebar3 qemu-kvm qemu-utils dnsmasq-base gcc\n",
     % "  - mkdir -p /opt/actions-runner\n",
     % "  - curl -O -L https://github.com/actions/runner/releases/download/v2.316.1/actions-runner-linux-x64-2.316.1.tar.gz\n",
     % "  - tar xzf ./actions-runner-linux-x64-2.316.1.tar.gz -C /opt/actions-runner\n",
     % "  - rm ./actions-runner-linux-x64-2.316.1.tar.gz\n",
     "  - mkdir -p /opt/actions-runner\n",

     "apt:\n",
     "  primary:\n",
     "    - arches: [default]\n",
     "      search:\n",
     "        - http://mirror.ipb.de/debian/\n",
     "        - http://security.debian.org/debian-security\n",

     "power_state:\n",
     "  mode: poweroff\n",
     "  condition: test -d /opt/actions-runner\n"].


%% @doc
%% This function *moves* a file into the layers/ folder and also creates a link in images/ using the provided image name.
%% Since copying large image files could be potentially slow, this function only performs renames. Therefore, as a caller
%% of this function you have to make sure that the file is safe to move.
rename_image(FilePath, ImageName) ->
    {ok, ImgInfo} = thoas:decode(os:cmd(binary_to_list(iolist_to_binary(["qemu-img info --output json ", FilePath])))),
    case ImgInfo of
        #{<<"backing-filename">> := BackingFilePath} ->
            PathComponents = filename:split(BackingFilePath),
            ["layers", BackingFileName] = lists:nthtail(length(PathComponents) - 2, PathComponents),
            os:cmd(binary_to_list(iolist_to_binary(["qemu-img rebase -u -b ", BackingFileName, " ", FilePath]))),
            ok;
        _ -> ok
    end,
    ShaSum0 = os:cmd(binary_to_list(iolist_to_binary(["sha256sum -bz ", FilePath]))),
    [ShaSum, _Rest] = string:split(ShaSum0, " "),
    LayerFileName = iolist_to_binary([ShaSum, ".qcow2"]),
    LayerPath = filename:join([virtuerl_mgt:home_path(), "layers", LayerFileName]),
    ok = filelib:ensure_dir(LayerPath),
    ok = file:rename(FilePath, LayerPath),
    ImgPath = filename:join([virtuerl_mgt:home_path(), "images", ImageName]),
    ok = filelib:ensure_dir(ImgPath),
    ok = file:make_symlink(filename:join(["..", "layers", LayerFileName]), ImgPath),
    {ok, LayerPath}.


% virtuerl_img:build_image("debian-12-genericcloud-amd64-20240507-1740.qcow2", virtuerl_img:sample_cloud_config(), "my_test_image", #{host => localhost, network => <<"ba1c637c-e349-d739-5b74-ed919fa38bdd">>}).
-spec build_image(iodata(), iodata(), iodata(), #{host := Host :: atom, network_id := Network :: binary()}) -> _.
build_image(ParentImage, CloudConfig, ImageName, Options) ->
    % TODO (future): as a first step we could ask other hosts in the cluster for an image first
    Defaults = #{
                 domain_name => iolist_to_binary(["internal-", virtuerl_util:uuid4()]),
                 timeout_seconds => 20 * 60
                },
    #{
      host := Host,
      network_id := NetworkId,
      domain_name := DomainName,
      timeout_seconds := TimeoutSec
     } = maps:merge(Defaults, Options),

    {ok, #{id := DomainId}} = virtuerl_mgt:domain_create(
                                #{
                                  host => Host,  % how to ensure it is running on the local machine?
                                  name => DomainName,
                                  base_image => ParentImage,
                                  network_id => NetworkId,
                                  vpcu => 4,
                                  memory => 8096,
                                  user_data => CloudConfig
                                 }),

    {ok, _Path} = ensure_image(ParentImage),

    Res = case wait_for_domain_shutdown(DomainId, TimeoutSec * 1000) of
              ok ->
                  RootVolPath = filename:join([virtuerl_mgt:home_path(), "domains", DomainId, "root.qcow2"]),
                  rename_image(RootVolPath, ImageName);
              Else -> Else
          end,

    virtuerl_mgt:domain_delete(#{id => DomainId}),  % TODO: add possibility to archive (so we can delete dom beforehand)
    Res.


handle_call(list_images, _Sender, State) ->
    {ok, Filenames} = file:list_dir(filename:join(virtuerl_mgt:home_path(), "images")),
    Images = [ Filename || Filename <- Filenames, virtuerl_util:ends_with(Filename, ".qcow2") ],
    Result = lists:uniq(["debian-12-genericcloud-amd64-20240507-1740.qcow2" | Images]),
    {reply, Result, State};

handle_call({ensure_image, ImageName}, _Sender, State) ->
    Path = filename:join([virtuerl_mgt:home_path(), "images", ImageName]),
    Res = case file:read_link(Path) of
              {ok, Target} ->
                  PathComponents = filename:split(Target),
                  ["layers", LayerFileName] = lists:nthtail(length(PathComponents) - 2, PathComponents),
                  {ok, filename:join([virtuerl_mgt:home_path(), "layers", LayerFileName])};
              {error, _} ->
                  {ok, Pat} = re:compile("^debian-12-genericcloud-amd64-(\\d+-\\d+).qcow2$"),
                  case re:run(ImageName, Pat, [{capture, all_but_first, list}]) of
                      {match, [Build]} ->
                          CacheImagePath = filename:join(["/tmp/virtuerl/cache", ImageName]),
                          ok = filelib:ensure_dir(CacheImagePath),
                          TempImagePath = filename:join(["/tmp/virtuerl/", ImageName]),
                          case filelib:is_regular(CacheImagePath) of
                              true -> ok;
                              false ->
                                  io:format("DOWNLOADING..."),
                                  {ok, _} = httpc:request(get,
                                                          {["https://cloud.debian.org/images/cloud/bookworm/", Build, "/", ImageName], []},
                                                          [],
                                                          [{stream, TempImagePath}]),
                                  ok = file:rename(TempImagePath, CacheImagePath)
                          end,
                          {ok, _} = file:copy(CacheImagePath, TempImagePath),
                          rename_image(TempImagePath, ImageName);
                      nomatch ->
                          {error, not_supported}
                  end
          end,
    {reply, Res, State}.


handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(_Req, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
