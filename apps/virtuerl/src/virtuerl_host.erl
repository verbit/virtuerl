-module(virtuerl_host).

-behaviour(gen_server).

-export([start_link/2,
         home_path/0,
         sync/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         handle_continue/2]).

-include_lib("kernel/include/logger.hrl").

-define(APPLICATION, virtuerl).

-record(state, {server_id, vm_proc_mod, table, idmap, controller, dom_tap_map, prefix}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================


home_path() ->
    application:get_env(?APPLICATION, home, "var").


sync(Pid) ->
    gen_server:cast(Pid, sync).


start_link(ServerId, Conf) ->
    gen_server:start_link({via, virtuerl_reg, {ServerId, ?MODULE}}, ?MODULE, [ServerId, Conf], []).


init([ServerId, Conf]) ->
    #{cluster := Cluster, vm_proc_mod := VmProcMod, prefix := IfPrefix} = Conf,
    pg:join(Cluster, [self()]),
    % TODO: query supervisor and populate dom_tap_map
    {ok, #state{server_id = ServerId, vm_proc_mod = VmProcMod, idmap = #{}, dom_tap_map = #{}, prefix = IfPrefix}}.


generate_unique_tap_name(Prefix, TapNames) ->
    TapName = iolist_to_binary([Prefix, "tap", binary:encode_hex(<<(rand:uniform(16#ffffff)):24>>)]),
    case lists:member(TapName, TapNames) of
        false ->
            TapName;
        true ->
            generate_unique_tap_name(Prefix, TapNames)
    end.


handle_continue(sync_domains, #state{server_id = ServerId, vm_proc_mod = VmProcMod, idmap = IdMap, dom_tap_map = DomTapMap, prefix = IfPrefix, controller = ControllerPid} = State) ->
    Domains0 = virtuerl_mgt:domains_list(ControllerPid, ServerId),
    % TODO: Tap name generation should be part of the virtuerl_net module since
    %       this is a detail we shouldn't care about here
    {_, Domains1} = lists:foldl(fun(Dom, {IfNames, Doms}) ->
                                        #{id := DomId} = Dom,
                                        TapName = case DomTapMap of
                                                      #{DomId := TapName0} -> TapName0;
                                                      _ -> generate_unique_tap_name(IfPrefix, IfNames)
                                                  end,
                                        {[TapName | IfNames], [maps:put(tap_name, TapName, Dom) | Doms]}
                                end,
                                {[], []},
                                Domains0),

    Domains = maps:from_list([ {Id, Dom} || #{id := Id} = Dom <- Domains1 ]),

    TargetDomains = [ Id || {Id, Domain} <- maps:to_list(Domains),
                            case Domain of
                                #{state := stopped} -> false;
                                _ -> true
                            end ],
    RunningDomains = [ Id || {Id, _, _, _} <- virtuerl_sup:which_children(ServerId), is_binary(Id) ],
    ToDelete = RunningDomains -- TargetDomains,
    ToAdd = TargetDomains -- RunningDomains,

    [ virtuerl_sup:terminate_child(ServerId, Id) || Id <- ToDelete ],
    [ virtuerl_sup:delete_child(ServerId, Id) || Id <- ToDelete ],

    % cleanup deleted domains
    case file:list_dir(filename:join([virtuerl_host:home_path(), "domains"])) of
        {ok, Filenames} ->
            DirsToDel = [ iolist_to_binary(FName) || FName <- Filenames ] -- maps:keys(Domains),
            [ file:del_dir_r(filename:join([virtuerl_host:home_path(), "domains", Dir])) || Dir <- DirsToDel ];
        _ -> ok
    end,

    virtuerl_net:update_net(ServerId, Domains1),

    [ virtuerl_mgt:notify(ControllerPid, {domain_stopped, DomId}) || DomId <- ToDelete ],

    VmPids = [ {Id,
                virtuerl_sup:start_child(ServerId,
                                         #{
                                           id => Id,
                                           start => {VmProcMod, start_link, [maps:get(Id, Domains)]},
                                           restart => transient,
                                           shutdown => infinity
                                          })} || Id <- ToAdd ],

    [ virtuerl_mgt:notify(ControllerPid, {domain_started, DomId}) || {DomId, _} <- VmPids ],
    VmPidToDomId = maps:from_list([ {VmPid, DomId} || {DomId, {ok, VmPid}} <- VmPids ]),
    [ monitor(process, VmPid) || {_, {ok, VmPid}} <- VmPids ],

    NewDomTapMap = maps:map(fun(_Id, #{tap_name := TapName}) -> TapName end, Domains),
    {noreply, State#state{idmap = maps:merge(IdMap, VmPidToDomId), dom_tap_map = NewDomTapMap}}.


handle_call(_Request, _From, _State) ->
    erlang:error(not_implemented).


handle_cast(sync, State) ->
    {noreply, State, {continue, sync_domains}};
handle_cast(_Request, _State) ->
    erlang:error(not_implemented).


handle_info({enslave, ControllerPid}, #state{server_id = ServerId} = State) ->
    ?LOG_NOTICE(#{who => ?MODULE, msg => "got enslaved!", controller => ControllerPid}),
    yes = gen_server:call(ControllerPid, {register_name, ServerId, self()}),
    ?LOG_NOTICE(#{who => ?MODULE, msg => "successfully registered!", controller => ControllerPid, self => self(), name => ServerId}),
    {noreply, State#state{controller = ControllerPid}, {continue, sync_domains}};
handle_info({'DOWN', _, process, Pid, normal}, #state{idmap = IdMap} = State) ->
    NewIdMap = case IdMap of
                   #{Pid := DomId} ->
                       virtuerl_mgt:domain_stop(DomId),
                       maps:remove(Pid, IdMap);
                   #{} ->
                       ?LOG_WARNING(#{module => ?MODULE, msg => "process down but not in registry", pid => Pid}),
                       IdMap
               end,
    {noreply, State#state{idmap = NewIdMap}};
handle_info(Info, State) ->
    ?LOG_NOTICE(#{module => ?MODULE, msg => "unhandled info message", info => Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
