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
    io:format("DOMAINS0: ~p~n", [Domains0]),
    {_, Domains1} = lists:foldl(fun(Dom, {IfNames, Doms}) ->
                                        #{id := DomId} = Dom,
                                        case DomTapMap of
                                            #{DomId := TapName} -> {[TapName | IfNames], [maps:put(tap_name, TapName, Dom) | Doms]};
                                            _ ->
                                                TapName = generate_unique_tap_name(IfPrefix, IfNames),
                                                {[TapName | IfNames], [maps:put(tap_name, TapName, Dom) | Doms]}
                                        end
                                end,
                                {[], []},
                                Domains0),

    io:format("DOMAINS1: ~p~n", [Domains1]),
    Domains = [ {Id, Dom} || #{id := Id} = Dom <- Domains1 ],
    TargetDomains = maps:from_list(
                      [ {Id, case maps:get(host, Domain, localhost) of localhost -> node(); Else -> Else end}
                        || {Id, Domain} <- Domains,
                           case Domain of
                               #{state := stopped} -> false;
                               _ -> true
                           end ]),

    AllNodes = nodes([this]),
    RunningDomains = maps:from_list(lists:flatten(
                                      [ [ {Id, Node} || {Id, _, _, _} <- virtuerl_sup:which_children(ServerId, Node), is_binary(Id) ]
                                        || Node <- AllNodes ])),
    ToDelete = maps:without(maps:keys(TargetDomains), RunningDomains),
    ToAdd = maps:without(maps:keys(RunningDomains), TargetDomains),
    [ virtuerl_sup:terminate_child(ServerId, Node, Id) || {Id, Node} <- maps:to_list(ToDelete) ],
    [ virtuerl_sup:delete_child(ServerId, Node, Id) || {Id, Node} <- maps:to_list(ToDelete) ],

    % cleanup deleted domains
    case file:list_dir(filename:join([virtuerl_host:home_path(), "domains"])) of
        {ok, Filenames} ->
            DirsToDel = sets:subtract(sets:from_list([ iolist_to_binary(FName) || FName <- Filenames ]),
                                      sets:from_list(maps:keys(maps:from_list(Domains)))),
            [ file:del_dir_r(filename:join([virtuerl_host:home_path(), "domains", Dir])) || Dir <- sets:to_list(DirsToDel) ];
        _ -> ok
    end,

    % lists:foreach(fun () -> ok end, List)
    DomsByNode0 = maps:groups_from_list(
                    fun({_Id, Dom}) -> case maps:get(host, Dom, localhost) of localhost -> node(); Else -> Else end end,
                    fun({_Id, Dom}) -> Dom end,
                    Domains),
    DomsByNode = maps:merge(#{node() => []}, DomsByNode0),
    [ virtuerl_net:update_net(Node, ServerId, Doms) || {Node, Doms} <- maps:to_list(DomsByNode), lists:member(Node, AllNodes) ],

    io:format("DomsByNode: ~p~n", [DomsByNode]),
    io:format("RUNNING: ~p~n", [RunningDomains]),
    io:format("TOADD: ~p~n", [ToAdd]),
    io:format("TODELETE: ~p~n", [ToDelete]),
    [ virtuerl_mgt:notify(ControllerPid, {domain_stopped, DomId}) || DomId <- maps:keys(ToDelete) ],

    VmPids = [ {Id,
                virtuerl_sup:start_child(ServerId,
                                         Node,
                                         {Id,
                                          {VmProcMod, start_link, [maps:get(Id, maps:from_list(Domains))]},
                                          transient,
                                          infinity,
                                          worker,
                                          []})} || {Id, Node} <- maps:to_list(ToAdd), lists:member(Node, AllNodes) ],
    io:format("VMPIDS: ~p~n", [VmPids]),
    [ virtuerl_mgt:notify(ControllerPid, {domain_started, DomId}) || {DomId, _} <- VmPids ],
    VmPidToDomId = maps:from_list([ {VmPid, DomId} || {DomId, {ok, VmPid}} <- VmPids ]),
    [ monitor(process, VmPid) || {_, {ok, VmPid}} <- VmPids ],

    NewDomTapMap = maps:from_list([ {Id, TapName} || {Id, #{tap_name := TapName}} <- Domains ]),
    {noreply, State#state{idmap = maps:merge(IdMap, VmPidToDomId), dom_tap_map = NewDomTapMap}}.


handle_call(_Request, _From, _State) ->
    erlang:error(not_implemented).


handle_cast(sync, State) ->
    {noreply, State, {continue, sync_domains}};
handle_cast(_Request, _State) ->
    erlang:error(not_implemented).


handle_info({enslave, ControllerPid}, State) ->
    ?LOG_NOTICE(#{who => ?MODULE, msg => "got enslaved!", controller => ControllerPid}),
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
