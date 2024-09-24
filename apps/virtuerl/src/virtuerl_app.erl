-module(virtuerl_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).


%% app start/stop
start(_StartType, _StartArgs) ->

    httpc:set_options([{ipfamily, inet6fb4}]),
    filelib:ensure_path("var/log"),

    case filelib:is_file("virtuerl.config") of
        true ->
            {ok, [Conf]} = file:consult("virtuerl.config"),
            application:set_env(Conf);
        false ->
            ok
    end,

    {ok, Pid} = virtuerl_sup_sup:start_link(),

    Servers = case application:get_env(servers) of
                  undefined -> #{};
                  {ok, Servers0} -> Servers0
              end,
    [ virtuerl_server:start(Name, Conf) || {Name, Conf} <- maps:to_list(Servers) ],

    Clusters = case application:get_env(clusters) of
                   undefined -> [];
                   {ok, Clusters0} -> Clusters0
               end,
    [ virtuerl_mgt_sup:start(Cluster) || Cluster <- Clusters ],

    {ok, Pid}.


start() ->
    application:ensure_all_started(virtuerl).
%%    exec:debug(4).


stop(_State) ->
    ok.
