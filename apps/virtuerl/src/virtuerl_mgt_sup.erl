-module(virtuerl_mgt_sup).

-behaviour(supervisor).

-export([start/1, start_link/1]).
-export([init/1]).


start(Cluster) ->
    {ok, SupPid} = supervisor:start_child(virtuerl_sup_sup,
                                          #{id => virtuerl_mgt_sup, start => {virtuerl_mgt_sup, start_link, [Cluster]}, type => supervisor}),
    {ok, SupPid}.


start_link(Cluster) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Cluster]).


init([Cluster]) ->
    SupFlags = #{
                 strategy => one_for_one,
                 intensity => 300,
                 period => 5
                },
    ChildSpecs = [#{id => virtuerl_ipam, start => {virtuerl_ipam, start_link, []}},
                  #{id => virtuerl_mgt, start => {virtuerl_mgt, start_link, [Cluster]}}],
    {ok, {SupFlags, ChildSpecs}}.
