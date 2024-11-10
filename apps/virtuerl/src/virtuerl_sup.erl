-module(virtuerl_sup).

-behaviour(supervisor).

-export([start_link/2,
         start_child/2,
         which_children/1,
         delete_child/2,
         terminate_child/2]).
-export([init/1]).


start_child(ServerId, ChildSpec) ->
    supervisor:start_child({via, virtuerl_reg, {ServerId, ?MODULE}}, ChildSpec).


which_children(ServerId) ->
    supervisor:which_children({via, virtuerl_reg, {ServerId, ?MODULE}}).


terminate_child(ServerId, DomId) ->
    supervisor:terminate_child({via, virtuerl_reg, {ServerId, ?MODULE}}, DomId).


delete_child(ServerId, DomId) ->
    supervisor:delete_child({via, virtuerl_reg, {ServerId, ?MODULE}}, DomId).


start_link(ServerId, Conf) ->
    supervisor:start_link({via, virtuerl_reg, {ServerId, ?MODULE}}, ?MODULE, [ServerId, Conf]).


init([ServerId, Conf]) ->
    SupFlags = #{
                 strategy => one_for_one,
                 intensity => 300,
                 period => 5
                },
    ChildSpecs = [{virtuerl_img,
                   {virtuerl_img, start_link, [ServerId]},
                   permanent,
                   infinity,
                   worker,
                   []},
                  {virtuerl_net,
                   {virtuerl_net, start_link, [ServerId, Conf]},
                   permanent,
                   infinity,
                   worker,
                   []},
                  {virtuerl_host,
                   {virtuerl_host, start_link, [ServerId, Conf]},
                   permanent,
                   infinity,
                   worker,
                   []}],
    {ok, {SupFlags, ChildSpecs}}.
