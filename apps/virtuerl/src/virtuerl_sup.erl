-module(virtuerl_sup).

-behaviour(supervisor).

-export([start_link/2, start_child/3, which_children/2, delete_child/3, terminate_child/3]).
-export([init/1]).


start_child(ServerId, Node, ChildSpec) ->
    erpc:call(Node, supervisor, start_child, [{via, virtuerl_reg, {ServerId, ?MODULE}}, ChildSpec]).


which_children(ServerId, Node) ->
    erpc:call(Node, supervisor, which_children, [{via, virtuerl_reg, {ServerId, ?MODULE}}]).


terminate_child(ServerId, Node, DomId) ->
    erpc:call(Node, supervisor, terminate_child, [{via, virtuerl_reg, {ServerId, ?MODULE}}, DomId]).


delete_child(ServerId, Node, DomId) ->
    erpc:call(Node, supervisor, delete_child, [{via, virtuerl_reg, {ServerId, ?MODULE}}, DomId]).


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
