-module(virtuerl_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    SupFlags = #{
                 strategy => one_for_one,
                 intensity => 300,
                 period => 5
                },
    ChildSpecs = [{virtuerl_pubsub,
                   {virtuerl_pubsub, start_link, []},
                   permanent,
                   infinity,
                   worker,
                   []},
                  {virtuerl_ipam,
                   {virtuerl_ipam, start_link, []},
                   permanent,
                   infinity,
                   worker,
                   []},
                  {virtuerl_img,
                   {virtuerl_img, start_link, []},
                   permanent,
                   infinity,
                   worker,
                   []},
                  {virtuerl_mgt,
                   {virtuerl_mgt, start_link, []},
                   permanent,
                   infinity,
                   worker,
                   []},
                  {virtuerl_net,
                   {virtuerl_net, start_link, []},
                   permanent,
                   infinity,
                   worker,
                   []},
                  {virtuerl_api,
                   {virtuerl_api, start_link, []},
                   permanent,
                   infinity,
                   worker,
                   []}],
    {ok, {SupFlags, ChildSpecs}}.
