-module(virtuerl_sup_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupFlags = #{
                 strategy => one_for_one,
                 intensity => 300,
                 period => 5
                },
    ChildSpecs = [#{id => virtuerl_reg, start => {virtuerl_reg, start_link, []}},
                  #{id => virtuerl_pubsub, start => {virtuerl_pubsub, start_link, []}},
                  #{id => pg, start => {pg, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.
