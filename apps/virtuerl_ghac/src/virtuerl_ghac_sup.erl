-module(virtuerl_ghac_sup).

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
    ChildSpecs = case application:get_env(gh_pat) of
                     undefined -> [];
                     _ -> [{virtuerl_ghac, {virtuerl_ghac, start_link, []}, permanent, infinity, worker, []}]
                 end,
    {ok, {SupFlags, ChildSpecs}}.
