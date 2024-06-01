%%%-------------------------------------------------------------------
%% @doc virtuerl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(virtuerl_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 300,
                 period => 5},
    ChildSpecs = [
      {
        virtuerl_pubsub,
        {virtuerl_pubsub, start_link, []},
        permanent,
        infinity,
        worker,
        []
      }, {
      virtuerl_ipam,
      {virtuerl_ipam, start_link, []},
      permanent,
      infinity,
      worker,
      []
      }, {
      virtuerl_img,
      {virtuerl_img, start_link, []},
      permanent,
      infinity,
      worker,
      []
      }, {
      virtuerl_mgt,
      {virtuerl_mgt, start_link, []},
      permanent,
      infinity,
      worker,
      []
    }, {
      virtuerl_net,
      {virtuerl_net, start_link, []},
      permanent,
      infinity,
      worker,
      []
    }
      , {
      virtuerl_api,
      {virtuerl_api, start_link, []},
      permanent,
      infinity,
      worker,
      []
    }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
