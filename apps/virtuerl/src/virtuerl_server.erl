-module(virtuerl_server).

% -behaviour(gen_server).

-export([start/0, start/1, start/2]).

%% Callbacks for `gen_server`
% -export([init/1, handle_call/3, handle_cast/2]).

% start_link([]) ->
%     gen_server:start_link({local, ?MODULE}, ?MODULE, []).

% init([]) ->
%     _ = ets:new(virtuerl_servers, [named_table]),
%     Tab = ets:whereis(virtuerl_servers),
%     erlang:error(not_implemented).

% handle_call({update_config, Name, Conf},_From,State) ->

% handle_cast(Request,State) ->
%     erlang:error(not_implemented).


start() -> start(default).


start(Name) -> start(Name, #{}).


start(Name, Conf0) ->
    Conf = maps:merge(#{
                        vm_proc_mod => virtuerl_qemu,
                        net_prov_mod => virtuerl_net,
                        prefix => "verl"
                       },
                      Conf0),
    {ok, SupPid} = supervisor:start_child(virtuerl_sup_sup,
                                          #{id => Name, start => {virtuerl_sup, start_link, [Name, Conf]}, type => supervisor}),
    {ok, SupPid}.
