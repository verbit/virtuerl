-module(virtuerl_mock_net).

-behaviour(gen_server).
-behaviour(virtuerl_net).

-export([start/0, start_link/0]).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2]).

%% Callbacks for `virtuerl_net`
-export([get_ifs/0, run_if_cmds/1]).


get_ifs() ->
    gen_server:call(?MODULE, get_ifs).


run_if_cmds(Cmds) ->
    gen_server:call(?MODULE, {run_if_cmds, Cmds}).


start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, #{}}.


handle_call(get_ifs, _From, State) ->
    {reply, maps:values(State), State};
handle_call({run_if_cmds, Cmds}, _From, State) ->
    ct:print("~p~n", [Cmds]),
    NewState = lists:foldl(fun(Cmd, Acc) ->
                                   case Cmd of
                                       {br_del, BrName} ->
                                           {ok, _} = maps:find(BrName, Acc),
                                           maps:remove(BrName, Acc);
                                       {tap_del, TapName} ->
                                           {ok, _} = maps:find(TapName, Acc),
                                           maps:remove(TapName, Acc);
                                       {br_add, BrName, Cidrs} ->
                                           error = maps:find(BrName, Acc),
                                           maps:put(BrName,
                                                    #{
                                                      <<"ifname">> => BrName,
                                                      <<"addr_info">> =>
                                                          lists:map(fun(Cidr) ->
                                                                            {Addr, Prefixlen} = virtuerl_net:parse_cidr(Cidr),
                                                                            #{<<"local">> => virtuerl_net:format_ip_bitstring(Addr), <<"prefixlen">> => Prefixlen, <<"scope">> => <<"global">>}
                                                                    end,
                                                                    Cidrs)
                                                     },
                                                    Acc);
                                       {tap_add, Tap, _Mac, _BrName} ->
                                           error = maps:find(Tap, Acc),
                                           maps:put(Tap, #{<<"ifname">> => Tap}, Acc)
                                   end
                           end,
                           State,
                           Cmds),

    {reply, ok, NewState}.


handle_cast(Request, State) ->
    erlang:error(not_implemented).
