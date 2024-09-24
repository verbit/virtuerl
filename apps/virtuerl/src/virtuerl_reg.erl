-module(virtuerl_reg).

-behaviour(gen_server).

-export([start_link/0, register_name/2, unregister_name/1, whereis_name/1, send/2]).
%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2]).

-type name() :: {atom(), atom()}.


-spec register_name(Name :: name(), Pid :: pid()) -> yes | no.
register_name(Name, Pid) -> gen_server:call(?MODULE, {register_name, Name, Pid}).


-spec unregister_name(Name :: name()) -> _.
unregister_name(Name) -> gen_server:cast(?MODULE, {unregister_name, Name}).


-spec whereis_name(Name :: name()) -> pid() | undefined.
whereis_name(Name) -> gen_server:call(?MODULE, {whereis_name, Name}).


-spec send(Name :: name(), Msg :: term()) -> pid().
send(Name, Msg) ->
    case whereis_name(Name) of
        undefined -> exit({badarg, Name, Msg});
        Pid -> Pid ! Msg, Pid
    end.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, #{}}.


% -spec handle_call({register_name, name(), pid()}, _From :: _, State :: #{}) -> {reply, no | yes, #{}}.
handle_call({register_name, Name, Pid}, _From, State) ->
    {Res, NewState} = case State of
                          #{Name := _} -> {no, State};
                          _ -> {yes, State#{Name => Pid}}
                      end,
    {reply, Res, NewState};

handle_call({whereis_name, Name}, _From, State) ->
    Res = case State of
              #{Name := Pid} -> Pid;
              _ -> undefined
          end,
    {reply, Res, State}.


handle_cast({unregister_name, Name}, State) ->
    {noreply, maps:remove(Name, State)}.
