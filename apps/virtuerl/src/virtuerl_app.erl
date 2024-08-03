-module(virtuerl_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).


%% app start/stop
start(_StartType, _StartArgs) ->

    httpc:set_options([{ipfamily, inet6fb4}]),
    filelib:ensure_path("var/log"),

    case filelib:is_file("virtuerl.config") of
        true ->
            {ok, [Conf]} = file:consult("virtuerl.config"),
            application:set_env(Conf);
        false ->
            ok
    end,

    virtuerl_sup:start_link().


start() ->
    application:ensure_all_started(virtuerl).
%%    exec:debug(4).


stop(_State) ->
    ok.
