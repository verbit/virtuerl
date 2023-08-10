%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_api_domain_res).

-behaviour(cowboy_handler).

%%-export([start_link/0]).
-export([init/2, content_types_provided/2, to_text/2, allowed_methods/2, content_types_accepted/2, from_json/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%%start_link() ->
%%  Dispatch = cowboy_router:compile([
%%    {'_', [{"/", virtuerl_api, []}]}
%%  ]),
%%  Res = cowboy:start_clear(my_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
%%  io:format("RESULT: ~p~n", [Res]),
%%  Res.

%% handler
%%init(Req0, State) ->
%%  Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello elloh">>, Req0),
%%  {ok, Req, State}.

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, from_json}],
    Req, State
  }.

from_json(Req, State) ->
  {ok, RawJSON, Req1} = cowboy_req:read_body(Req),
  {ok, JSON} = thoas:decode(RawJSON),
  #{<<"networkID">> := NetworkID} = JSON,
  Conf = #{network_id => NetworkID},
%%  virtuerl_mgt:create_vm(Conf),
  {true, Req1, State}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"plain">>, '*'}, to_text}],
    Req, State
  }.

to_text(Req, State) ->
  {<<"OK">>, Req, State}.
