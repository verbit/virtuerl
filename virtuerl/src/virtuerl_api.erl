%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_api).

-behaviour(cowboy_handler).

-export([start_link/0]).
-export([handle/4]).
%%-export([init/2, content_types_provided/2, to_text/2, allowed_methods/2, content_types_accepted/2, from_json/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  mochiweb_http:start_link([
    {name, ?MODULE},
    {loop, fun loop/1},
    {ip, any},
    {port, 8080}
  ]).


%% 1> {ok,MP} = re:compile("(?<A>A)|(?<B>B)|(?<C>C)").
%% {ok,{re_pattern,3,0,0,
%%                 <<69,82,67,80,119,0,0,0,0,0,0,0,1,0,0,0,255,255,255,255,
%%                   255,255,...>>}}
%% 2> {namelist, N} = re:inspect(MP,namelist).
%% {namelist,[<<"A">>,<<"B">>,<<"C">>]}
%% 3> {match,L} = re:run("AA",MP,[{capture,all_names,binary}]).
%% {match,[<<"A">>,<<>>,<<>>]}
%% 4> NameMap = lists:zip(N,L).
%% [{<<"A">>,<<"A">>},{<<"B">>,<<>>},{<<"C">>,<<>>}]

urls() ->
  RawURLs = [
    {"^/networks/?$", networks, ['GET', 'POST']},
    {"^/networks/(?<id>[^/]+)/?$", network, ['GET', 'PUT', 'DELETE']},
    {"^/domains/?$", domains, ['GET', 'POST']},
    {"^/domains/(?<id>[^/]+)/?$", domain, ['GET', 'DELETE']}
  ],
  CompiledUrls = lists:map(fun ({URL, Tag, Methods}) ->
    {ok, Pat} = re:compile(URL),
    {namelist, Namelist} = re:inspect(Pat, namelist),
    {Pat, Namelist, Tag, sets:from_list(Methods)}
                           end, RawURLs),
  CompiledUrls.

dispatch(Req, []) ->
  mochiweb_request:not_found(Req);
dispatch(Req, [{Pat, Namelist, Tag, AllowedMethods}|T]) ->
  Path = mochiweb_request:get(path, Req),
  case re:run(Path, Pat, [{capture, all_names, list}]) of
    {match, Match} -> dispatch_matched(Match, Namelist, Tag, AllowedMethods, Req);
    match -> dispatch_matched([], Namelist, Tag, AllowedMethods, Req);
    nomatch -> dispatch(Req, T)
  end.

dispatch_matched(Match, Namelist, Tag, AllowedMethods, Req) ->
  Method = mochiweb_request:get(method, Req),
  case sets:is_element(Method, AllowedMethods) of
    false -> mochiweb_request:respond({405, [{"Content-Type", "text/plain"}], "Method Not Allowed"}, Req);
    _ ->
      PathMap = maps:from_list(lists:zip(lists:map(fun binary_to_atom/1, Namelist), Match)),
      io:format("PathMap: ~p~n", [PathMap]),
      try handle(Tag, Method, PathMap, Req)
      catch
        error:function_clause -> mochiweb_request:respond({500, [{"Content-Type", "text/plain"}], "Error"}, Req)
%%            ;
%%            error:bad_argument -> mochiweb_request:respond({500, [{"Content-Type", "text/plain"}], "Error"}, Req)
      end
  end.

loop(Req) ->
  dispatch(Req, urls()).

%% handler
%%init(Req0, State) ->
%%  Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello elloh">>, Req0),
%%  {ok, Req, State}.

%%init(Req, State) ->
%%  {cowboy_rest, Req, State}.
%%
%%allowed_methods(Req, State) ->
%%  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.
%%
%%content_types_accepted(Req, State) ->
%%  {[{{<<"application">>, <<"json">>, '*'}, from_json}],
%%    Req, State
%%  }.
%%
%%from_json(Req, State) ->
%%  {ok, RawJSON, Req1} = cowboy_req:read_body(Req),
%%  {ok, JSON} = thoas:decode(RawJSON),
%%  #{<<"networkID">> := NetworkID} = JSON,
%%  Conf = #{network_id => NetworkID},
%%%%  virtuerl_mgt:create_vm(Conf),
%%  {true, Req1, State}.
%%
%%content_types_provided(Req, State) ->
%%  {[{{<<"text">>, <<"plain">>, '*'}, to_text}],
%%    Req, State
%%  }.
%%
%%to_text(Req, State) ->
%%  {<<"OK">>, Req, State}.

parse_json(Req) ->
  Body = mochiweb_request:recv_body(Req),
  {ok, JSON} = thoas:decode(Body),
  JSON.

handle(networks, 'GET', _, Req) ->
  {ok, Nets} = virtuerl_ipam:ipam_list_nets(),
  io:format("~p~n", [Nets]),
  mochiweb_request:respond({200, [{"Content-Type", "application/json"}], thoas:encode(Nets)}, Req);
handle(networks, 'POST', _, Req) ->
  JSON = parse_json(Req),
  #{<<"network">> := NetJson} = JSON,
  {ok, NetDefs} = case NetJson of
    #{<<"cidr4">> := Cidr4, <<"cidr6">> := Cidr6} ->
      {Addr4, Prefixlen4} = virtuerl_net:parse_cidr(Cidr4),
      {Addr6, Prefixlen6} = virtuerl_net:parse_cidr(Cidr6),
      {ok, [{Addr4, Prefixlen4}, {Addr6, Prefixlen6}]};
    #{<<"cidr4">> := Cidr4} ->
      {Addr4, Prefixlen4} = virtuerl_net:parse_cidr(Cidr4),
      {ok, [{Addr4, Prefixlen4}]};
    #{<<"cidr6">> := Cidr6} ->
      {Addr6, Prefixlen6} = virtuerl_net:parse_cidr(Cidr6),
      {ok, [{Addr6, Prefixlen6}]};
    _ ->
      {error, no_cidrs_provided}
  end,

  io:format("POST~n"),
  io:format("NetworkDefs ~p~n", [NetDefs]),
  {ok, ID} = virtuerl_ipam:ipam_create_net(NetDefs),
  RespJSON = thoas:encode(#{id => ID}),
  mochiweb_request:respond({201, [{"Content-Type", "application/json"}, {"Location", "/networks/" ++ binary_to_list(ID)}], RespJSON}, Req);
handle(network, 'PUT', #{id := ID}, Req) ->
  JSON = parse_json(Req),
  #{<<"network">> := #{<<"cidr">> := CIDR}} = JSON,
  {Addr, Prefixlen} = virtuerl_net:parse_cidr(CIDR),
  io:format("PUT: ~p~n", [ID]),
  io:format("NetworkDef ~p/~p~n", [Addr, Prefixlen]),
  virtuerl_ipam:ipam_put_net({list_to_binary(ID), Addr, Prefixlen}),
  mochiweb_request:ok({[], "HELLO\n"}, Req);
handle(network, 'GET', #{id := ID}, Req) ->
  io:format("GET: ~p~n", [ID]),
  {ok, Net} = virtuerl_ipam:ipam_get_net(list_to_binary(ID)),
  RespJson = thoas:encode(Net),
  mochiweb_request:respond({200, [{"Content-Type", "application/json"}], RespJson}, Req);
handle(network, 'DELETE', #{id := ID}, Req) ->
  io:format("DELETE: ~p~n", [ID]),
  ok = virtuerl_ipam:ipam_delete_net(list_to_binary(ID)),
  mochiweb_request:respond({204, [{"Content-Type", "application/json"}], <<"">>}, Req);

handle(domains, 'POST', _, Req) ->
  JSON = parse_json(Req),
  #{<<"domain">> := #{<<"network_id">> := NetworkID}} = JSON,
  #{<<"domain">> := SubJSON} = JSON,
  Addresses0 = maps:with([<<"ipv4_addr">>, <<"ipv6_addr">>], SubJSON),
  DomainMap0 = maps:from_list([{binary_to_atom(K), V} || {K, V} <- maps:to_list(Addresses0)]),
  DomainMap1 = DomainMap0#{network_id => NetworkID},
  {ok, Resp} = virtuerl_mgt:domain_create(DomainMap1),
  #{id := DomainID} = Resp,
  RespJSON = thoas:encode(Resp),
  mochiweb_request:respond({201, [{"Content-Type", "application/json"}, {"Location", "/domains/" ++ binary_to_list(DomainID)}], RespJSON}, Req);
handle(domain, 'GET', #{id := ID}, Req) ->
  io:format("DOMAIN GET: ~p~n", [ID]),
  DomResp = virtuerl_mgt:domain_get(#{id => list_to_binary(ID)}),
  io:format("~p~n", [DomResp]),
  case DomResp of
    {ok, Dom} ->
      mochiweb_request:ok({[], thoas:encode(Dom)}, Req);
    notfound ->
      mochiweb_request:not_found(Req)
  end;
handle(domain, 'DELETE', #{id := ID}, Req) ->
  io:format("DOMAIN DELETE: ~p~n", [ID]),
  mochiweb_request:respond({204, [{"Content-Type", "application/json"}], <<"">>}, Req).
