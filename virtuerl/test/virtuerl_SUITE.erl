%%%-------------------------------------------------------------------
%%% @author ilya-stroeer
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2023 9:48 PM
%%%-------------------------------------------------------------------
-module(virtuerl_SUITE).
-author("ilya-stroeer").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_domain/1, test_network/1]).

all() -> [test_network,test_domain].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(virtuerl),
  Config.

end_per_suite(_Config) ->
  ok.

test_network(_Config) ->
  NetJson = thoas:encode(#{"network" => #{"cidr4" => "192.168.111.0/24", "cidr6" => "2001:db8::/80"}}),
  {ok, {{_, 201, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/networks", [], "application/json", NetJson}, [], []),
  Loc = proplists:get_value("location", Headers),
  NetUri = uri_string:resolve(Loc, "http://localhost:8080"),

  {ok, {{_, 200, _}, _, NetBody}} = httpc:request(get, {NetUri, []}, [], []),
  {ok, #{<<"cidrs">> := [<<"192.168.111.0/24">>, <<"2001:db8::/80">>]}} = thoas:decode(NetBody),

  {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {NetUri, []}, [], []).

test_domain(_Config) ->
  NetJson = thoas:encode(#{"network" => #{"cidr4" => "192.168.111.0/24", "cidr6" => "2001:db8::/80"}}),
  {ok, {{_, 201, _}, NetHeaders, NetBody}} = httpc:request(post, {"http://localhost:8080/networks", [], "application/json", NetJson}, [], []),
  NetLoc = proplists:get_value("location", NetHeaders),
  NetUri = uri_string:resolve(NetLoc, "http://localhost:8080"),
  {ok, #{<<"id">> := NetId}} = thoas:decode(NetBody),

  DomainJson = thoas:encode(#{"domain" => #{"network_id" => NetId, "ipv4_addr" => "192.168.111.39"}}),
  {ok, {{_, 201, _}, DomainHeaders, DomainBody}} = httpc:request(post, {"http://localhost:8080/domains", [], "application/json", DomainJson}, [], []),
  DomainLoc = proplists:get_value("location", DomainHeaders),
  DomainUri = uri_string:resolve(DomainLoc, "http://localhost:8080"),
  {ok, #{<<"ipv4_addr">> := <<"192.168.111.39">>, <<"ipv6_addr">> := <<"2001:db8::8">>}} = thoas:decode(DomainBody),

  {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {DomainUri, []}, [], []),
  {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {NetUri, []}, [], []).
