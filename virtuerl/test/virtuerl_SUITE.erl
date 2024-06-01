%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2023 9:48 PM
%%%-------------------------------------------------------------------
-module(virtuerl_SUITE).
-author("ilya").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_domain/1, test_network/1, test_create_domain/1, test_create_domain_dualstack/1]).

all() -> [test_create_domain, test_create_domain_dualstack].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(virtuerl),
  Config.

end_per_suite(_Config) ->
  application:stop(virtuerl).

test_network(_Config) ->
  NetJson = thoas:encode(#{"network" => #{"cidr4" => "192.168.111.0/24", "cidr6" => "2001:db8::/80"}}),
  {ok, {{_, 201, _}, Headers, _}} = httpc:request(post, {"http://localhost:8081/networks", [], "application/json", NetJson}, [], []),
  Loc = proplists:get_value("location", Headers),
  NetUri = uri_string:resolve(Loc, "http://localhost:8081"),

  {ok, {{_, 200, _}, _, NetBody}} = httpc:request(get, {NetUri, []}, [], []),
  {ok, #{<<"cidrs">> := [<<"192.168.111.0/24">>, <<"2001:db8::/80">>]}} = thoas:decode(NetBody),

  {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {NetUri, []}, [], []).

test_domain(_Config) ->
  NetJson = thoas:encode(#{"network" => #{"cidr4" => "192.168.111.0/24", "cidr6" => "2001:db8::/80"}}),
  {ok, {{_, 201, _}, NetHeaders, NetBody}} = httpc:request(post, {"http://localhost:8081/networks", [], "application/json", NetJson}, [], []),
  NetLoc = proplists:get_value("location", NetHeaders),
  NetUri = uri_string:resolve(NetLoc, "http://localhost:8081"),
  {ok, #{<<"id">> := NetId}} = thoas:decode(NetBody),

  DomainJson = thoas:encode(#{"domain" => #{"network_id" => NetId, "ipv4_addr" => "192.168.111.39"}}),
  {ok, {{_, 201, _}, DomainHeaders, DomainBody}} = httpc:request(post, {"http://localhost:8081/domains", [], "application/json", DomainJson}, [], []),
  DomainLoc = proplists:get_value("location", DomainHeaders),
  DomainUri = uri_string:resolve(DomainLoc, "http://localhost:8081"),
  {ok, #{<<"mac_addr">> := MacAddr, <<"ipv4_addr">> := <<"192.168.111.39">>, <<"ipv6_addr">> := <<"2001:db8::8">>}} = thoas:decode(DomainBody),
  <<_:6, Laa:2, _/binary>> = binary:decode_hex(MacAddr),
  2 = Laa,

%%  {ok, {{_, 200, _}, _, DomainBody1}} = httpc:request(DomainUri),
%%  {ok, #{<<"mac_addr">> := MacAddr, <<"ipv4_addr">> := <<"192.168.111.39">>, <<"ipv6_addr">> := <<"2001:db8::8">>}} = thoas:decode(DomainBody1),

  {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {DomainUri, []}, [], []),
  {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {NetUri, []}, [], []).

test_create_domain(_Config) ->
  {ok, NetID} = virtuerl_ipam:ipam_create_net([{<<192:8,168:8,17:8,0:8>>, 24}]),
  {ok, #{id := DomId, ipv4_addr := <<"192.168.17.8">>}} = virtuerl_mgt:domain_create(#{name => "test_domain", vcpu => 1, memory => 512, network_id => NetID,
    inbound_rules => [#{protocols => ["tcp"], target_ports => [80]}],
    user_data =>
"#cloud-config

users:
  - name: tester
    passwd: $6$Cf1HnaIWk8TunKFs$40sITB7utYJbVL9kkmhVwzCW33vbq55IGSbpLp1AqOufng1qNxf8wyHj4fdp3xMAfr0yrGioiWwtRvbN58rlI.
    lock_passwd: false
    shell: /bin/bash
    sudo: ALL=(ALL) NOPASSWD:ALL
    ssh_authorized_keys:
      - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBDCT3LrJenezXzP9T6519IgpVCP1uv6f5iQwZ+IDdFc

apt:
  primary:
    - arches: [default]
      search:
        - http://mirror.ipb.de/debian/
        - http://security.debian.org/debian-security

packages:
  - nginx

runcmd:
  - service nginx restart
"
    }),  % password: asd
  {ok, {{_, 200, _}, _, _}} = wait_for_http("http://192.168.17.8/", 5*60*1000),

  % make sure address is actually released and reused
  virtuerl_pubsub:subscribe(),
  virtuerl_mgt:domain_delete(#{id => DomId}),
  receive
    {domain_deleted, DomId} ->ok
  end,
  {ok, #{id := Dom2Id, ipv4_addr := <<"192.168.17.8">>}} = virtuerl_mgt:domain_create(#{name => "test_domain_2", vcpu => 1, memory => 512, network_id => NetID, user_data => ""}),
  virtuerl_mgt:domain_delete(#{id => Dom2Id}),
  receive
    {domain_deleted, Dom2Id} ->ok
  end,

  virtuerl_ipam:ipam_delete_net(NetID),

  ok.

test_create_domain_dualstack(_Config) ->
  {ok, NetID} = virtuerl_ipam:ipam_create_net([{<<192:8,168:8,17:8,0:8>>, 24}, {<<16#fd58:16, 16#40cb:16, 16#bddb:16, 0:80>>, 48}]),
  {ok, #{id := DomId, ipv4_addr := <<"192.168.17.8">>, ipv6_addr := <<"fd58:40cb:bddb::8">>}} = virtuerl_mgt:domain_create(#{name => "test_domain", vcpu => 1, memory => 512, network_id => NetID,
    inbound_rules => [#{protocols => ["tcp"], target_ports => [80]}],
    user_data =>
"#cloud-config

users:
  - name: tester
    passwd: $6$Cf1HnaIWk8TunKFs$40sITB7utYJbVL9kkmhVwzCW33vbq55IGSbpLp1AqOufng1qNxf8wyHj4fdp3xMAfr0yrGioiWwtRvbN58rlI.
    lock_passwd: false
    shell: /bin/bash
    sudo: ALL=(ALL) NOPASSWD:ALL
    ssh_authorized_keys:
      - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBDCT3LrJenezXzP9T6519IgpVCP1uv6f5iQwZ+IDdFc

apt:
  primary:
    - arches: [default]
      search:
        - http://mirror.ipb.de/debian/
        - http://security.debian.org/debian-security

packages:
  - nginx

runcmd:
  - service nginx restart
"
    }),  % password: asd
  {ok, {{_, 200, _}, _, _}} = wait_for_http("http://[fd58:40cb:bddb::8]/", 5*60*1000),
  {ok, {{_, 200, _}, _, _}} = wait_for_http("http://192.168.17.8/", 5*60*1000),

  virtuerl_pubsub:subscribe(),
  virtuerl_mgt:domain_delete(#{id => DomId}),
  receive
    {domain_deleted, DomId} ->ok
  end,

  virtuerl_ipam:ipam_delete_net(NetID),

  ok.

wait_for_http(Url, Timeout) ->
  Deadline = erlang:system_time(millisecond) + Timeout,
  do_wait_for_http(Url, Deadline).

do_wait_for_http(Url, Deadline) ->
  case httpc:request(get, {Url, []}, [{timeout, 1000}], []) of
    {error, _} ->
        case erlang:system_time(millisecond) > Deadline of
          true ->
            {error, timeout};
          false ->
            timer:sleep(2000),
            do_wait_for_http(Url, Deadline)
        end;
    Res -> Res
  end.
