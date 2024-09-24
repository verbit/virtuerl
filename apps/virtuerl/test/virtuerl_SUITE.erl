-module(virtuerl_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_create_domain/1, test_create_domain_dualstack/1]).


all() -> [test_create_domain, test_create_domain_dualstack].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(virtuerl),
    virtuerl_server:start(default, #{cluster => default}),
    virtuerl_mgt_sup:start(default),
    Config.


end_per_suite(_Config) ->
    application:stop(virtuerl).


test_create_domain(_Config) ->
    {ok, NetID} = virtuerl_ipam:ipam_create_net([{<<192:8, 168:8, 17:8, 0:8>>, 24}]),
    {ok, #{id := DomId, ipv4_addr := <<"192.168.17.8">>}} =
        virtuerl_mgt:domain_create(
          #{
            name => "test_domain",
            vcpu => 1,
            memory => 512,
            network_id => NetID,
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
    {ok, {{_, 200, _}, _, _}} = wait_for_http("http://192.168.17.8/", 5 * 60 * 1000),

    % make sure address is actually released and reused
    virtuerl_pubsub:subscribe(),
    virtuerl_mgt:domain_delete(#{id => DomId}),
    receive
        {domain_deleted, DomId} -> ok
    end,
    {ok, #{id := Dom2Id, ipv4_addr := <<"192.168.17.8">>}} =
        virtuerl_mgt:domain_create(#{name => "test_domain_2", vcpu => 1, memory => 512, network_id => NetID, user_data => ""}),
    virtuerl_mgt:domain_delete(#{id => Dom2Id}),
    receive
        {domain_deleted, Dom2Id} -> ok
    end,

    virtuerl_ipam:ipam_delete_net(NetID),

    ok.


test_create_domain_dualstack(_Config) ->
    {ok, NetID} = virtuerl_ipam:ipam_create_net([{<<192:8, 168:8, 17:8, 0:8>>, 24}, {<<16#fd58:16, 16#40cb:16, 16#bddb:16, 0:80>>, 48}]),
    {ok, #{id := DomId, ipv4_addr := <<"192.168.17.8">>, ipv6_addr := <<"fd58:40cb:bddb::8">>}} =
        virtuerl_mgt:domain_create(
          #{
            name => "test_domain",
            vcpu => 1,
            memory => 512,
            network_id => NetID,
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
    {ok, {{_, 200, _}, _, _}} = wait_for_http("http://[fd58:40cb:bddb::8]/", 5 * 60 * 1000),
    {ok, {{_, 200, _}, _, _}} = wait_for_http("http://192.168.17.8/", 5 * 60 * 1000),

    virtuerl_pubsub:subscribe(),
    virtuerl_mgt:domain_delete(#{id => DomId}),
    receive
        {domain_deleted, DomId} -> ok
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
