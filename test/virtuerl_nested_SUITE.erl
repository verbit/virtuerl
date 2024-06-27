-module(virtuerl_nested_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_create_domain/1]).

-include_lib("common_test/include/ct.hrl").


all() -> [test_create_domain].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(virtuerl),
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
            inbound_rules => [#{protocols => ["tcp"], target_ports => [80, 22]}],
            user_data =>
                "#cloud-config

users:
  - name: tester
    passwd: $6$Cf1HnaIWk8TunKFs$40sITB7utYJbVL9kkmhVwzCW33vbq55IGSbpLp1AqOufng1qNxf8wyHj4fdp3xMAfr0yrGioiWwtRvbN58rlI.
    lock_passwd: false
    shell: /bin/bash
    sudo: ALL=(ALL) NOPASSWD:ALL
    ssh_authorized_keys:
      - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJMKtCLcW+LcoEWUfYBuDbaaNkiRoa1CsB+2nWNHIhvi

apt:
  primary:
    - arches: [default]
      search:
        - http://mirror.ipb.de/debian/
        - http://security.debian.org/debian-security

packages:
  - erlang-base
  - nginx

runcmd:
  - service nginx restart
"
           }),  % password: asd
    {ok, {{_, 200, _}, _, _}} = wait_for_http("http://192.168.17.8/", 5 * 60 * 1000),

    ok = file:write_file("id_ed25519",
                         "-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACCTCrQi3Fvi3KBFlH2Abg22mjZIkaGtQrAftp1jRyIb4gAAAJDJNDQ7yTQ0
OwAAAAtzc2gtZWQyNTUxOQAAACCTCrQi3Fvi3KBFlH2Abg22mjZIkaGtQrAftp1jRyIb4g
AAAEDIBLrG0qYlS4tvkqCVC2o4tsrlAbK7DMRmAz8sFxmCJJMKtCLcW+LcoEWUfYBuDbaa
NkiRoa1CsB+2nWNHIhviAAAACmlseWFAdDQ2MHMBAgM=
-----END OPENSSH PRIVATE KEY-----
"),
    ok = file:change_mode("id_ed25519", 8#00600),

    Ssh = os:find_executable("ssh"),
    {ok, Peer, _Node} =
        peer:start_link(#{
                          exec => {Ssh,
                                   ["-i", "id_ed25519",
                                    "-o", "StrictHostKeyChecking=no",
                                    "-o", "UserKnownHostsFile=/dev/null",
                                    "tester@192.168.17.8",
                                    "erl"]},
                          connection => standard_io
                         }),
    {ok, Ips} = peer:call(Peer, inet, getifaddrs, []),
    ct:print("IPs: ~p~n", [Ips]),
    peer:stop(Peer),

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
