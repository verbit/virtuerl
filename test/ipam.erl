-module(ipam).
-include_lib("eunit/include/eunit.hrl").
-include_lib("khepri/src/khepri_error.hrl").


simple_test() ->
  ?assert(0 == 0).

fixture_test_() ->
  {setup,
    fun() ->
      {ok, StoreId} = khepri:start("khepri-test", test),
      {ok, Pid} = virtuerl_app:start_server(StoreId),
      {StoreId, Pid}
    end,
    fun({StoreId, Pid}) ->
      virtuerl_app:stop_server(Pid),
      khepri:stop(StoreId),
      case file:del_dir_r("khepri-test") of
        {error, enoent} ->
          ok;
        ok ->
          ok
      end
    end,
    fun({StoreId, Pid}) ->
      [
        {"network4", fun() -> network_test_4(StoreId) end},
        {"network6", fun() -> network_test_6(StoreId) end},
        {"test-me", fun() -> test_me(StoreId) end},
        {"test-me6", fun() -> test_me6(StoreId) end}
      ]
    end
  }.

network_test_4(StoreId) ->
  NetId = <<"aabbcc">>,
  {error, network_too_small} = virtuerl_app:ipam_put_net({NetId, <<192:8, 168:8, 10:8, 0:8>>, 29}),
  {error, ?khepri_error(node_not_found, _)} = khepri:get(StoreId, [network, NetId]),
  ok = khepri:delete(StoreId, [network, NetId]).

network_test_6(StoreId) ->
  NetId = <<"aabbcc">>,
  {error, network_too_small} = virtuerl_app:ipam_put_net({NetId, <<16#20010db8000000000000000000000000:128>>, 125}),
  {error, ?khepri_error(node_not_found, _)} = khepri:get(StoreId, [network, NetId]),
  ok = khepri:delete(StoreId, [network, NetId]).

test_me(StoreId) ->
  NetId = <<"aabbcc">>,
  {error, network_not_found} = virtuerl_app:ipam_next_ip(NetId),
  ok = virtuerl_app:ipam_put_net({NetId, <<192:8, 168:8, 10:8, 0:8>>, 28}),
%%  {ok, NextIP} = virtuerl_app:ipam_next_ip(NetId),
  {ok, Net} = khepri:get(StoreId, [network, NetId]),
  ?debugVal(Net),
  {Time, {ok, NextIP}} = timer:tc(virtuerl_app, ipam_next_ip, [NetId]),
  ?debugVal(Time),
  ?debugVal(NextIP),
  ?assertEqual(<<192:8, 168:8, 10:8, 8:8>>, NextIP),
  {TimeSecond, {ok, _}} = timer:tc(virtuerl_app, ipam_next_ip, [NetId]),
  ?debugVal(TimeSecond),
  [{ok, _} = virtuerl_app:ipam_next_ip(NetId) || _ <- lists:seq(1, 6)],
  {TimeLast, {error, no_ip_available}} = timer:tc(virtuerl_app, ipam_next_ip, [NetId]),
  ?debugVal(TimeLast),
  ok = khepri:delete(StoreId, [network, NetId]).

test_me6(StoreId) ->
  NetId = <<"aabbcc">>,
  {error, network_not_found} = virtuerl_app:ipam_next_ip(NetId),
  ok = virtuerl_app:ipam_put_net({NetId, <<16#20010db8000000000000000000000000:128>>, 124}),
%%  {ok, NextIP} = virtuerl_app:ipam_next_ip(NetId),
  {ok, Net} = khepri:get(StoreId, [network, NetId]),
  ?debugVal(Net),
  {Time, {ok, NextIP}} = timer:tc(virtuerl_app, ipam_next_ip, [NetId]),
  ?debugVal(Time),
  ?debugVal(NextIP),
  ?assertEqual(<<16#20010db8000000000000000000000008:128>>, NextIP),
  {TimeSecond, {ok, _}} = timer:tc(virtuerl_app, ipam_next_ip, [NetId]),
  ?debugVal(TimeSecond),
  [{ok, _} = virtuerl_app:ipam_next_ip(NetId) || _ <- lists:seq(1, 6)],
  {TimeLast, {error, no_ip_available}} = timer:tc(virtuerl_app, ipam_next_ip, [NetId]),
  ?debugVal(TimeLast),
  ok = khepri:delete(StoreId, [network, NetId]).

