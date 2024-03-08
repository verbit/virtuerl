%%%-------------------------------------------------------------------
%%% @author ilya-stroeer
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2023 4:05 PM
%%%-------------------------------------------------------------------
-module(virtuerl_util).
-author("ilya-stroeer").

%% API
-export([uuid4/0, mac_to_str/1, delete_file/1, cmd/1]).

uuid4() ->
  ID = string:lowercase(binary:encode_hex(<<(rand:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)-1):128>>)),
  <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = ID,
  Uuid4 = iolist_to_binary([A, "-", B, "-", C, "-", D, "-", E]),
  Uuid4.

mac_to_str(<<Mac:48>>) ->
  <<A:16, B:16, C:16, D:16, E:16, F:16>> = string:lowercase(binary:encode_hex(<<Mac:48>>)),
  <<A:16, $::8, B:16, $::8, C:16, $::8, D:16, $::8, E:16, $::8, F:16>>.

delete_file(Filename) ->
  case file:delete(Filename) of
    ok -> ok;
    {error, enoent} -> ok;
    Other -> Other
  end.

cmd(Cmd) ->
  Port = open_port({spawn, Cmd}, [exit_status]),
  receive
    {Port, {exit_status, 0}} -> ok;
    {Port, {exit_signal, _}} -> error
  end.
