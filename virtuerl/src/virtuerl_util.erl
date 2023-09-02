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
-export([uuid4/0]).

uuid4() ->
  ID = string:lowercase(binary:encode_hex(<<(rand:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)-1):128>>)),
  <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = ID,
  Uuid4 = iolist_to_binary([A, "-", B, "-", C, "-", D, "-", E]),
  Uuid4.
