%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_vnc_proxy).

-export([start/1]).

-define(SERVER, ?MODULE).
-define(APPLICATION, virtuerl).

-define(vncINVALID, 0).
-define(vncNONE, 1).
-define(vncVNC_AUTH, 2).
-define(vncOK, 0).
-define(vncFAILED, 1).
-define(vncMSG_TYPE_FramebufferUpdate, 0).
-define(vncMSG_TYPE_FramebufferUpdateRequest, 3).
-define(vncMSG_TYPE_KeyEvent, 4).
-define(vncMSG_TYPE_PointerEvent, 5).
-define(vncENCODING_RAW, 0).

-record(state, {win, socket, panel, tex_id, vnc_conf}).
-record(vnc_conf, {width, height}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start(DomainId) ->
  VncSocketPath = filename:join([virtuerl_mgt:home_path(), "domains", DomainId, "vnc.sock"]),
  Self = self(),
  spawn(fun() -> init(VncSocketPath, Self) end).

init(VncSocketPath, Sender) ->
  {ok, Socket} = gen_tcp:connect({local, VncSocketPath}, 0, [{active, true}, binary, local]),
  receive
    {tcp, Socket, <<"RFB ", Major:3/binary, ".", Minor:3/binary, "\n">>} ->
      io:format("Major: ~s / Minor: ~s~n", [Major, Minor]),
      ok
  after 2000 ->
    timeout
  end,

  ok = gen_tcp:send(Socket, <<"RFB 003.008\n">>),
  receive
    {tcp, Socket, <<NumAuthTypes:8/integer, AuthType:NumAuthTypes/binary>>} ->
      % TODO: AuthTypes is a list
      io:format("AuthType: ~p~n", [AuthType]),
      case AuthType of
        ?vncINVALID -> {error, auth_invalid};
        ?vncNONE -> ok;
        ?vncVNC_AUTH -> {error, auth_not_supported};
        _ -> io:format("Unknown auth: ~p~n", [AuthType])
      end
  after 2000 ->
    {error, timeout}
  end,

  ok = gen_tcp:send(Socket, <<?vncNONE>>),
  receive
    {tcp, Socket, <<SecurityResult:32/integer>>} ->
      io:format("Security result: ~B~n", [SecurityResult]),
      case SecurityResult of
        ?vncOK -> ok;
        ?vncFAILED -> error
      end
  after 2000 ->
    {error, timeout}
  end,

  ok = gen_tcp:send(Socket, <<1>>),
  VncConf = receive
              {tcp, Socket, <<Width:16/integer, Height:16/integer,
                BitsPerPixel:8/integer, Depth:8/integer, BigEndianFlag:8/integer, TrueColorFlag:8/integer,
                RedMax:16/integer, GreenMax:16/integer, BlueMax:16/integer,
                RedShift:8/integer, GreenShift:8/integer, BlueShift:8/integer,
                _:3/binary,
                NameLength:32/integer, Name:NameLength/binary>>} ->
                io:format("~s: ~Bx~B~n~B/~B (BE: ~B, TC: ~B)~nR:(X>>~B)&~B  G:(X>>~B)&~B  B:(X>>~B)&~B~n", [Name, Width, Height, Depth, BitsPerPixel, BigEndianFlag, TrueColorFlag, RedShift, RedMax, GreenShift, GreenMax, BlueShift, BlueMax]),
                #vnc_conf{width = Width, height = Height}
            after 2000 ->
      {error, timeout}
            end,
  #vnc_conf{width = VncWidth, height = VncHeight} = VncConf,
  inet:setopts(Socket, [{active, once}]),

  Sender ! {conf, VncWidth, VncHeight},

  monitor(process, Sender),
  loop(Socket, Sender).

loop(Socket, Sender) ->
  receive
    {tcp, Socket, <<?vncMSG_TYPE_FramebufferUpdate, _Padding, NumRects:16/integer, Rest/binary>>} ->
      {TimeRects, Rects} = timer:tc(fun() -> read_rects(NumRects, Rest, Socket) end),
      Sender ! {framebuffer_update, Rects},
      inet:setopts(Socket, [{active, once}]),
      loop(Socket, Sender);
    {tcp, Socket,_Data} ->
      io:format("Got data: ~p~n", [_Data]),
      inet:setopts(Socket, [{active, once}]),
      loop(Socket, Sender);
    {framebuffer_update_request, Incremental, X, Y, Width, Height} ->
      ok = gen_tcp:send(Socket, <<?vncMSG_TYPE_FramebufferUpdateRequest, Incremental, X:16, Y:16, Width:16, Height:16>>),
      loop(Socket, Sender);
    {key_event, DownFlag, Key} ->
      ok = gen_tcp:send(Socket, <<?vncMSG_TYPE_KeyEvent, DownFlag, 0:16, Key:32>>),
      loop(Socket, Sender);
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("Terminating because: ~p (~p)~n", [Reason, Pid]);
    _Message ->
      io:format("Got message: ~p~n", [_Message]),
      loop(Socket, Sender)
end.

read_rects(0, _Data, _Socket) ->
  [];
read_rects(NumRects, Data, Socket) ->
  case Data of
    <<X:16/integer, Y:16/integer, Width:16/integer, Height:16/integer, Encoding:32/signed-integer, Rest/binary>> ->
      case Encoding of
        ?vncENCODING_RAW ->
          NumBytes = Width * Height * 4,
          case NumBytes > byte_size(Rest) of
            true ->
              BytesToFetch = NumBytes - byte_size(Rest) + (NumRects - 1) * 12,
              {ok, MoreData} = gen_tcp:recv(Socket, BytesToFetch),
              read_rects(NumRects, <<Data/binary, MoreData/binary>>, Socket);
            false ->
              <<PixelBytes:NumBytes/binary, ActualRest/binary>> = Rest,
              Rect = {X, Y, Width, Height, PixelBytes},
              Rects = read_rects(NumRects - 1, ActualRest, Socket),
              [Rect | Rects]
          end;
        _ -> io:format("Unsupported encoding ~p~n", [Encoding])
      end;
    _ ->
      {ok, MoreData} = gen_tcp:recv(Socket, 12 - byte_size(Data) + (NumRects - 1) * 12),
      read_rects(NumRects, <<Data/binary, MoreData/binary>>, Socket)
  end.
