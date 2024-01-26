%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2021. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%%-------------------------------------------------------------------
%%% File    : hello.erl
%%% Author  : Matthew Harrison <harryhuk at users.sourceforge.net>
%%% Description : _really_ minimal example of a wxerlang app
%%%               implemented with wx_object behaviour
%%%
%%% Created :  18 Sep 2008 by  Matthew Harrison <harryhuk at users.sourceforge.net>
%%%            Dan rewrote it to show wx_object behaviour
%%%-------------------------------------------------------------------
-module(virtuerl_vnc).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([
  init/1, handle_info/2, handle_event/2,
  code_change/3, terminate/2]).
-export([start/1]).

-behaviour(wx_object).

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

start(Parent) ->
  wx_object:start_link(?MODULE, [Parent], []).

%% Init is called in the new process.
init([Parent]) ->
  {ok, Socket} = gen_tcp:connect("localhost", 5901, [{active, true}, binary]),
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
%%  #vnc_conf{width = Width, height = Height} = #vnc_conf{width = 800, height = 600},
  inet:setopts(Socket, [{active, once}]),
  ok = gen_tcp:send(Socket, <<?vncMSG_TYPE_FramebufferUpdateRequest, 0, 0:16, 0:16, VncWidth:16, VncHeight:16>>),

  {Mx, My, _, _} = wxWindow:getTextExtent(Parent, "M"),
%%  wxFrame:setClientSize(Frame, {60*Mx, 20*My}),
  Panel = wxGLCanvas:new(Parent, [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}]),
  Context = wxGLContext:new(Panel),
%%  wxWindow:setMinSize(Panel, {VncWidth, VncHeight}),

  wxGLCanvas:connect(Panel, key_down),
  wxGLCanvas:connect(Panel, key_up),

  wxGLCanvas:setCurrent(Panel, Context),

gl:enable(?GL_TEXTURE_2D),
  [TexId] = gl:genTextures(1),
  io:format("TEXTURE ID: ~p~n", [TexId]),
  gl:bindTexture(?GL_TEXTURE_2D, TexId),
  gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB, VncWidth, VncHeight, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, 0),
  gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),
  Err = gl:getError(),
  io:format("ERROR: ~p~n", [glu:errorString(Err)]),
  wxGLCanvas:connect(Panel, erase_background, [{callback, fun(_,_) -> ok end}]),
  wxGLCanvas:connect(Panel, paint),
  {Panel, #state{socket=Socket, panel = Panel, tex_id = TexId, vnc_conf = VncConf}}.

read_rects(0, _, Socket) ->
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

%% Handled as in normal gen_server callbacks
handle_info({tcp, Socket, <<?vncMSG_TYPE_FramebufferUpdate, _Padding, NumRects:16/integer, Rest/binary>>}, #state{panel = Panel, tex_id = TexId, vnc_conf = #vnc_conf{width = Width, height = Height}} = State) ->
  {TimeRects, Rects} = timer:tc(fun() -> read_rects(NumRects, Rest, Socket) end),
  {TimeRes, _} = timer:tc(fun() ->
    wx:batch(fun() ->
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    [gl:texSubImage2D(?GL_TEXTURE_2D, 0, X, Y, Width, Height, ?GL_BGRA, ?GL_UNSIGNED_BYTE, Data) || {X, Y, Width, Height, Data} <- Rects]
      end),
    gl:flush()
           end),
  wxPanel:refresh(Panel, [{eraseBackground, false}]),
%%  io:format("Writing rects took ~p/~p~n", [TimeRects, TimeRes]),

  inet:setopts(Socket, [{active, once}]),
  ok = gen_tcp:send(Socket, <<?vncMSG_TYPE_FramebufferUpdateRequest, 1, 0:16, 0:16, Width:16, Height:16>>),
  {noreply, State};

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_event(#wx{event=#wxPaint{type = paint}, obj = _Obj}, State = #state{win=Frame, panel=Panel, tex_id = TexId, vnc_conf = #vnc_conf{width = Width, height = Height}}) ->
  {W, H} = wxGLCanvas:getSize(_Obj),
  Wscale = W / Width,
  Hscale = H / Height,
  {Scale, Xt, Yt} = case Wscale > Hscale of
                      true ->
                        {Hscale, max((W - Width*Hscale) / 2, 0), 0};
                      false ->
                        {Wscale, 0, max((H - Height*Wscale) / 2, 0)}
                    end,

  {ScaleX, ScaleY} = {Width * Scale / W, Height * Scale / H},
  gl:viewport(0,0,W,H),

  gl:matrixMode(?GL_MODELVIEW),
  gl:loadIdentity(),
  gl:scalef(ScaleX, ScaleY, 0.0),
  gl:clearColor(0.2, 0.2, 0.2, 1.0),
  gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
  gl:bindTexture(?GL_TEXTURE_2D, TexId),
  gl:enable(?GL_TEXTURE_2D),
  gl:'begin'(?GL_QUADS),

  gl:texCoord2f(0.0, 1.0),
  gl:vertex2f(-1.0, -1.0),
  gl:texCoord2f(0.0, 0.0),
  gl:vertex2f(-1.0,  1.0),
  gl:texCoord2f(1.0, 0.0),
  gl:vertex2f( 1.0,  1.0),
  gl:texCoord2f(1.0, 1.0),
  gl:vertex2f( 1.0, -1.0),

  gl:'end'(),
  gl:flush(),
  wxGLCanvas:swapBuffers(_Obj),
  {noreply, State};
handle_event(#wx{event=#wxKey{type = Type, keyCode = KeyCode, rawCode = Key}} = Event, State = #state{win=Frame, panel=Panel, socket = Socket}) ->
  io:format("Event ~p~n", [Event]),
  DownFlag = case Type of
    key_down -> 1;
    key_up -> 0
  end,
  ok = gen_tcp:send(Socket, <<?vncMSG_TYPE_KeyEvent, DownFlag, 0:16, Key:32>>),
  {noreply, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, State =  #state{win=Frame}) ->
  io:format("~p Quitting window ~n",[self()]),
  ok = wxFrame:setStatusText(Frame, "Closing...",[]),
  wxWindow:destroy(Frame),
  {stop, normal, State};
handle_event(Event, State) ->
  io:format("Got Event ~p~n",[Event]),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  ok.
