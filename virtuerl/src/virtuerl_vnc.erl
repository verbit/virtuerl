-module(virtuerl_vnc).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([
  init/1, handle_info/2, handle_event/2,
  code_change/3, terminate/2]).
-export([start/3]).

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

-record(state, {win, socket, panel, tex_id, vnc_conf, parent}).
-record(vnc_conf, {width, height}).

start(Parent, DomainId, Node) ->
  wx_object:start_link(?MODULE, [Parent, DomainId, Node], []).

%% Init is called in the new process.
init([Parent, DomainId, Node]) ->
  {Mx, My, _, _} = wxWindow:getTextExtent(Parent, "M"),
%%  wxFrame:setClientSize(Frame, {60*Mx, 20*My}),
  Panel = wxGLCanvas:new(Parent, [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}]),
  Context = wxGLContext:new(Panel),
%%  wxWindow:setMinSize(Panel, {VncWidth, VncHeight}),

  wxGLCanvas:connect(Panel, key_down),
  wxGLCanvas:connect(Panel, key_up),

  wxGLCanvas:setCurrent(Panel, Context),

  VncProxy = virtuerl_vnc_proxy:start(DomainId, Node),
  {VncWidth, VncHeight} = receive
    {conf, TVncWidth, TVncHeight} -> {TVncWidth, TVncHeight}
                          after 1000 ->
      io:format("the foock?~n"),
      {700, 400}
  end,

  VncProxy ! {framebuffer_update_request, 0, 0, 0, VncWidth, VncHeight},

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
  {Panel, #state{socket=VncProxy, parent = Parent, panel = Panel, tex_id = TexId, vnc_conf = {VncWidth, VncHeight}}}.

%% Handled as in normal gen_server callbacks
handle_info({framebuffer_update, Rects}, #state{socket = Proxy, panel = Panel, tex_id = TexId, vnc_conf = {VncWidth, VncHeight}} = State) ->
%%  io:format("got framebuffer_update~n"),
    wx:batch(fun() ->
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    [gl:texSubImage2D(?GL_TEXTURE_2D, 0, X, Y, Width, Height, ?GL_BGRA, ?GL_UNSIGNED_BYTE, Data) || {X, Y, Width, Height, Data} <- Rects]
      end),
    gl:flush(),
  wxPanel:refresh(Panel, [{eraseBackground, false}]),
%%  io:format("Writing rects took ~p/~p~n", [TimeRects, TimeRes]),


  Proxy ! {framebuffer_update_request, 1, 0, 0, VncWidth, VncHeight},
  {noreply, State};

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_event(#wx{event=#wxPaint{type = paint}, obj = _Obj}, State = #state{parent = Parent, win=Frame, panel=Panel, tex_id = TexId, vnc_conf = {Width, Height}}) ->
  {W, H} = wxGLCanvas:getSize(Panel),
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
  wxGLCanvas:swapBuffers(Panel),
  {noreply, State};
handle_event(#wx{event=#wxKey{type = Type, keyCode = KeyCode, rawCode = Key}} = Event, State = #state{win=Frame, panel=Panel, socket = Socket}) ->
  io:format("Event ~p~n", [Event]),
  DownFlag = case Type of
    key_down -> 1;
    key_up -> 0
  end,
  Socket ! {key_event, DownFlag, Key},
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
