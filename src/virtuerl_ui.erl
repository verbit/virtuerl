-module(virtuerl_ui).

-behaviour(wx_object).

-export([start/0, start/1]).
-export([init/1,
         handle_info/2,
         handle_event/2,
         handle_call/3,
         code_change/3,
         terminate/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {
          win,
          network_panel,
          domain_panel,
          port_fwd_panel,
          toolbar,
          page,
          node
         }).


start() ->
    start(node()).


start(Node) ->
    wx_object:start_link(?MODULE, [Node], []).


%% Init is called in the new process.
init([Node]) ->
    erpc:call(Node, virtuerl_pubsub, subscribe, [self()]),

    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "Hello World", []),

    MenuBar = wxMenuBar:new(),
    Menu = wxMenu:new(),
    wxMenu:append(Menu, ?wxID_EXIT, "Quit"),
    wxMenuBar:append(MenuBar, Menu, "File"),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    {Mx, My, _, _} = wxFrame:getTextExtent(Frame, "M"),
    wxFrame:setClientSize(Frame, {80 * Mx, 30 * My}),

    Toolbar = create_toolbar(Frame, 100),

    wxFrame:createStatusBar(Frame, []),

    %% if we don't handle this ourselves, wxwidgets will close the window
    %% when the user clicks the frame's close button, but the event loop still runs
    wxFrame:connect(Frame, close_window),

    RootPanel = wxPanel:new(Frame, [{size, wxFrame:getSize(Frame)}]),
    Notebook = wxNotebook:new(RootPanel, ?wxID_ANY),

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, Notebook, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(RootPanel, Sizer),

    NetworkPanel = virtuerl_ui_net:start_link(Notebook, Toolbar, Node),
    DomainPanel = virtuerl_ui_dom:start_link(Notebook, Toolbar, Node),
    PortFwdPanel = virtuerl_ui_fwd:start_link(Notebook, Toolbar, Node),

    wxNotebook:addPage(Notebook, NetworkPanel, "Networks"),
    wxNotebook:addPage(Notebook, DomainPanel, "Domains"),
    wxNotebook:addPage(Notebook, PortFwdPanel, "Port Forwarding"),
    wxNotebook:connect(Notebook, command_notebook_page_changed),
    wxNotebook:setSelection(Notebook, 1),

    ok = wxFrame:setStatusText(Frame, "Hello World!", []),
    wxWindow:fit(Frame),
    wxWindow:show(Frame),
    wxWindow:raise(Frame),
    wxWindow:setFocus(Frame),
    wxWindow:layout(Frame),
    {Frame,
     #state{
       node = Node,
       toolbar = Toolbar,
       page = 1,
       win = Frame,
       network_panel = NetworkPanel,
       domain_panel = DomainPanel,
       port_fwd_panel = PortFwdPanel
      }}.


create_toolbar(Frame, BaseNum) ->
    PlayIconDC = wxMemoryDC:new(),
    PlayIcon = wxBitmap:new(30, 30, [{depth, 32}]),
    wxBufferedDC:selectObject(PlayIconDC, PlayIcon),
    wxMemoryDC:setBrush(PlayIconDC, ?wxGREEN_BRUSH),
    wxMemoryDC:setPen(PlayIconDC, ?wxGREEN_PEN),
    wxMemoryDC:drawPolygon(PlayIconDC, [{0, 0}, {30, 15}, {0, 30}]),
    wxMemoryDC:destroy(PlayIconDC),
    PlayIconDisabled = wxBitmap:new(wxImage:convertToGreyscale(wxBitmap:convertToImage(PlayIcon))),

    StopIconDC = wxMemoryDC:new(),
    StopIcon = wxBitmap:new(30, 30, [{depth, 32}]),
    wxBufferedDC:selectObject(StopIconDC, StopIcon),
    wxMemoryDC:setBrush(StopIconDC, ?wxRED_BRUSH),
    wxMemoryDC:setPen(StopIconDC, ?wxRED_PEN),
    wxMemoryDC:drawRectangle(StopIconDC, {0, 0}, {30, 30}),
    wxMemoryDC:destroy(StopIconDC),
    StopIconDisabled = wxBitmap:new(wxImage:convertToGreyscale(wxBitmap:convertToImage(StopIcon))),

    DelIconDC = wxMemoryDC:new(),
    DelIcon = wxBitmap:new(30, 30, [{depth, 32}]),
    wxBufferedDC:selectObject(DelIconDC, DelIcon),
    wxMemoryDC:setBrush(DelIconDC, ?wxBLUE_BRUSH),
    wxMemoryDC:setPen(DelIconDC, ?wxBLACK_PEN),
    wxMemoryDC:drawRectangle(DelIconDC, {3, 10}, {24, 20}),
    wxMemoryDC:drawRectangle(DelIconDC, {0, 5}, {30, 5}),
    wxMemoryDC:drawRectangle(DelIconDC, {10, 0}, {10, 5}),
    wxMemoryDC:setBrush(DelIconDC, ?wxWHITE_BRUSH),
    wxMemoryDC:setPen(DelIconDC, ?wxWHITE_PEN),
    wxMemoryDC:drawLine(DelIconDC, {9, 14}, {9, 26}),
    wxMemoryDC:drawLine(DelIconDC, {15, 14}, {15, 26}),
    wxMemoryDC:drawLine(DelIconDC, {21, 14}, {21, 26}),
    wxMemoryDC:destroy(DelIconDC),
    DelIconDisabled = wxBitmap:new(wxImage:convertToGreyscale(wxBitmap:convertToImage(DelIcon))),

    AddIconDC = wxMemoryDC:new(),
    AddIcon = wxBitmap:new(30, 30, [{depth, 32}]),
    wxBufferedDC:selectObject(AddIconDC, AddIcon),
    wxMemoryDC:setBrush(AddIconDC, ?wxGREEN_BRUSH),
    wxMemoryDC:setPen(AddIconDC, ?wxGREEN_PEN),
    wxMemoryDC:drawRectangle(AddIconDC, {0, 12}, {30, 6}),
    wxMemoryDC:drawRectangle(AddIconDC, {12, 0}, {6, 30}),
    wxMemoryDC:destroy(AddIconDC),
    AddIconDisabled = wxBitmap:new(wxImage:convertToGreyscale(wxBitmap:convertToImage(AddIcon))),

    Toolbar = wxFrame:createToolBar(Frame),
    wxToolBar:addTool(Toolbar, BaseNum + 0, "test123", PlayIcon, PlayIconDisabled),
    wxToolBar:enableTool(Toolbar, BaseNum + 0, false),
    wxToolBar:addTool(Toolbar, BaseNum + 1, "test123", StopIcon, StopIconDisabled),
    wxToolBar:enableTool(Toolbar, BaseNum + 1, false),
    wxToolBar:addTool(Toolbar, BaseNum + 2, "test123", DelIcon, DelIconDisabled),
    wxToolBar:enableTool(Toolbar, BaseNum + 2, false),
    wxToolBar:addTool(Toolbar, BaseNum + 3, "test123", AddIcon, AddIconDisabled),
    wxToolBar:enableTool(Toolbar, BaseNum + 3, true),
    wxToolBar:realize(Toolbar),
    Toolbar.


handle_info(Msg, State) ->
    ?LOG_INFO(#{info => Msg}),
    {noreply, State}.


handle_call(Msg, _From, State) ->
    ?LOG_INFO(#{call => Msg}),
    {reply, ok, State}.


%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxBookCtrl{nSel = Index}}, State) ->
    #state{
      toolbar = Toolbar,
      network_panel = NetworkPanel,
      domain_panel = DomainPanel,
      port_fwd_panel = PortFwdPanel
     } = State,
    case Index of
        0 -> wx_object:call(NetworkPanel, selected);
        1 -> wx_object:call(DomainPanel, selected);
        2 -> wx_object:call(PortFwdPanel, selected);
        _ -> wxToolBar:connect(Toolbar, command_menu_selected)
    end,
    {noreply, State#state{page = Index}};
handle_event(#wx{event = #wxClose{}}, #state{win = Frame} = State) ->
    io:format("~p Closing window ~n", [self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...", []),
    wxWindow:destroy(Frame),
    {stop, normal, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, #state{win = Frame} = State) ->
    ok = wxFrame:setStatusText(Frame, "Closing...", []),
    wxWindow:destroy(Frame),
    {stop, normal, State};
handle_event(Event, State) ->
    io:format("Unknown Event: ~p~n", [Event]),
    {noreply, State}.


code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.


terminate(_Reason, _State) ->
    ok.
