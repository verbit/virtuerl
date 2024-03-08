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
-module(virtuerl_ui).
-include_lib("wx/include/wx.hrl").

-export([start/0, start/1,
  init/1, handle_info/2, handle_event/2, handle_call/3,
  code_change/3, terminate/2]).

-behaviour(wx_object).

-record(state, {win, info_panel, info, domain_panel, domain_info, toolbar, domain_list_box, domains, page, net_list_box, node}).

start() ->
  start(node()).
start(Node) ->
  wx_object:start_link(?MODULE, [Node], []).

%% Init is called in the new process.
init([Node]) ->
%%  virtuerl_pubsub:subscribe(),

  wx:new(),
  Frame = wxFrame:new(wx:null(),
    -1, % window id
    "Hello World", % window title
    []),

  MenuBar = wxMenuBar:new(),
  Menu = wxMenu:new(),
  wxMenu:append(Menu, ?wxID_EXIT, "Quit"),
  wxMenuBar:append(MenuBar, Menu, "File"),
  wxFrame:setMenuBar(Frame, MenuBar),
  wxFrame:connect(Frame, command_menu_selected),

  {Mx, My, _, _} = wxMiniFrame:getTextExtent(Frame, "M"),
  wxFrame:setClientSize(Frame, {60*Mx, 20*My}),

  Toolbar = create_toolbar(Frame, 100),

  wxFrame:createStatusBar(Frame,[]),

  %% if we don't handle this ourselves, wxwidgets will close the window
  %% when the user clicks the frame's close button, but the event loop still runs
  wxFrame:connect(Frame, close_window),

  RootPanel = wxPanel:new(Frame, [{size, wxFrame:getSize(Frame)}]),
  Notebook = wxNotebook:new(RootPanel, ?wxID_ANY),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(Sizer, Notebook, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(RootPanel, Sizer),


  NetworkPanel = wxPanel:new(Notebook, []),
  NetworksSizer = wxBoxSizer:new(?wxHORIZONTAL),
  NetworkSplitter = wxSplitterWindow:new(NetworkPanel),
  wxSizer:add(NetworksSizer, NetworkSplitter, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(NetworkPanel, NetworksSizer),

  wxNotebook:addPage(Notebook, NetworkPanel, "Networks"),

  {ok, Nets} = erpc:call(Node, virtuerl_ipam, ipam_list_nets, []),
  Choices = maps:keys(Nets),
  ListBox = wxListBox:new(NetworkSplitter, 42, [{choices, Choices}]),
  wxListBox:connect(ListBox, command_listbox_selected), % command_listbox_doubleclicked

  Info = wxPanel:new(NetworkSplitter),
  InfoSizer = wxBoxSizer:new(?wxHORIZONTAL),
  InfoGrid = wxFlexGridSizer:new(3, 2, 0, 0),
  wxSizer:add(InfoSizer, InfoGrid, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(Info, InfoSizer),

  wxSplitterWindow:splitHorizontally(NetworkSplitter, ListBox, Info, [{sashPosition, 25 * Mx}]),

  % BEGIN Domains
  DomainPanel = wxPanel:new(Notebook, []),
  DomainsSizer = wxBoxSizer:new(?wxHORIZONTAL),
  DomainSplitter = wxSplitterWindow:new(DomainPanel),
  wxSizer:add(DomainsSizer, DomainSplitter, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(DomainPanel, DomainsSizer),

  wxNotebook:addPage(Notebook, DomainPanel, "Domains"),
  wxNotebook:connect(Notebook, command_notebook_page_changed),
  wxNotebook:setSelection(Notebook, 1),

  Domains = erpc:call(Node, virtuerl_mgt, domains_list, []),
  ColumnNames = ["ID", "Name", "IPs", "CPU", "RAM"],
  ColumnAlignment = [?wxLIST_FORMAT_LEFT, ?wxLIST_FORMAT_LEFT, ?wxLIST_FORMAT_LEFT, ?wxLIST_FORMAT_RIGHT, ?wxLIST_FORMAT_RIGHT],
  DomainsTuples = [{Id, Name, lists:join($,, [virtuerl_net:format_ip(Ip) || {Ip, _Prefixlen} <- Cidrs]), integer_to_binary(Vcpu), [integer_to_binary(Memory), $M]} || #{id := Id, name := Name, cidrs := Cidrs, vcpu := Vcpu, memory := Memory} <- Domains],
  DomainsIds = [Id || {Id, _Name, _, _, _} <- DomainsTuples],
  DomainListBox = wxListCtrl:new(DomainSplitter, [{style, ?wxLC_REPORT}]),
  lists:foreach(fun ({Idx, Name}) -> wxListCtrl:insertColumn(DomainListBox, Idx, Name, [{format, lists:nth(Idx + 1, ColumnAlignment)}]) end, lists:enumerate(0, ColumnNames)),
  lists:foreach(fun (Idx) ->
    Item = wxListItem:new(),
    wxListItem:setId(Item, Idx),
    wxListCtrl:insertItem(DomainListBox, Item)
                end, lists:seq(1, length(DomainsTuples))),
  lists:foreach(fun (ColIdx) ->
    lists:foreach(fun ({RowIdx, Dom}) ->
      wxListCtrl:setItem(DomainListBox, RowIdx, ColIdx - 1, element(ColIdx, Dom))
                  end, lists:enumerate(0, DomainsTuples))
                end, lists:seq(1, length(ColumnNames))),
  wxListCtrl:connect(DomainListBox, command_list_item_selected), % command_listbox_doubleclicked

  DomainInfo = wxPanel:new(DomainSplitter),
  DomainInfoSizer = wxBoxSizer:new(?wxVERTICAL),
%%  DomainInfoGrid = wxFlexGridSizer:new(3, 2, 0, 0),
  DomainInfoGrid = wxGridBagSizer:new(),
  wxSizer:add(DomainInfoSizer, DomainInfoGrid, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(DomainInfo, DomainInfoSizer),

  DomainButtonsSizer = wxBoxSizer:new(?wxHORIZONTAL),
  DomainDupBtn = wxButton:new(DomainInfo, 4044, [{label, "Duplicate"}]),

  wxButton:connect(DomainDupBtn, command_button_clicked),
  DomainDelBtn = wxButton:new(DomainInfo, ?wxID_ANY, [{label, "Delete"}]),
  wxSizer:add(DomainButtonsSizer, DomainDupBtn),
  wxSizer:add(DomainButtonsSizer, DomainDelBtn),
  wxSizer:add(DomainInfoSizer, DomainButtonsSizer, [{flag, ?wxALIGN_RIGHT}]),

  wxSplitterWindow:splitHorizontally(DomainSplitter, DomainListBox, DomainInfo, [{sashPosition, 25 * Mx}]),
  % END Domains

  ok = wxFrame:setStatusText(Frame, "Hello World!",[]),
  wxWindow:fit(Frame),
  wxWindow:show(Frame),
  wxWindow:raise(Frame),
  wxWindow:setFocus(Frame),
  wxWindow:layout(Frame),
  {Frame, #state{node = Node, toolbar = Toolbar, page = 1, win=Frame, net_list_box = ListBox, info_panel = Info, info=InfoGrid, domain_panel = DomainInfo, domain_info=DomainInfoGrid, domain_list_box=DomainListBox, domains = DomainsIds}}.

create_toolbar(Frame, BaseNum) ->
  PlayIconDC = wxMemoryDC:new(),
  PlayIcon = wxBitmap:new(30, 30, [{depth, 32}]),
  wxBufferedDC:selectObject(PlayIconDC, PlayIcon),
  wxMemoryDC:setBrush(PlayIconDC, ?wxGREEN_BRUSH),
  wxMemoryDC:setPen(PlayIconDC, ?wxGREEN_PEN),
  wxMemoryDC:drawPolygon(PlayIconDC, [{0,0},{30,15},{0,30}]),
  wxMemoryDC:destroy(PlayIconDC),
  PlayIconDisabled = wxBitmap:new(wxImage:convertToGreyscale(wxBitmap:convertToImage(PlayIcon))),

  StopIconDC = wxMemoryDC:new(),
  StopIcon = wxBitmap:new(30, 30, [{depth, 32}]),
  wxBufferedDC:selectObject(StopIconDC, StopIcon),
  wxMemoryDC:setBrush(StopIconDC, ?wxRED_BRUSH),
  wxMemoryDC:setPen(StopIconDC, ?wxRED_PEN),
  wxMemoryDC:drawRectangle(StopIconDC, {0,0},{30,30}),
  wxMemoryDC:destroy(StopIconDC),
  StopIconDisabled = wxBitmap:new(wxImage:convertToGreyscale(wxBitmap:convertToImage(StopIcon))),

  DelIconDC = wxMemoryDC:new(),
  DelIcon = wxBitmap:new(30, 30, [{depth, 32}]),
  wxBufferedDC:selectObject(DelIconDC, DelIcon),
  wxMemoryDC:setBrush(DelIconDC, ?wxBLUE_BRUSH),
  wxMemoryDC:setPen(DelIconDC, ?wxBLACK_PEN),
  wxMemoryDC:drawRectangle(DelIconDC, {3,10},{24,20}),
  wxMemoryDC:drawRectangle(DelIconDC, {0,5},{30,5}),
  wxMemoryDC:drawRectangle(DelIconDC, {10,0},{10,5}),
  wxMemoryDC:setBrush(DelIconDC, ?wxWHITE_BRUSH),
  wxMemoryDC:setPen(DelIconDC, ?wxWHITE_PEN),
  wxMemoryDC:drawLine(DelIconDC, {9,14}, {9,26}),
  wxMemoryDC:drawLine(DelIconDC, {15,14}, {15,26}),
  wxMemoryDC:drawLine(DelIconDC, {21,14}, {21,26}),
  wxMemoryDC:destroy(DelIconDC),
  DelIconDisabled = wxBitmap:new(wxImage:convertToGreyscale(wxBitmap:convertToImage(DelIcon))),

  AddIconDC = wxMemoryDC:new(),
  AddIcon = wxBitmap:new(30, 30, [{depth, 32}]),
  wxBufferedDC:selectObject(AddIconDC, AddIcon),
  wxMemoryDC:setBrush(AddIconDC, ?wxGREEN_BRUSH),
  wxMemoryDC:setPen(AddIconDC, ?wxGREEN_PEN),
  wxMemoryDC:drawRectangle(AddIconDC, {0,12},{30,6}),
  wxMemoryDC:drawRectangle(AddIconDC, {12,0},{6,30}),
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
  wxToolBar:connect(Toolbar, command_menu_selected),
  Toolbar.

%% Handled as in normal gen_server callbacks
handle_info({domain_out, _Id, Text}, #state{domain_panel = DomainPanel} = State) ->
  SerialOut = wx:typeCast(wxPanel:findWindow(DomainPanel, 69), wxStyledTextCtrl),
  io:put_chars(Text),
  wxStyledTextCtrl:appendText(SerialOut, Text),
  wxStyledTextCtrl:scrollToLine(SerialOut, wxStyledTextCtrl:getLineCount(SerialOut)),
  {noreply,State};
handle_info(Msg, #state{domain_panel = DomainPanel} = State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxBookCtrl{nSel = Index}}, State) ->
  io:format("TAB: ~p~n", [Index]),
  {noreply, State#state{page = Index}};
handle_event(#wx{id = 42, event = #wxCommand{type = command_listbox_selected,
  cmdString = Choice}},
    State = #state{info_panel = Panel, info=Info, domains = DomainIds, node = Node}) ->
  {ok, Nets} = erpc:call(Node, virtuerl_ipam, ipam_list_nets, []),
  Net = maps:get(list_to_binary(Choice), Nets),
  #{cidr4 := #{address := Address, prefixlen := Prefixlen}} = Net,
  wxFlexGridSizer:clear(Info, [{delete_windows, true}]),
  wxSizer:add(Info, wxStaticText:new(Panel, -1, "CIDR")),
  wxSizer:add(Info, wxStaticText:new(Panel, -1, iolist_to_binary([Address, "/", integer_to_binary(Prefixlen)]))),
  io:format("dblclick ~p (~p)~n", [Choice, Net]),
  {noreply, State};
handle_event(#wx{event = #wxList{type = command_list_item_selected,
  itemIndex = ItemIndex}},
    State = #state{domain_panel = DomainPanel, domain_info = DomainInfo, toolbar=Toolbar, domains = DomainIds, node = Node}) ->
  Domains = maps:from_list([{Id, Domain} || Domain = #{id := Id} <- erpc:call(Node, virtuerl_mgt, domains_list, [])]),
  wxGridBagSizer:clear(DomainInfo, [{delete_windows, true}]),

  DomainId = lists:nth(ItemIndex + 1, DomainIds),
  Domain = maps:get(DomainId, Domains),
  #{state := DomainState, network_id := NetworkId} = Domain,
  case DomainState of
    running ->
      wxToolBar:enableTool(Toolbar, 100, false),
      wxToolBar:enableTool(Toolbar, 101, true);
    stopped ->
      wxToolBar:enableTool(Toolbar, 100, true),
      wxToolBar:enableTool(Toolbar, 101, false)
  end,
  wxToolBar:enableTool(Toolbar, 102, true),
  wxGridBagSizer:add(DomainInfo, wxStaticText:new(DomainPanel, -1, "ID"), {0, 0}),
  wxGridBagSizer:add(DomainInfo, wxStaticText:new(DomainPanel, -1, DomainId), {0, 1}),
  wxGridBagSizer:add(DomainInfo, wxStaticText:new(DomainPanel, -1, "Network ID"), {1, 0}),
  wxGridBagSizer:add(DomainInfo, wxStaticText:new(DomainPanel, -1, NetworkId), {1, 1}),

  DomainWithoutUserData = maps:remove(user_data, Domain),
  wxGridBagSizer:add(DomainInfo, wxStaticText:new(DomainPanel, -1, io_lib:format("~p", [DomainWithoutUserData])), {2, 0}, [{span, {1, 2}}]),

%%  SerialOut = wxStyledTextCtrl:new(DomainPanel, [{id, 69}]),
%%  wxStyledTextCtrl:setLexer(SerialOut, ?wxSTC_LEX_ERRORLIST),
%%  wxStyledTextCtrl:styleSetVisible(SerialOut, 23, false),
%%  wxStyledTextCtrl:styleSetVisible(SerialOut, 24, false),
%%  wxStyledTextCtrl:setProperty(SerialOut, "lexer.errorlist.value.separate", "0"),
%%  wxStyledTextCtrl:setProperty(SerialOut, "lexer.errorlist.escape.sequences", "1"),
%%  wxGridBagSizer:add(DomainInfo, SerialOut, {3, 0}, [{span, {1, 2}}, {flag, ?wxEXPAND}]),
%%  WebView = wxWebView:new(DomainPanel, 999, [{url, "http://0.0.0.0:9000/noVNC-1.4.0/vnc.html?port=5700&?path=&resize=scale&autoconnect=true"}]),
%%  wxGridBagSizer:add(DomainInfo, WebView, {3, 0}, [{span, {1, 2}}, {flag, ?wxEXPAND}]),
  VncWindow = virtuerl_vnc:start(DomainPanel, DomainId, Node),
  wxGridBagSizer:add(DomainInfo, VncWindow, {0, 2}, [{span, {3, 1}}, {flag, ?wxEXPAND}]),

  wxGridBagSizer:addGrowableRow(DomainInfo, 2),

  UserData = maps:get(user_data, Domain, ""),
  UserDataCtrl = wxStyledTextCtrl:new(DomainPanel),
  wxStyledTextCtrl:setScrollWidth(UserDataCtrl, wxStyledTextCtrl:textWidth(UserDataCtrl, wxStyledTextCtrl:getStyleAt(UserDataCtrl, 0), UserData)),
  wxStyledTextCtrl:setText(UserDataCtrl, list_to_binary(UserData)),
  wxStyledTextCtrl:setReadOnly(UserDataCtrl, true),
  LastRowIndex = 3,
%%  Msg = wxNotificationMessage:new(lists:flatten(io_lib:format("LastRowIndex: ~p~n", [LastRowIndex]))),
%%  wxNotificationMessage:show(Msg),
  wxGridBagSizer:add(DomainInfo, UserDataCtrl, {LastRowIndex, 0}, [{span, {1, 3}}, {flag, ?wxEXPAND}]),
  wxGridBagSizer:addGrowableRow(DomainInfo, LastRowIndex),
  wxGridBagSizer:addGrowableCol(DomainInfo, 2),
  wxPanel:layout(DomainPanel),
  io:format("dblclick ~p (~p)~n", [DomainId, Domain]),
  {noreply, State};
handle_event(#wx{id = 4044, event = #wxCommand{type = command_button_clicked}}, #state{domain_list_box = DomainListBox, domains = DomainIds, node = Node} = State) ->
  SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
  DomainId = lists:nth(SelectedItem + 1, DomainIds),
  {ok, Domain} = erpc:call(Node, virtuerl_mgt, domain_get, [#{id => DomainId}]),
  create_domain_dialog(Node, Domain),
  {noreply, State};
handle_event(#wx{id = 100, obj = Toolbar, event = #wxCommand{type = command_menu_selected}} = Event, #state{page = 1, domain_list_box = DomainListBox, domains = DomainIds, node = Node} = State) ->
  io:format("~p~n", [Event]),
  SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
  DomainId = lists:nth(SelectedItem + 1, DomainIds),
  Choice = DomainId,
  io:format("~p~n", [Choice]),
  wxToolBar:enableTool(Toolbar, 100, false),
  wxToolBar:enableTool(Toolbar, 101, true),
  ok = erpc:call(Node, virtuerl_mgt, domain_start, [Choice]),
  {noreply, State};
handle_event(#wx{id = 101, obj = Toolbar, event = #wxCommand{type = command_menu_selected}} = Event, #state{page = 1, domain_list_box = DomainListBox, domains = DomainIds, node = Node} = State) ->
  io:format("~p~n", [Event]),
  SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
  DomainId = lists:nth(SelectedItem + 1, DomainIds),
  Choice = DomainId,
  io:format("~p~n", [Choice]),
  wxToolBar:enableTool(Toolbar, 100, true),
  wxToolBar:enableTool(Toolbar, 101, false),
  ok = erpc:call(Node, virtuerl_mgt, domain_stop, [Choice]),
  {noreply, State};
handle_event(#wx{id = 102, obj = Toolbar, event = #wxCommand{type = command_menu_selected}} = Event, #state{page = 1, domain_list_box = DomainListBox, domains = DomainIds, node = Node} = State) ->
  io:format("~p~n", [Event]),
  SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
  DomainId = lists:nth(SelectedItem + 1, DomainIds),
  Choice = DomainId,
  io:format("~p~n", [Choice]),
%%  wxToolBar:enableTool(Toolbar, 100, true),
%%  wxToolBar:enableTool(Toolbar, 101, false),
%%  ok = erpc:call(Node, virtuerl_mgt, domain_stop, (Choice),
  erpc:call(Node, virtuerl_mgt, domain_delete, [#{id => DomainId}]),
  {noreply, State};
handle_event(#wx{id = 103, obj = Toolbar, event = #wxCommand{type = command_menu_selected}} = Event, #state{page = 1, domain_list_box = DomainListBox, domains = DomainIds, node = Node} = State) ->
  create_domain_dialog(Node, #{network_id => "", name=> "", user_data => "", vcpu => 2, memory => 1024}),
  {noreply, State};
%% BEGIN: Network Toolbar
handle_event(#wx{id = 102, event = #wxCommand{type = command_menu_selected}}, #state{page = 0, net_list_box = ListBox, node = Node} = State) ->
  NetId = wxListBox:getStringSelection(ListBox),
  erpc:call(Node, virtuerl_ipam, ipam_delete_net, [NetId]),
  {noreply, State};
handle_event(#wx{id = 103, event = #wxCommand{type = command_menu_selected}}, #state{page = 0, node = Node} = State) ->
  create_network_dialog(Node),
  {noreply, State};
%% END: Network Toolbar
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
  io:format("~p Closing window ~n",[self()]),
  ok = wxFrame:setStatusText(Frame, "Closing...",[]),
  wxWindow:destroy(Frame),
  {stop, normal, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, State =  #state{win=Frame}) ->
  io:format("~p Quitting window ~n",[self()]),
  ok = wxFrame:setStatusText(Frame, "Closing...",[]),
  wxWindow:destroy(Frame),
  {stop, normal, State};
handle_event(Event, State) ->
  io:format("Unknown Event: ~p~n", [Event]),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  ok.

create_network_dialog(Node) ->
  Dialog = wxDialog:new(wx:null(), ?wxID_ANY, "Create Network", [{size, {1000, 500}}]),
  DialogSizer = wxBoxSizer:new(?wxVERTICAL),
  DialogGridSizer = wxFlexGridSizer:new(1, 2, 0, 0),
  wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "CIDR")),
  CidrCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY),
  wxSizer:add(DialogGridSizer, CidrCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(DialogSizer, DialogGridSizer),
  ButtonSizer = wxDialog:createStdDialogButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
  wxSizer:add(DialogSizer, ButtonSizer),
  wxPanel:setSizer(Dialog, DialogSizer),
%%  wxWindow:layout(DialogSizer),

  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      NetDef = virtuerl_net:parse_cidr(wxTextCtrl:getValue(CidrCtrl)),
      {ok, NetId} = erpc:call(Node, virtuerl_ipam, ipam_create_net, [[NetDef]]);
    _ -> ok
  end,
  wxDialog:destroy(Dialog).

create_domain_dialog(Node, #{network_id := NetworkId, name:= DomainName, user_data := UserData, vcpu := Vcpu, memory := Memory} = Domain) ->
  {ok, Nets} = erpc:call(Node, virtuerl_ipam, ipam_list_nets, []),
  Choices = maps:keys(Nets),
  Dialog = wxDialog:new(wx:null(), ?wxID_ANY, "Create Domain", [{size, {1000, 500}}]),
  DialogSizer = wxBoxSizer:new(?wxVERTICAL),
  DialogGridSizer = wxFlexGridSizer:new(1, 2, 0, 0),
  wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Name")),
  NameCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY, [{value, DomainName}]),
  wxSizer:add(DialogGridSizer, NameCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Network")),
  NetworkChoice = wxChoice:new(Dialog, ?wxID_ANY, [{choices, Choices}]),
  wxChoice:setStringSelection(NetworkChoice, NetworkId),
  wxSizer:add(DialogGridSizer, NetworkChoice),
  wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "CPU")),
  VcpuCtrl = wxSpinCtrl:new(Dialog, [{min, 1}, {max, 256}, {initial, Vcpu}]),
  wxSizer:add(DialogGridSizer, VcpuCtrl),
  wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Memory")),
  MemoryCtrl = wxSpinCtrl:new(Dialog, [{min, 128}, {max, 131072}, {initial, Memory}]),
  wxSizer:add(DialogGridSizer, MemoryCtrl),
  wxSizer:add(DialogSizer, DialogGridSizer),
  UserDataCtrl = wxStyledTextCtrl:new(Dialog),
  wxStyledTextCtrl:setLexer(UserDataCtrl, ?wxSTC_LEX_YAML),
  wxStyledTextCtrl:setText(UserDataCtrl, UserData),
  wxSizer:add(DialogSizer, UserDataCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
  ButtonSizer = wxDialog:createStdDialogButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
  wxSizer:add(DialogSizer, ButtonSizer),
  wxPanel:setSizer(Dialog, DialogSizer),

  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      io:format("true ~p~n", [wxChoice:getStringSelection(NetworkChoice)]),
      erpc:call(Node, virtuerl_mgt, domain_create, [#{
        name => wxTextCtrl:getValue(NameCtrl),
        network_id => list_to_binary(wxChoice:getStringSelection(NetworkChoice)),
        user_data => wxStyledTextCtrl:getText(UserDataCtrl),
        vcpu => wxSpinCtrl:getValue(VcpuCtrl),
        memory => wxSpinCtrl:getValue(MemoryCtrl)
      }]);
    _ -> ok
  end,
  wxDialog:destroy(Dialog);
create_domain_dialog(Node, Domain) ->
  create_domain_dialog(Node, Domain#{user_data => ""}).
