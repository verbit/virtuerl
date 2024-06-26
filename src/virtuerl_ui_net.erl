-module(virtuerl_ui_net).

-behaviour(wx_object).

-export([start_link/3]).
-export([init/1, handle_event/2, handle_info/2, handle_call/3, terminate/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {node, toolbar, info_panel, info, net_list_box}).


start_link(Parent, Toolbar, Node) ->
    wx_object:start_link(?MODULE, [Node, Parent, Toolbar], []).


init([Node, Parent, Toolbar]) ->
    {Mx, _My, _, _} = wxFrame:getTextExtent(Parent, "M"),

    NetworkPanel = wxPanel:new(Parent, []),
    NetworksSizer = wxBoxSizer:new(?wxHORIZONTAL),
    NetworkSplitter = wxSplitterWindow:new(NetworkPanel),
    wxSizer:add(NetworksSizer, NetworkSplitter, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(NetworkPanel, NetworksSizer),

    {ok, Nets} = erpc:call(Node, virtuerl_ipam, ipam_list_nets, []),
    Choices = maps:keys(Nets),
    ListBox = wxListBox:new(NetworkSplitter, 42, [{choices, Choices}]),
    wxListBox:connect(ListBox, command_listbox_selected),  % command_listbox_doubleclicked

    Info = wxPanel:new(NetworkSplitter),
    InfoSizer = wxBoxSizer:new(?wxHORIZONTAL),
    InfoGrid = wxFlexGridSizer:new(3, 2, 0, 0),
    wxSizer:add(InfoSizer, InfoGrid, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(Info, InfoSizer),

    wxSplitterWindow:splitHorizontally(NetworkSplitter, ListBox, Info, [{sashPosition, 25 * Mx}]),

    {NetworkPanel,
     #state{
       node = Node,
       toolbar = Toolbar,
       info_panel = Info,
       info = InfoGrid,
       net_list_box = ListBox
      }}.


handle_event(#wx{id = 42, event = #wxCommand{type = command_listbox_selected, cmdString = Choice}}, State) ->
    #state{info_panel = Panel, info = Info, node = Node} = State,
    {ok, Nets} = erpc:call(Node, virtuerl_ipam, ipam_list_nets, []),
    Net = maps:get(list_to_binary(Choice), Nets),
    Cidrs = case Net of
                #{cidr4 := Cidr4, cidr6 := Cidr6} ->
                    [Cidr4, Cidr6];
                #{cidr4 := Cidr4} ->
                    [Cidr4];
                #{cidr6 := Cidr6} ->
                    [Cidr6]
            end,
    CidrsStr = [ binary_to_list(iolist_to_binary([IpAddr, "/", integer_to_binary(Prefixlen)]))
                 || #{address := IpAddr, prefixlen := Prefixlen} <- Cidrs ],
    wxFlexGridSizer:clear(Info, [{delete_windows, true}]),
    wxSizer:add(Info, wxStaticText:new(Panel, -1, "CIDR")),
    wxSizer:add(Info, wxStaticText:new(Panel, -1, string:join(CidrsStr, ","))),
    {noreply, State};
%% BEGIN: Network Toolbar
handle_event(#wx{id = 102, event = #wxCommand{type = command_menu_selected}}, State) ->
    #state{net_list_box = ListBox, node = Node} = State,
    NetId = wxListBox:getStringSelection(ListBox),
    erpc:call(Node, virtuerl_ipam, ipam_delete_net, [NetId]),
    {noreply, State};
handle_event(#wx{id = 103, event = #wxCommand{type = command_menu_selected}}, State) ->
    #state{node = Node} = State,
    create_network_dialog(Node),
    {noreply, State};
handle_event(Request, State) ->
    ?LOG_INFO(#{who => ?MODULE, what => handle_event, request => Request}),
    {noreply, State}.


handle_call(Request, _From, #state{toolbar = Toolbar} = State) ->
    ?LOG_INFO(#{who => ?MODULE, what => handle_call, request => Request}),
    wxToolBar:connect(Toolbar, command_menu_selected),
    {reply, ok, State}.


handle_info(Msg, State) ->
    ?LOG_INFO(#{info => Msg}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


create_network_dialog(Node) ->
    Dialog = wxDialog:new(wx:null(), ?wxID_ANY, "Create Network", [{size, {1000, 500}}]),
    DialogSizer = wxBoxSizer:new(?wxVERTICAL),
    % DialogGridSizer = wxFlexGridSizer:new(1, 2, 0, 0),
    DialogGridSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "CIDR")),
    CidrCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY, [{style, ?wxTE_MULTILINE}]),
    wxSizer:add(DialogGridSizer, CidrCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(DialogSizer, DialogGridSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
    ButtonSizer = wxDialog:createStdDialogButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(DialogSizer, ButtonSizer),
    wxPanel:setSizer(Dialog, DialogSizer),

    case wxDialog:showModal(Dialog) of
        ?wxID_OK ->
            NetDef = [ virtuerl_net:parse_cidr(string:trim(IpStr, both))
                       || IpStr <- string:split(wxTextCtrl:getValue(CidrCtrl), ",", all) ],
            {ok, _NetId} = erpc:call(Node, virtuerl_ipam, ipam_create_net, [NetDef]);
        _ -> ok
    end,
    wxDialog:destroy(Dialog).
