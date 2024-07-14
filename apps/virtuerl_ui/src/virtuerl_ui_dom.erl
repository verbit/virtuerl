-module(virtuerl_ui_dom).

-behaviour(wx_object).

-export([start_link/3]).
-export([init/1, handle_event/2, handle_info/2, handle_call/3, terminate/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {node, toolbar, domain_panel, domain_info, domain_list_box}).


start_link(Parent, Toolbar, Node) ->
    wx_object:start_link(?MODULE, [Node, Parent, Toolbar], []).


init([Node, Parent, Toolbar]) ->
    {_Mx, My, _, _} = wxFrame:getTextExtent(Parent, "M"),

    DomainPanel = wxPanel:new(Parent, []),
    DomainsSizer = wxBoxSizer:new(?wxHORIZONTAL),
    DomainSplitter = wxSplitterWindow:new(DomainPanel),
    wxSizer:add(DomainsSizer, DomainSplitter, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(DomainPanel, DomainsSizer),

    DomainListBox = wxListCtrl:new(DomainSplitter, [{style, ?wxLC_REPORT}]),
    ColumnNames = ["ID", "Name", "Node", "CPU", "RAM", "IPs"],
    ColumnWidths = [ case P of
                         Str when is_list(Str) ->
                             {Width, _, _, _} = wxListCtrl:getTextExtent(DomainListBox, P),
                             Width;
                         Other -> Other
                     end
                     || P <- ["mmmmmm", "virtual-machine", "host-machine.example.com", ?wxLIST_AUTOSIZE_USEHEADER,
                              "mmmmmm M", "444.444.444.444,mmmm:mmmm:mmmm:mmmm"] ],
    ColumnAlignment = [?wxLIST_FORMAT_LEFT, ?wxLIST_FORMAT_LEFT, ?wxLIST_FORMAT_LEFT, ?wxLIST_FORMAT_RIGHT,
                       ?wxLIST_FORMAT_RIGHT, ?wxLIST_FORMAT_LEFT],
    lists:foreach(
      fun({Idx, Name}) ->
              wxListCtrl:insertColumn(DomainListBox,
                                      Idx,
                                      Name,
                                      [{format, lists:nth(Idx + 1, ColumnAlignment)},
                                       {width, lists:nth(Idx + 1, ColumnWidths)}])
      end,
      lists:enumerate(0, ColumnNames)),
    wxListCtrl:connect(DomainListBox, command_list_item_selected),  % command_listbox_doubleclicked
    update_domains(Node, DomainListBox),

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
    DomainEditBtn = wxButton:new(DomainInfo, 4046, [{label, "Edit"}]),
    wxButton:connect(DomainEditBtn, command_button_clicked),
    wxSizer:add(DomainButtonsSizer, DomainDupBtn),
    wxSizer:add(DomainButtonsSizer, DomainDelBtn),
    wxSizer:add(DomainButtonsSizer, DomainEditBtn),
    wxSizer:add(DomainInfoSizer, DomainButtonsSizer, [{flag, ?wxALIGN_RIGHT}]),

    wxSplitterWindow:splitHorizontally(DomainSplitter, DomainListBox, DomainInfo, [{sashPosition, 7 * My}]),

    {DomainPanel,
     #state{
       node = Node,
       toolbar = Toolbar,
       domain_panel = DomainInfo,
       domain_info = DomainInfoGrid,
       domain_list_box = DomainListBox
      }}.


handle_event(#wx{event = #wxList{type = command_list_item_selected, itemIndex = ItemIndex}}, State) ->
    #state{domain_panel = DomainPanel, domain_info = DomainInfo, toolbar = Toolbar, domain_list_box = DomainListBox, node = Node} = State,
    wxGridBagSizer:clear(DomainInfo, [{delete_windows, true}]),  % TODO: this breaks VNC module

    DomainId = iolist_to_binary(wxListCtrl:getItemText(DomainListBox, ItemIndex, [{col, 0}])),
    {ok, Domain} = erpc:call(Node, virtuerl_mgt, domain_get, [#{id => DomainId}]),
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
    wxGridBagSizer:add(DomainInfo,
                       wxStaticText:new(DomainPanel, -1, io_lib:format("~p", [DomainWithoutUserData])),
                       {2, 0},
                       [{span, {1, 2}}]),

    %%  SerialOut = wxStyledTextCtrl:new(DomainPanel, [{id, 69}]),
    %%  wxStyledTextCtrl:setLexer(SerialOut, ?wxSTC_LEX_ERRORLIST),
    %%  wxStyledTextCtrl:styleSetVisible(SerialOut, 23, false),
    %%  wxStyledTextCtrl:styleSetVisible(SerialOut, 24, false),
    %%  wxStyledTextCtrl:setProperty(SerialOut, "lexer.errorlist.value.separate", "0"),
    %%  wxStyledTextCtrl:setProperty(SerialOut, "lexer.errorlist.escape.sequences", "1"),
    %%  wxGridBagSizer:add(DomainInfo, SerialOut, {3, 0}, [{span, {1, 2}}, {flag, ?wxEXPAND}]),
    %%  WebView = wxWebView:new(DomainPanel, 999, [{url,
    %%  "http://0.0.0.0:9000/noVNC-1.4.0/vnc.html?port=5700&?path=&resize=scale&autoconnect=true"}]),
    %%  wxGridBagSizer:add(DomainInfo, WebView, {3, 0}, [{span, {1, 2}}, {flag, ?wxEXPAND}]),
    VncWindow = virtuerl_vnc:start(DomainPanel, DomainId, Node),
    wxGridBagSizer:add(DomainInfo, VncWindow, {0, 2}, [{span, {3, 1}}, {flag, ?wxEXPAND}]),

    wxGridBagSizer:addGrowableRow(DomainInfo, 2),

    UserData = maps:get(user_data, Domain, ""),
    UserDataCtrl = wxStyledTextCtrl:new(DomainPanel),
    wxStyledTextCtrl:setScrollWidth(
      UserDataCtrl,
      wxStyledTextCtrl:textWidth(UserDataCtrl, wxStyledTextCtrl:getStyleAt(UserDataCtrl, 0), UserData)),
    wxStyledTextCtrl:setText(UserDataCtrl, list_to_binary(UserData)),
    wxStyledTextCtrl:setReadOnly(UserDataCtrl, true),
    LastRowIndex = 3,
    %%  Msg = wxNotificationMessage:new(lists:flatten(io_lib:format("LastRowIndex: ~p~n", [LastRowIndex]))),
    %%  wxNotificationMessage:show(Msg),
    wxGridBagSizer:add(DomainInfo, UserDataCtrl, {LastRowIndex, 0}, [{span, {1, 3}}, {flag, ?wxEXPAND}]),
    wxGridBagSizer:addGrowableRow(DomainInfo, LastRowIndex),
    wxGridBagSizer:addGrowableCol(DomainInfo, 2),
    wxPanel:layout(DomainPanel),
    {noreply, State};
handle_event(#wx{id = 4044, event = #wxCommand{type = command_button_clicked}}, State) ->
    #state{domain_list_box = DomainListBox, node = Node} = State,
    SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
    DomainId = iolist_to_binary(wxListCtrl:getItemText(DomainListBox, SelectedItem, [{col, 0}])),
    {ok, Domain} = erpc:call(Node, virtuerl_mgt, domain_get, [#{id => DomainId}]),
    create_domain_dialog(Node, Domain, create),
    {noreply, State};
handle_event(#wx{id = 4046, event = #wxCommand{type = command_button_clicked}}, State) ->
    #state{domain_list_box = DomainListBox, node = Node} = State,
    SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
    DomainId = iolist_to_binary(wxListCtrl:getItemText(DomainListBox, SelectedItem, [{col, 0}])),
    {ok, Domain} = erpc:call(Node, virtuerl_mgt, domain_get, [#{id => DomainId}]),
    create_domain_dialog(Node, Domain, update),
    {noreply, State};
handle_event(#wx{id = 100, obj = Toolbar, event = #wxCommand{type = command_menu_selected}} = _Event, State) ->
    #state{domain_list_box = DomainListBox, node = Node} = State,
    SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
    DomainId = iolist_to_binary(wxListCtrl:getItemText(DomainListBox, SelectedItem, [{col, 0}])),
    Choice = DomainId,
    wxToolBar:enableTool(Toolbar, 100, false),
    wxToolBar:enableTool(Toolbar, 101, true),
    ok = erpc:call(Node, virtuerl_mgt, domain_start, [Choice]),
    {noreply, State};
handle_event(#wx{id = 101, obj = Toolbar, event = #wxCommand{type = command_menu_selected}} = _Event, State) ->
    #state{domain_list_box = DomainListBox, node = Node} = State,
    SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
    DomainId = iolist_to_binary(wxListCtrl:getItemText(DomainListBox, SelectedItem, [{col, 0}])),
    Choice = DomainId,
    wxToolBar:enableTool(Toolbar, 100, true),
    wxToolBar:enableTool(Toolbar, 101, false),
    ok = erpc:call(Node, virtuerl_mgt, domain_stop, [Choice]),
    {noreply, State};
handle_event(#wx{id = 102, obj = _Toolbar, event = #wxCommand{type = command_menu_selected}} = _Event, State) ->
    #state{domain_list_box = DomainListBox, node = Node} = State,
    SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
    DomainId = iolist_to_binary(wxListCtrl:getItemText(DomainListBox, SelectedItem, [{col, 0}])),
    erpc:call(Node, virtuerl_mgt, domain_delete, [#{id => DomainId}]),
    {noreply, State};
handle_event(#wx{id = 103, obj = _Toolbar, event = #wxCommand{type = command_menu_selected}} = _Event, State) ->
    #state{domain_list_box = _DomainListBox, node = Node} = State,
    Domain = #{
               network_id => "",
               name => "",
               user_data => "",
               vcpu => 2,
               memory => 1024,
               inbound_rules => [],
               os_type => "linux"
              },
    create_domain_dialog(Node, Domain, create),
    {noreply, State};
handle_event(Request, State) ->
    ?LOG_INFO(#{who => ?MODULE, what => handle_event, request => Request}),
    {noreply, State}.


handle_call(Request, _From, #state{toolbar = Toolbar} = State) ->
    ?LOG_INFO(#{who => ?MODULE, what => handle_call, request => Request}),
    wxToolBar:connect(Toolbar, command_menu_selected),
    {reply, ok, State}.


handle_info({domain_out, _Id, Text}, #state{domain_panel = DomainPanel} = State) ->
    SerialOut = wx:typeCast(wxPanel:findWindow(DomainPanel, 69), wxStyledTextCtrl),
    io:put_chars(Text),
    wxStyledTextCtrl:appendText(SerialOut, Text),
    wxStyledTextCtrl:scrollToLine(SerialOut, wxStyledTextCtrl:getLineCount(SerialOut)),
    {noreply, State};
handle_info({domain_created, _Id}, #state{node = Node, domain_list_box = DomainListBox} = State) ->
    update_domains(Node, DomainListBox),
    {noreply, State};
handle_info({domain_deleted, _Id}, #state{node = Node, domain_list_box = DomainListBox} = State) ->
    update_domains(Node, DomainListBox),
    {noreply, State};
handle_info(Msg, State) ->
    ?LOG_INFO(#{info => Msg}),
    {noreply, State}.


update_domains(Node, DomainListBox) ->
    Domains = erpc:call(Node, virtuerl_mgt, domains_list, []),
    DomainsTuples = [ {Id,
                       Name,
                       case HostNode of Atom when is_atom(Atom) -> atom_to_list(Atom); Str -> Str end,
                       integer_to_binary(Vcpu),
                       [integer_to_binary(Memory), " M"],
                       lists:join($,, [ virtuerl_net:format_ip(Ip) || {Ip, _Prefixlen} <- Cidrs ])}
                      || #{id := Id, name := Name, node := HostNode, cidrs := Cidrs, vcpu := Vcpu, memory := Memory} <- Domains ],

    SelDomIdRes = selected_domain_id(DomainListBox),
    true = wxListCtrl:deleteAllItems(DomainListBox),
    wx:foreach(
      fun(Idx) ->
              Item = wxListItem:new(),
              wxListItem:setId(Item, Idx),
              wxListCtrl:insertItem(DomainListBox, Item)
      end,
      lists:seq(1, length(DomainsTuples))),
    wx:foreach(
      fun({RowIdx, Dom}) ->
              wx:foreach(
                fun(ColIdx) ->
                        wxListCtrl:setItem(DomainListBox, RowIdx, ColIdx - 1, element(ColIdx, Dom))
                end,
                lists:seq(1, 5)),
              {DomId, _, _, _, _, _} = Dom,
              case SelDomIdRes of
                  {_Idx, DomId} ->
                      wxListCtrl:setItemState(DomainListBox,
                                              RowIdx,
                                              ?wxLIST_STATE_FOCUSED bor ?wxLIST_STATE_SELECTED,
                                              ?wxLIST_STATE_FOCUSED bor ?wxLIST_STATE_SELECTED);
                  _ -> ok
              end
      end,
      lists:enumerate(0, DomainsTuples)).


-spec selected_domain_id(wxListCtrl:wxListCtrl()) -> {integer(), binary()} | none.
selected_domain_id(DomainListBox) ->
    SelectedItem = wxListCtrl:getNextItem(DomainListBox, -1, [{state, ?wxLIST_STATE_SELECTED}]),
    % _DomainId = lists:nth(SelectedItem + 1, DomainIds),
    case SelectedItem of
        -1 -> none;
        Num -> {Num, iolist_to_binary(wxListCtrl:getItemText(DomainListBox, SelectedItem, [{col, 0}]))}
    end.


terminate(_Reason, _State) ->
    ok.


create_domain_dialog(Node, Domain, Op) ->
    Title = case Op of
                create -> "Create Domain";
                update -> "Update Domain"
            end,

    #{
      network_id := NetworkId,
      name := DomainName,
      user_data := UserData,
      vcpu := Vcpu,
      memory := Memory,
      inbound_rules := InboundRules
     } = Domain,
    {ok, Nets} = erpc:call(Node, virtuerl_ipam, ipam_list_nets, []),
    NetworkChoices = maps:keys(Nets),
    ImageChoices = erpc:call(Node, virtuerl_img, list_images, []),
    Dialog = wxDialog:new(wx:null(), ?wxID_ANY, Title, [{size, {1000, 500}}]),
    DialogSizer = wxBoxSizer:new(?wxVERTICAL),
    DialogGridSizer = wxFlexGridSizer:new(1, 2, 0, 0),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Host")),
    HostChoice = wxChoice:new(Dialog, ?wxID_ANY, [{choices, ["localhost"]}]),
    wxChoice:setStringSelection(HostChoice, "localhost"),
    wxSizer:add(DialogGridSizer, HostChoice),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Name")),
    NameCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY, [{value, DomainName}]),
    wxSizer:add(DialogGridSizer, NameCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Network")),
    NetworkChoice = wxChoice:new(Dialog, ?wxID_ANY, [{choices, NetworkChoices}]),
    wxChoice:setStringSelection(NetworkChoice, NetworkId),
    wxSizer:add(DialogGridSizer, NetworkChoice),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "CPU")),
    VcpuCtrl = wxSpinCtrl:new(Dialog, [{min, 1}, {max, 256}, {initial, Vcpu}]),
    wxSizer:add(DialogGridSizer, VcpuCtrl),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Memory")),
    MemoryCtrl = wxSpinCtrl:new(Dialog, [{min, 128}, {max, 131072}, {initial, Memory}]),
    wxSizer:add(DialogGridSizer, MemoryCtrl),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "OS")),
    OsChoice = wxChoice:new(Dialog, ?wxID_ANY, [{choices, ["linux", "windows"]}]),
    wxChoice:setStringSelection(OsChoice, "linux"),
    wxSizer:add(DialogGridSizer, OsChoice),
    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Image")),
    ImageChoice = wxChoice:new(Dialog, ?wxID_ANY, [{choices, ImageChoices}]),
    [DefaultImage | _] = ImageChoices,
    wxChoice:setStringSelection(ImageChoice, DefaultImage),
    wxSizer:add(DialogGridSizer, ImageChoice),

    wxSizer:add(DialogSizer, DialogGridSizer),

    % Inbound Rules
    % wxBoxSizer:new(?wxHORIZONTAL),
    InboundRulesInitial = string:join(
                            [ string:join([string:join(Protos, ","),
                                           string:join([ case Port of
                                                             Num when is_integer(Num) ->
                                                                 integer_to_list(Num);
                                                             Str when is_list(Str) orelse is_binary(Str) ->
                                                                 Str
                                                         end || Port <- Ports ],
                                                       ","),
                                           string:join(Sources, ",")],
                                          ";")
                              || #{protocols := Protos, target_ports := Ports, sources := Sources} <- InboundRules ],
                            "\n"),
    InboundRulesCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY, [{style, ?wxTE_MULTILINE}, {value, InboundRulesInitial}]),
    wxSizer:add(DialogSizer, InboundRulesCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    UserDataCtrl = wxStyledTextCtrl:new(Dialog),
    wxStyledTextCtrl:setLexer(UserDataCtrl, ?wxSTC_LEX_YAML),
    wxStyledTextCtrl:setText(UserDataCtrl, UserData),
    wxSizer:add(DialogSizer, UserDataCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
    ButtonSizer = wxDialog:createStdDialogButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(DialogSizer, ButtonSizer),
    wxPanel:setSizer(Dialog, DialogSizer),

    case Op of
        create ->
            ok;
        update ->
            wxChoice:disable(HostChoice),
            wxTextCtrl:disable(NameCtrl),
            wxChoice:disable(NetworkChoice),
            wxSpinCtrl:disable(VcpuCtrl),
            wxSpinCtrl:disable(MemoryCtrl),
            wxChoice:disable(OsChoice),
            wxChoice:disable(ImageChoice),
            wxStyledTextCtrl:disable(UserDataCtrl)
    end,

    case wxDialog:showModal(Dialog) of
        ?wxID_OK ->
            InboundRulesVal = wxTextCtrl:getValue(InboundRulesCtrl),
            Rules0 = [ string:trim(Rule) || Rule <- string:split(InboundRulesVal, "\n", all) ],
            Rules1 = [ Rule || Rule <- Rules0, not string:is_empty(Rule) ],
            Rules2 = [ [ string:trim(Elem) || Elem <- string:split(Rule, ";", all) ] || Rule <- Rules1 ],
            Rules3 = [ case Rule of
                           [Protos, Ports, Sources] ->
                               #{
                                 protocols => nonempty_splitrim(Protos, ","),
                                 target_ports => nonempty_splitrim(Ports, ","),
                                 sources => nonempty_splitrim(Sources, ",")
                                }
                       end || Rule <- Rules2 ],
            case Op of
                create ->
                    erpc:call(Node,
                              virtuerl_mgt,
                              domain_create,
                              [#{
                                 host => list_to_atom(wxChoice:getStringSelection(HostChoice)),
                                 name => wxTextCtrl:getValue(NameCtrl),
                                 os_type => wxChoice:getStringSelection(OsChoice),
                                 base_image => wxChoice:getStringSelection(ImageChoice),
                                 network_id => list_to_binary(wxChoice:getStringSelection(NetworkChoice)),
                                 user_data => wxStyledTextCtrl:getText(UserDataCtrl),
                                 vcpu => wxSpinCtrl:getValue(VcpuCtrl),
                                 memory => wxSpinCtrl:getValue(MemoryCtrl),
                                 inbound_rules => Rules3
                                }]);
                update ->
                    #{id := DomainId, state := RunState} = Domain,
                    erpc:call(Node,
                              virtuerl_mgt,
                              domain_update,
                              [#{
                                 id => DomainId,
                                 state => RunState,
                                 inbound_rules => Rules3
                                }])
            end;
        _ -> ok
    end,
    wxDialog:destroy(Dialog).


nonempty_splitrim(Str, Delim) ->
    Res = [ string:trim(Elem) || Elem <- string:split(Str, Delim, all) ],
    [ Elem || Elem <- Res, not string:is_empty(Elem) ].
