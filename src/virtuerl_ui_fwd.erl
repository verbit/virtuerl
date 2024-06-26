-module(virtuerl_ui_fwd).

-behaviour(wx_object).

-export([start_link/3]).
-export([init/1, handle_event/2, handle_info/2, handle_call/3, terminate/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {node, toolbar, list_box}).


start_link(Parent, Toolbar, Node) ->
    wx_object:start_link(?MODULE, [Node, Parent, Toolbar], []).


init([Node, Parent, Toolbar]) ->
    PortFwdPanel = wxPanel:new(Parent, []),
    PortFwdsSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(PortFwdPanel, PortFwdsSizer),
    PortFwdListBox = wxListBox:new(PortFwdPanel, ?wxID_ANY),
    % wxListBox:appendStrings(PortFwdListBox, ["test", "abc -> def"]),
    update_forwardings(Node, PortFwdListBox),
    wxSizer:add(PortFwdsSizer, PortFwdListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    {PortFwdPanel, #state{node = Node, toolbar = Toolbar, list_box = PortFwdListBox}}.


update_forwardings(Node, ListBox) ->
    % Fwds = erpc:call(Node, virtuerl_mgt, port_fwds_list, []),
    Domains = erpc:call(Node, virtuerl_mgt, domains_list, []),
    Fwds = lists:flatten([ [ Fwd#{target => Id} || Fwd <- Fwds ]
                           || #{id := Id, port_fwds := Fwds} <- Domains ]),
    Elems = [ io_lib:format(":~p ~p -> ~s:~p", [SourcePort, Protos, Target, TargetPort])
              || #{protos := Protos, source_port := SourcePort, target_port := TargetPort, target := Target} <- Fwds ],
    wxListBox:clear(ListBox),
    wxListBox:appendStrings(ListBox, Elems).


handle_event(#wx{id = 103, obj = _Toolbar, event = #wxCommand{type = command_menu_selected}} = _Event, State) ->
    #state{node = Node, list_box = ListBox} = State,

    Dialog = wxDialog:new(wx:null(), ?wxID_ANY, "Create Port Forwarding", [{size, {1000, 500}}]),
    DialogSizer = wxBoxSizer:new(?wxVERTICAL),
    DialogGridSizer = wxFlexGridSizer:new(1, 2, 0, 0),

    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "From")),
    PortFromCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY),
    wxSizer:add(DialogGridSizer, PortFromCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "To")),
    PortToCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY),
    wxSizer:add(DialogGridSizer, PortToCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Target")),
    Domains = erpc:call(Node, virtuerl_mgt, domains_list, []),
    TargetChoices = [ io_lib:format("~s / ~s", [Name, Id]) || #{id := Id, name := Name} <- Domains ],
    TargetChoice = wxChoice:new(Dialog, ?wxID_ANY, [{choices, TargetChoices}]),
    % wxChoice:setStringSelection(TargetChoice, Id),
    [ wxChoice:setClientData(TargetChoice, Idx - 1, Dom) || {Idx, Dom} <- lists:enumerate(Domains) ],
    wxSizer:add(DialogGridSizer, TargetChoice, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSizer:add(DialogGridSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Protocols")),
    ProtosCtrl = wxCheckListBox:new(Dialog, ?wxID_ANY, [{choices, ["tcp", "udp"]}]),
    wxSizer:add(DialogGridSizer, ProtosCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSizer:add(DialogSizer, DialogGridSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
    ButtonSizer = wxDialog:createStdDialogButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(DialogSizer, ButtonSizer),
    wxPanel:setSizer(Dialog, DialogSizer),

    case wxDialog:showModal(Dialog) of
        ?wxID_OK ->
            SelectedDom = wxChoice:getClientData(TargetChoice, wxChoice:getSelection(TargetChoice)),
            #{id := SelDomId} = SelectedDom,

            Checked = [ list_to_atom(wxCheckListBox:getString(ProtosCtrl, Idx - 1))
                        || Idx <- lists:seq(1, wxCheckListBox:getCount(ProtosCtrl)),
                           wxCheckListBox:isChecked(ProtosCtrl, Idx - 1) ],

            SourcePort = list_to_integer(wxTextCtrl:getValue(PortFromCtrl)),
            TargetPort = list_to_integer(wxTextCtrl:getValue(PortToCtrl)),

            Payload = #{source_port => SourcePort, target_port => TargetPort, protos => Checked},
            ?LOG_INFO(#{
                        who => ?MODULE,
                        what => "new port forwarding",
                        domain_id => SelDomId,
                        payload => Payload
                       }),

            erpc:call(Node,
                      virtuerl_mgt,
                      add_port_fwd,
                      [SelDomId, Payload]);
        _ -> ok
    end,
    wxDialog:destroy(Dialog),
    update_forwardings(Node, ListBox),
    {noreply, State};
handle_event(Request, State) ->
    ?LOG_INFO(#{who => ?MODULE, what => handle_event, request => Request}),
    {noreply, State}.


handle_call(Request, _From, #state{toolbar = Toolbar} = State) ->
    ?LOG_INFO(#{who => ?MODULE, what => handle_call, request => Request}),
    wxToolBar:connect(Toolbar, command_menu_selected),
    {reply, ok, State}.


handle_info(Info, State) ->
    erlang:error(not_implemented).


terminate(_Reason, _State) ->
    ok.
