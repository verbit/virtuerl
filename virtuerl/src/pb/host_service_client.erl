%%%-------------------------------------------------------------------
%% @doc Client module for grpc service HostService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(host_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'HostService').
-define(PROTO_MODULE, 'host_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec create_bootstrap_token(host_pb:create_bootstrap_token_request()) ->
    {ok, host_pb:create_bootstrap_token_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_bootstrap_token(Input) ->
    create_bootstrap_token(ctx:new(), Input, #{}).

-spec create_bootstrap_token(ctx:t() | host_pb:create_bootstrap_token_request(), host_pb:create_bootstrap_token_request() | grpcbox_client:options()) ->
    {ok, host_pb:create_bootstrap_token_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_bootstrap_token(Ctx, Input) when ?is_ctx(Ctx) ->
    create_bootstrap_token(Ctx, Input, #{});
create_bootstrap_token(Input, Options) ->
    create_bootstrap_token(ctx:new(), Input, Options).

-spec create_bootstrap_token(ctx:t(), host_pb:create_bootstrap_token_request(), grpcbox_client:options()) ->
    {ok, host_pb:create_bootstrap_token_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_bootstrap_token(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/HostService/CreateBootstrapToken">>, Input, ?DEF(create_bootstrap_token_request, create_bootstrap_token_response, <<"CreateBootstrapTokenRequest">>), Options).

%% @doc Unary RPC
-spec get_host(host_pb:host()) ->
    {ok, host_pb:host(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_host(Input) ->
    get_host(ctx:new(), Input, #{}).

-spec get_host(ctx:t() | host_pb:host(), host_pb:host() | grpcbox_client:options()) ->
    {ok, host_pb:host(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_host(Ctx, Input) when ?is_ctx(Ctx) ->
    get_host(Ctx, Input, #{});
get_host(Input, Options) ->
    get_host(ctx:new(), Input, Options).

-spec get_host(ctx:t(), host_pb:host(), grpcbox_client:options()) ->
    {ok, host_pb:host(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_host(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/HostService/GetHost">>, Input, ?DEF(host, host, <<"Host">>), Options).

%% @doc Unary RPC
-spec list_hosts(host_pb:list_hosts_request()) ->
    {ok, host_pb:list_hosts_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_hosts(Input) ->
    list_hosts(ctx:new(), Input, #{}).

-spec list_hosts(ctx:t() | host_pb:list_hosts_request(), host_pb:list_hosts_request() | grpcbox_client:options()) ->
    {ok, host_pb:list_hosts_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_hosts(Ctx, Input) when ?is_ctx(Ctx) ->
    list_hosts(Ctx, Input, #{});
list_hosts(Input, Options) ->
    list_hosts(ctx:new(), Input, Options).

-spec list_hosts(ctx:t(), host_pb:list_hosts_request(), grpcbox_client:options()) ->
    {ok, host_pb:list_hosts_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_hosts(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/HostService/ListHosts">>, Input, ?DEF(list_hosts_request, list_hosts_response, <<"ListHostsRequest">>), Options).

%% @doc Unary RPC
-spec register(host_pb:register_host_request()) ->
    {ok, host_pb:host(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
register(Input) ->
    register(ctx:new(), Input, #{}).

-spec register(ctx:t() | host_pb:register_host_request(), host_pb:register_host_request() | grpcbox_client:options()) ->
    {ok, host_pb:host(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
register(Ctx, Input) when ?is_ctx(Ctx) ->
    register(Ctx, Input, #{});
register(Input, Options) ->
    register(ctx:new(), Input, Options).

-spec register(ctx:t(), host_pb:register_host_request(), grpcbox_client:options()) ->
    {ok, host_pb:host(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
register(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/HostService/Register">>, Input, ?DEF(register_host_request, host, <<"RegisterHostRequest">>), Options).

%% @doc Unary RPC
-spec deregister(host_pb:host()) ->
    {ok, host_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
deregister(Input) ->
    deregister(ctx:new(), Input, #{}).

-spec deregister(ctx:t() | host_pb:host(), host_pb:host() | grpcbox_client:options()) ->
    {ok, host_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
deregister(Ctx, Input) when ?is_ctx(Ctx) ->
    deregister(Ctx, Input, #{});
deregister(Input, Options) ->
    deregister(ctx:new(), Input, Options).

-spec deregister(ctx:t(), host_pb:host(), grpcbox_client:options()) ->
    {ok, host_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
deregister(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/HostService/Deregister">>, Input, ?DEF(host, empty, <<"Host">>), Options).

%% @doc Unary RPC
-spec heartbeat(host_pb:heartbeat_request()) ->
    {ok, host_pb:heartbeat_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
heartbeat(Input) ->
    heartbeat(ctx:new(), Input, #{}).

-spec heartbeat(ctx:t() | host_pb:heartbeat_request(), host_pb:heartbeat_request() | grpcbox_client:options()) ->
    {ok, host_pb:heartbeat_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
heartbeat(Ctx, Input) when ?is_ctx(Ctx) ->
    heartbeat(Ctx, Input, #{});
heartbeat(Input, Options) ->
    heartbeat(ctx:new(), Input, Options).

-spec heartbeat(ctx:t(), host_pb:heartbeat_request(), grpcbox_client:options()) ->
    {ok, host_pb:heartbeat_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
heartbeat(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/HostService/Heartbeat">>, Input, ?DEF(heartbeat_request, heartbeat_response, <<"HeartbeatRequest">>), Options).

