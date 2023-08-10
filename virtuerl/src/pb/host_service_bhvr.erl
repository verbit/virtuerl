%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service HostService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(host_service_bhvr).

%% Unary RPC
-callback create_bootstrap_token(ctx:t(), host_pb:create_bootstrap_token_request()) ->
    {ok, host_pb:create_bootstrap_token_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback get_host(ctx:t(), host_pb:host()) ->
    {ok, host_pb:host(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_hosts(ctx:t(), host_pb:list_hosts_request()) ->
    {ok, host_pb:list_hosts_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback register(ctx:t(), host_pb:register_host_request()) ->
    {ok, host_pb:host(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback deregister(ctx:t(), host_pb:host()) ->
    {ok, host_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback heartbeat(ctx:t(), host_pb:heartbeat_request()) ->
    {ok, host_pb:heartbeat_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

