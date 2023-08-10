%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service PortForwardingService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(port_forwarding_service_bhvr).

%% Unary RPC
-callback get_port_forwarding(ctx:t(), port_forwarding_pb:port_forwarding_identifier()) ->
    {ok, port_forwarding_pb:port_forwarding(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_port_forwardings(ctx:t(), port_forwarding_pb:list_port_forwardings_request()) ->
    {ok, port_forwarding_pb:list_port_forwardings_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback put_port_forwarding(ctx:t(), port_forwarding_pb:put_port_forwarding_request()) ->
    {ok, port_forwarding_pb:port_forwarding(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_port_forwarding(ctx:t(), port_forwarding_pb:port_forwarding_identifier()) ->
    {ok, port_forwarding_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

