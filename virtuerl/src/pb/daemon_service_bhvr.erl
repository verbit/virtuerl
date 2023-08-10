%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service DaemonService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(daemon_service_bhvr).

%% Unary RPC
-callback get_network(ctx:t(), controller_pb:get_network_request()) ->
    {ok, controller_pb:network(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_networks(ctx:t(), controller_pb:list_networks_request()) ->
    {ok, controller_pb:list_networks_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback create_network(ctx:t(), controller_pb:create_network_request()) ->
    {ok, controller_pb:network(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_network(ctx:t(), controller_pb:delete_network_request()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback start_domain(ctx:t(), controller_pb:start_domain_request()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback stop_domain(ctx:t(), controller_pb:stop_domain_request()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback get_domain(ctx:t(), controller_pb:get_domain_request()) ->
    {ok, controller_pb:domain(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_domains(ctx:t(), controller_pb:list_domains_request()) ->
    {ok, controller_pb:list_domains_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback create_domain(ctx:t(), controller_pb:create_domain_request()) ->
    {ok, controller_pb:domain(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_domain(ctx:t(), controller_pb:delete_domain_request()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% 
-callback download_image(controller_pb:download_image_request(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback get_volume(ctx:t(), controller_pb:get_volume_request()) ->
    {ok, controller_pb:volume(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_volumes(ctx:t(), controller_pb:list_volumes_request()) ->
    {ok, controller_pb:list_volumes_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback create_volume(ctx:t(), controller_pb:create_volume_request()) ->
    {ok, controller_pb:volume(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback update_volume(ctx:t(), controller_pb:update_volume_request()) ->
    {ok, controller_pb:volume(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_volume(ctx:t(), controller_pb:delete_volume_request()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_volume_attachments(ctx:t(), controller_pb:list_volume_attachments_request()) ->
    {ok, controller_pb:list_volume_attachments_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback get_volume_attachment(ctx:t(), controller_pb:volume_attachment_identifier()) ->
    {ok, controller_pb:volume_attachment(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback attach_volume(ctx:t(), controller_pb:volume_attachment_identifier()) ->
    {ok, controller_pb:volume_attachment(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback detach_volume(ctx:t(), controller_pb:volume_attachment_identifier()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback get_port_forwarding(ctx:t(), controller_pb:port_forwarding_identifier()) ->
    {ok, controller_pb:port_forwarding(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_port_forwardings(ctx:t(), controller_pb:list_port_forwardings_request()) ->
    {ok, controller_pb:list_port_forwardings_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback put_port_forwarding(ctx:t(), controller_pb:put_port_forwarding_request()) ->
    {ok, controller_pb:port_forwarding(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_port_forwarding(ctx:t(), controller_pb:port_forwarding_identifier()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback sync_routes(ctx:t(), controller_pb:sync_routes_request()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

