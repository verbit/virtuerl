%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service DomainService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(domain_service_bhvr).

%% Unary RPC
-callback get_domain(ctx:t(), domain_pb:get_domain_request()) ->
    {ok, domain_pb:domain(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_domains(ctx:t(), domain_pb:list_domains_request()) ->
    {ok, domain_pb:list_domains_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback create_domain(ctx:t(), domain_pb:create_domain_request()) ->
    {ok, domain_pb:domain(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_domain(ctx:t(), domain_pb:delete_domain_request()) ->
    {ok, domain_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% 
-callback download_image(domain_pb:download_image_request(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback get_network(ctx:t(), domain_pb:get_network_request()) ->
    {ok, domain_pb:network(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_networks(ctx:t(), domain_pb:list_networks_request()) ->
    {ok, domain_pb:list_networks_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback create_network(ctx:t(), domain_pb:create_network_request()) ->
    {ok, domain_pb:network(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_network(ctx:t(), domain_pb:delete_network_request()) ->
    {ok, domain_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

