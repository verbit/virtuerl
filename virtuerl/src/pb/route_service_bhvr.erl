%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service RouteService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(route_service_bhvr).

%% Unary RPC
-callback get_route_table(ctx:t(), route_pb:route_table_identifier()) ->
    {ok, route_pb:route_table(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_route_tables(ctx:t(), route_pb:list_route_tables_request()) ->
    {ok, route_pb:list_route_tables_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback create_route_table(ctx:t(), route_pb:create_route_table_request()) ->
    {ok, route_pb:route_table(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_route_table(ctx:t(), route_pb:route_table_identifier()) ->
    {ok, route_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback get_route(ctx:t(), route_pb:route_identifier()) ->
    {ok, route_pb:route(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_routes(ctx:t(), route_pb:list_routes_request()) ->
    {ok, route_pb:list_routes_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback put_route(ctx:t(), route_pb:put_route_request()) ->
    {ok, route_pb:route(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_route(ctx:t(), route_pb:route_identifier()) ->
    {ok, route_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback sync(ctx:t(), route_pb:sync_request()) ->
    {ok, route_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

