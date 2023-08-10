%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service DNS.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dns_bhvr).

%% Unary RPC
-callback get_dns_record(ctx:t(), controller_pb:dns_record_identifier()) ->
    {ok, controller_pb:dns_record(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback list_dns_records(ctx:t(), controller_pb:list_dns_records_request()) ->
    {ok, controller_pb:list_dns_records_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback put_dns_record(ctx:t(), controller_pb:put_dns_record_request()) ->
    {ok, controller_pb:dns_record(), ctx:t()} | grpcbox_stream:grpc_error_response().

%% Unary RPC
-callback delete_dns_record(ctx:t(), controller_pb:dns_record_identifier()) ->
    {ok, controller_pb:empty(), ctx:t()} | grpcbox_stream:grpc_error_response().

