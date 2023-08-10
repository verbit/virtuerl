%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service VolumeService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(volume_service_bhvr).

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

