%%%-------------------------------------------------------------------
%% @doc Client module for grpc service VolumeService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(volume_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'VolumeService').
-define(PROTO_MODULE, 'controller_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec get_volume(controller_pb:get_volume_request()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_volume(Input) ->
    get_volume(ctx:new(), Input, #{}).

-spec get_volume(ctx:t() | controller_pb:get_volume_request(), controller_pb:get_volume_request() | grpcbox_client:options()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_volume(Ctx, Input) when ?is_ctx(Ctx) ->
    get_volume(Ctx, Input, #{});
get_volume(Input, Options) ->
    get_volume(ctx:new(), Input, Options).

-spec get_volume(ctx:t(), controller_pb:get_volume_request(), grpcbox_client:options()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_volume(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/GetVolume">>, Input, ?DEF(get_volume_request, volume, <<"GetVolumeRequest">>), Options).

%% @doc Unary RPC
-spec list_volumes(controller_pb:list_volumes_request()) ->
    {ok, controller_pb:list_volumes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_volumes(Input) ->
    list_volumes(ctx:new(), Input, #{}).

-spec list_volumes(ctx:t() | controller_pb:list_volumes_request(), controller_pb:list_volumes_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_volumes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_volumes(Ctx, Input) when ?is_ctx(Ctx) ->
    list_volumes(Ctx, Input, #{});
list_volumes(Input, Options) ->
    list_volumes(ctx:new(), Input, Options).

-spec list_volumes(ctx:t(), controller_pb:list_volumes_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_volumes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_volumes(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/ListVolumes">>, Input, ?DEF(list_volumes_request, list_volumes_response, <<"ListVolumesRequest">>), Options).

%% @doc Unary RPC
-spec create_volume(controller_pb:create_volume_request()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_volume(Input) ->
    create_volume(ctx:new(), Input, #{}).

-spec create_volume(ctx:t() | controller_pb:create_volume_request(), controller_pb:create_volume_request() | grpcbox_client:options()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_volume(Ctx, Input) when ?is_ctx(Ctx) ->
    create_volume(Ctx, Input, #{});
create_volume(Input, Options) ->
    create_volume(ctx:new(), Input, Options).

-spec create_volume(ctx:t(), controller_pb:create_volume_request(), grpcbox_client:options()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_volume(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/CreateVolume">>, Input, ?DEF(create_volume_request, volume, <<"CreateVolumeRequest">>), Options).

%% @doc Unary RPC
-spec update_volume(controller_pb:update_volume_request()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
update_volume(Input) ->
    update_volume(ctx:new(), Input, #{}).

-spec update_volume(ctx:t() | controller_pb:update_volume_request(), controller_pb:update_volume_request() | grpcbox_client:options()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
update_volume(Ctx, Input) when ?is_ctx(Ctx) ->
    update_volume(Ctx, Input, #{});
update_volume(Input, Options) ->
    update_volume(ctx:new(), Input, Options).

-spec update_volume(ctx:t(), controller_pb:update_volume_request(), grpcbox_client:options()) ->
    {ok, controller_pb:volume(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
update_volume(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/UpdateVolume">>, Input, ?DEF(update_volume_request, volume, <<"UpdateVolumeRequest">>), Options).

%% @doc Unary RPC
-spec delete_volume(controller_pb:delete_volume_request()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_volume(Input) ->
    delete_volume(ctx:new(), Input, #{}).

-spec delete_volume(ctx:t() | controller_pb:delete_volume_request(), controller_pb:delete_volume_request() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_volume(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_volume(Ctx, Input, #{});
delete_volume(Input, Options) ->
    delete_volume(ctx:new(), Input, Options).

-spec delete_volume(ctx:t(), controller_pb:delete_volume_request(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_volume(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/DeleteVolume">>, Input, ?DEF(delete_volume_request, empty, <<"DeleteVolumeRequest">>), Options).

%% @doc Unary RPC
-spec list_volume_attachments(controller_pb:list_volume_attachments_request()) ->
    {ok, controller_pb:list_volume_attachments_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_volume_attachments(Input) ->
    list_volume_attachments(ctx:new(), Input, #{}).

-spec list_volume_attachments(ctx:t() | controller_pb:list_volume_attachments_request(), controller_pb:list_volume_attachments_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_volume_attachments_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_volume_attachments(Ctx, Input) when ?is_ctx(Ctx) ->
    list_volume_attachments(Ctx, Input, #{});
list_volume_attachments(Input, Options) ->
    list_volume_attachments(ctx:new(), Input, Options).

-spec list_volume_attachments(ctx:t(), controller_pb:list_volume_attachments_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_volume_attachments_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_volume_attachments(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/ListVolumeAttachments">>, Input, ?DEF(list_volume_attachments_request, list_volume_attachments_response, <<"ListVolumeAttachmentsRequest">>), Options).

%% @doc Unary RPC
-spec get_volume_attachment(controller_pb:volume_attachment_identifier()) ->
    {ok, controller_pb:volume_attachment(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_volume_attachment(Input) ->
    get_volume_attachment(ctx:new(), Input, #{}).

-spec get_volume_attachment(ctx:t() | controller_pb:volume_attachment_identifier(), controller_pb:volume_attachment_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:volume_attachment(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_volume_attachment(Ctx, Input) when ?is_ctx(Ctx) ->
    get_volume_attachment(Ctx, Input, #{});
get_volume_attachment(Input, Options) ->
    get_volume_attachment(ctx:new(), Input, Options).

-spec get_volume_attachment(ctx:t(), controller_pb:volume_attachment_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:volume_attachment(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_volume_attachment(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/GetVolumeAttachment">>, Input, ?DEF(volume_attachment_identifier, volume_attachment, <<"VolumeAttachmentIdentifier">>), Options).

%% @doc Unary RPC
-spec attach_volume(controller_pb:volume_attachment_identifier()) ->
    {ok, controller_pb:volume_attachment(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
attach_volume(Input) ->
    attach_volume(ctx:new(), Input, #{}).

-spec attach_volume(ctx:t() | controller_pb:volume_attachment_identifier(), controller_pb:volume_attachment_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:volume_attachment(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
attach_volume(Ctx, Input) when ?is_ctx(Ctx) ->
    attach_volume(Ctx, Input, #{});
attach_volume(Input, Options) ->
    attach_volume(ctx:new(), Input, Options).

-spec attach_volume(ctx:t(), controller_pb:volume_attachment_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:volume_attachment(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
attach_volume(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/AttachVolume">>, Input, ?DEF(volume_attachment_identifier, volume_attachment, <<"VolumeAttachmentIdentifier">>), Options).

%% @doc Unary RPC
-spec detach_volume(controller_pb:volume_attachment_identifier()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
detach_volume(Input) ->
    detach_volume(ctx:new(), Input, #{}).

-spec detach_volume(ctx:t() | controller_pb:volume_attachment_identifier(), controller_pb:volume_attachment_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
detach_volume(Ctx, Input) when ?is_ctx(Ctx) ->
    detach_volume(Ctx, Input, #{});
detach_volume(Input, Options) ->
    detach_volume(ctx:new(), Input, Options).

-spec detach_volume(ctx:t(), controller_pb:volume_attachment_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
detach_volume(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/VolumeService/DetachVolume">>, Input, ?DEF(volume_attachment_identifier, empty, <<"VolumeAttachmentIdentifier">>), Options).

