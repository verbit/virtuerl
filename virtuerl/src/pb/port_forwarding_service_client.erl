%%%-------------------------------------------------------------------
%% @doc Client module for grpc service PortForwardingService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(port_forwarding_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'PortForwardingService').
-define(PROTO_MODULE, 'port_forwarding_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec get_port_forwarding(port_forwarding_pb:port_forwarding_identifier()) ->
    {ok, port_forwarding_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_port_forwarding(Input) ->
    get_port_forwarding(ctx:new(), Input, #{}).

-spec get_port_forwarding(ctx:t() | port_forwarding_pb:port_forwarding_identifier(), port_forwarding_pb:port_forwarding_identifier() | grpcbox_client:options()) ->
    {ok, port_forwarding_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_port_forwarding(Ctx, Input) when ?is_ctx(Ctx) ->
    get_port_forwarding(Ctx, Input, #{});
get_port_forwarding(Input, Options) ->
    get_port_forwarding(ctx:new(), Input, Options).

-spec get_port_forwarding(ctx:t(), port_forwarding_pb:port_forwarding_identifier(), grpcbox_client:options()) ->
    {ok, port_forwarding_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_port_forwarding(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/PortForwardingService/GetPortForwarding">>, Input, ?DEF(port_forwarding_identifier, port_forwarding, <<"PortForwardingIdentifier">>), Options).

%% @doc Unary RPC
-spec list_port_forwardings(port_forwarding_pb:list_port_forwardings_request()) ->
    {ok, port_forwarding_pb:list_port_forwardings_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_port_forwardings(Input) ->
    list_port_forwardings(ctx:new(), Input, #{}).

-spec list_port_forwardings(ctx:t() | port_forwarding_pb:list_port_forwardings_request(), port_forwarding_pb:list_port_forwardings_request() | grpcbox_client:options()) ->
    {ok, port_forwarding_pb:list_port_forwardings_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_port_forwardings(Ctx, Input) when ?is_ctx(Ctx) ->
    list_port_forwardings(Ctx, Input, #{});
list_port_forwardings(Input, Options) ->
    list_port_forwardings(ctx:new(), Input, Options).

-spec list_port_forwardings(ctx:t(), port_forwarding_pb:list_port_forwardings_request(), grpcbox_client:options()) ->
    {ok, port_forwarding_pb:list_port_forwardings_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_port_forwardings(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/PortForwardingService/ListPortForwardings">>, Input, ?DEF(list_port_forwardings_request, list_port_forwardings_response, <<"ListPortForwardingsRequest">>), Options).

%% @doc Unary RPC
-spec put_port_forwarding(port_forwarding_pb:put_port_forwarding_request()) ->
    {ok, port_forwarding_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_port_forwarding(Input) ->
    put_port_forwarding(ctx:new(), Input, #{}).

-spec put_port_forwarding(ctx:t() | port_forwarding_pb:put_port_forwarding_request(), port_forwarding_pb:put_port_forwarding_request() | grpcbox_client:options()) ->
    {ok, port_forwarding_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_port_forwarding(Ctx, Input) when ?is_ctx(Ctx) ->
    put_port_forwarding(Ctx, Input, #{});
put_port_forwarding(Input, Options) ->
    put_port_forwarding(ctx:new(), Input, Options).

-spec put_port_forwarding(ctx:t(), port_forwarding_pb:put_port_forwarding_request(), grpcbox_client:options()) ->
    {ok, port_forwarding_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_port_forwarding(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/PortForwardingService/PutPortForwarding">>, Input, ?DEF(put_port_forwarding_request, port_forwarding, <<"PutPortForwardingRequest">>), Options).

%% @doc Unary RPC
-spec delete_port_forwarding(port_forwarding_pb:port_forwarding_identifier()) ->
    {ok, port_forwarding_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_port_forwarding(Input) ->
    delete_port_forwarding(ctx:new(), Input, #{}).

-spec delete_port_forwarding(ctx:t() | port_forwarding_pb:port_forwarding_identifier(), port_forwarding_pb:port_forwarding_identifier() | grpcbox_client:options()) ->
    {ok, port_forwarding_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_port_forwarding(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_port_forwarding(Ctx, Input, #{});
delete_port_forwarding(Input, Options) ->
    delete_port_forwarding(ctx:new(), Input, Options).

-spec delete_port_forwarding(ctx:t(), port_forwarding_pb:port_forwarding_identifier(), grpcbox_client:options()) ->
    {ok, port_forwarding_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_port_forwarding(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/PortForwardingService/DeletePortForwarding">>, Input, ?DEF(port_forwarding_identifier, empty, <<"PortForwardingIdentifier">>), Options).

