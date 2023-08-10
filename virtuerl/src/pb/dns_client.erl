%%%-------------------------------------------------------------------
%% @doc Client module for grpc service DNS.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dns_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'DNS').
-define(PROTO_MODULE, 'controller_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec get_dns_record(controller_pb:dns_record_identifier()) ->
    {ok, controller_pb:dns_record(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_dns_record(Input) ->
    get_dns_record(ctx:new(), Input, #{}).

-spec get_dns_record(ctx:t() | controller_pb:dns_record_identifier(), controller_pb:dns_record_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:dns_record(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_dns_record(Ctx, Input) when ?is_ctx(Ctx) ->
    get_dns_record(Ctx, Input, #{});
get_dns_record(Input, Options) ->
    get_dns_record(ctx:new(), Input, Options).

-spec get_dns_record(ctx:t(), controller_pb:dns_record_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:dns_record(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_dns_record(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DNS/GetDNSRecord">>, Input, ?DEF(dns_record_identifier, dns_record, <<"DNSRecordIdentifier">>), Options).

%% @doc Unary RPC
-spec list_dns_records(controller_pb:list_dns_records_request()) ->
    {ok, controller_pb:list_dns_records_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_dns_records(Input) ->
    list_dns_records(ctx:new(), Input, #{}).

-spec list_dns_records(ctx:t() | controller_pb:list_dns_records_request(), controller_pb:list_dns_records_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_dns_records_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_dns_records(Ctx, Input) when ?is_ctx(Ctx) ->
    list_dns_records(Ctx, Input, #{});
list_dns_records(Input, Options) ->
    list_dns_records(ctx:new(), Input, Options).

-spec list_dns_records(ctx:t(), controller_pb:list_dns_records_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_dns_records_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_dns_records(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DNS/ListDNSRecords">>, Input, ?DEF(list_dns_records_request, list_dns_records_response, <<"ListDNSRecordsRequest">>), Options).

%% @doc Unary RPC
-spec put_dns_record(controller_pb:put_dns_record_request()) ->
    {ok, controller_pb:dns_record(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_dns_record(Input) ->
    put_dns_record(ctx:new(), Input, #{}).

-spec put_dns_record(ctx:t() | controller_pb:put_dns_record_request(), controller_pb:put_dns_record_request() | grpcbox_client:options()) ->
    {ok, controller_pb:dns_record(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_dns_record(Ctx, Input) when ?is_ctx(Ctx) ->
    put_dns_record(Ctx, Input, #{});
put_dns_record(Input, Options) ->
    put_dns_record(ctx:new(), Input, Options).

-spec put_dns_record(ctx:t(), controller_pb:put_dns_record_request(), grpcbox_client:options()) ->
    {ok, controller_pb:dns_record(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_dns_record(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DNS/PutDNSRecord">>, Input, ?DEF(put_dns_record_request, dns_record, <<"PutDNSRecordRequest">>), Options).

%% @doc Unary RPC
-spec delete_dns_record(controller_pb:dns_record_identifier()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_dns_record(Input) ->
    delete_dns_record(ctx:new(), Input, #{}).

-spec delete_dns_record(ctx:t() | controller_pb:dns_record_identifier(), controller_pb:dns_record_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_dns_record(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_dns_record(Ctx, Input, #{});
delete_dns_record(Input, Options) ->
    delete_dns_record(ctx:new(), Input, Options).

-spec delete_dns_record(ctx:t(), controller_pb:dns_record_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_dns_record(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DNS/DeleteDNSRecord">>, Input, ?DEF(dns_record_identifier, empty, <<"DNSRecordIdentifier">>), Options).

