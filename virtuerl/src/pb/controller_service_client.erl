%%%-------------------------------------------------------------------
%% @doc Client module for grpc service ControllerService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(controller_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'ControllerService').
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
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetDNSRecord">>, Input, ?DEF(dns_record_identifier, dns_record, <<"DNSRecordIdentifier">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListDNSRecords">>, Input, ?DEF(list_dns_records_request, list_dns_records_response, <<"ListDNSRecordsRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/PutDNSRecord">>, Input, ?DEF(put_dns_record_request, dns_record, <<"PutDNSRecordRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/DeleteDNSRecord">>, Input, ?DEF(dns_record_identifier, empty, <<"DNSRecordIdentifier">>), Options).

%% @doc Unary RPC
-spec get_network(controller_pb:get_network_request()) ->
    {ok, controller_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_network(Input) ->
    get_network(ctx:new(), Input, #{}).

-spec get_network(ctx:t() | controller_pb:get_network_request(), controller_pb:get_network_request() | grpcbox_client:options()) ->
    {ok, controller_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_network(Ctx, Input) when ?is_ctx(Ctx) ->
    get_network(Ctx, Input, #{});
get_network(Input, Options) ->
    get_network(ctx:new(), Input, Options).

-spec get_network(ctx:t(), controller_pb:get_network_request(), grpcbox_client:options()) ->
    {ok, controller_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_network(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetNetwork">>, Input, ?DEF(get_network_request, network, <<"GetNetworkRequest">>), Options).

%% @doc Unary RPC
-spec list_networks(controller_pb:list_networks_request()) ->
    {ok, controller_pb:list_networks_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_networks(Input) ->
    list_networks(ctx:new(), Input, #{}).

-spec list_networks(ctx:t() | controller_pb:list_networks_request(), controller_pb:list_networks_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_networks_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_networks(Ctx, Input) when ?is_ctx(Ctx) ->
    list_networks(Ctx, Input, #{});
list_networks(Input, Options) ->
    list_networks(ctx:new(), Input, Options).

-spec list_networks(ctx:t(), controller_pb:list_networks_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_networks_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_networks(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListNetworks">>, Input, ?DEF(list_networks_request, list_networks_response, <<"ListNetworksRequest">>), Options).

%% @doc Unary RPC
-spec create_network(controller_pb:create_network_request()) ->
    {ok, controller_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_network(Input) ->
    create_network(ctx:new(), Input, #{}).

-spec create_network(ctx:t() | controller_pb:create_network_request(), controller_pb:create_network_request() | grpcbox_client:options()) ->
    {ok, controller_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_network(Ctx, Input) when ?is_ctx(Ctx) ->
    create_network(Ctx, Input, #{});
create_network(Input, Options) ->
    create_network(ctx:new(), Input, Options).

-spec create_network(ctx:t(), controller_pb:create_network_request(), grpcbox_client:options()) ->
    {ok, controller_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_network(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/CreateNetwork">>, Input, ?DEF(create_network_request, network, <<"CreateNetworkRequest">>), Options).

%% @doc Unary RPC
-spec delete_network(controller_pb:delete_network_request()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_network(Input) ->
    delete_network(ctx:new(), Input, #{}).

-spec delete_network(ctx:t() | controller_pb:delete_network_request(), controller_pb:delete_network_request() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_network(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_network(Ctx, Input, #{});
delete_network(Input, Options) ->
    delete_network(ctx:new(), Input, Options).

-spec delete_network(ctx:t(), controller_pb:delete_network_request(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_network(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/DeleteNetwork">>, Input, ?DEF(delete_network_request, empty, <<"DeleteNetworkRequest">>), Options).

%% @doc Unary RPC
-spec start_domain(controller_pb:start_domain_request()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
start_domain(Input) ->
    start_domain(ctx:new(), Input, #{}).

-spec start_domain(ctx:t() | controller_pb:start_domain_request(), controller_pb:start_domain_request() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
start_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    start_domain(Ctx, Input, #{});
start_domain(Input, Options) ->
    start_domain(ctx:new(), Input, Options).

-spec start_domain(ctx:t(), controller_pb:start_domain_request(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
start_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/StartDomain">>, Input, ?DEF(start_domain_request, empty, <<"StartDomainRequest">>), Options).

%% @doc Unary RPC
-spec stop_domain(controller_pb:stop_domain_request()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
stop_domain(Input) ->
    stop_domain(ctx:new(), Input, #{}).

-spec stop_domain(ctx:t() | controller_pb:stop_domain_request(), controller_pb:stop_domain_request() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
stop_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    stop_domain(Ctx, Input, #{});
stop_domain(Input, Options) ->
    stop_domain(ctx:new(), Input, Options).

-spec stop_domain(ctx:t(), controller_pb:stop_domain_request(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
stop_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/StopDomain">>, Input, ?DEF(stop_domain_request, empty, <<"StopDomainRequest">>), Options).

%% @doc Unary RPC
-spec get_domain(controller_pb:get_domain_request()) ->
    {ok, controller_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_domain(Input) ->
    get_domain(ctx:new(), Input, #{}).

-spec get_domain(ctx:t() | controller_pb:get_domain_request(), controller_pb:get_domain_request() | grpcbox_client:options()) ->
    {ok, controller_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    get_domain(Ctx, Input, #{});
get_domain(Input, Options) ->
    get_domain(ctx:new(), Input, Options).

-spec get_domain(ctx:t(), controller_pb:get_domain_request(), grpcbox_client:options()) ->
    {ok, controller_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetDomain">>, Input, ?DEF(get_domain_request, domain, <<"GetDomainRequest">>), Options).

%% @doc Unary RPC
-spec list_domains(controller_pb:list_domains_request()) ->
    {ok, controller_pb:list_domains_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_domains(Input) ->
    list_domains(ctx:new(), Input, #{}).

-spec list_domains(ctx:t() | controller_pb:list_domains_request(), controller_pb:list_domains_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_domains_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_domains(Ctx, Input) when ?is_ctx(Ctx) ->
    list_domains(Ctx, Input, #{});
list_domains(Input, Options) ->
    list_domains(ctx:new(), Input, Options).

-spec list_domains(ctx:t(), controller_pb:list_domains_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_domains_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_domains(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListDomains">>, Input, ?DEF(list_domains_request, list_domains_response, <<"ListDomainsRequest">>), Options).

%% @doc Unary RPC
-spec create_domain(controller_pb:create_domain_request()) ->
    {ok, controller_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_domain(Input) ->
    create_domain(ctx:new(), Input, #{}).

-spec create_domain(ctx:t() | controller_pb:create_domain_request(), controller_pb:create_domain_request() | grpcbox_client:options()) ->
    {ok, controller_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    create_domain(Ctx, Input, #{});
create_domain(Input, Options) ->
    create_domain(ctx:new(), Input, Options).

-spec create_domain(ctx:t(), controller_pb:create_domain_request(), grpcbox_client:options()) ->
    {ok, controller_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/CreateDomain">>, Input, ?DEF(create_domain_request, domain, <<"CreateDomainRequest">>), Options).

%% @doc Unary RPC
-spec delete_domain(controller_pb:delete_domain_request()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_domain(Input) ->
    delete_domain(ctx:new(), Input, #{}).

-spec delete_domain(ctx:t() | controller_pb:delete_domain_request(), controller_pb:delete_domain_request() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_domain(Ctx, Input, #{});
delete_domain(Input, Options) ->
    delete_domain(ctx:new(), Input, Options).

-spec delete_domain(ctx:t(), controller_pb:delete_domain_request(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/DeleteDomain">>, Input, ?DEF(delete_domain_request, empty, <<"DeleteDomainRequest">>), Options).

%% @doc 
-spec download_image(controller_pb:download_image_request()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
download_image(Input) ->
    download_image(ctx:new(), Input, #{}).

-spec download_image(ctx:t() | controller_pb:download_image_request(), controller_pb:download_image_request() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
download_image(Ctx, Input) when ?is_ctx(Ctx) ->
    download_image(Ctx, Input, #{});
download_image(Input, Options) ->
    download_image(ctx:new(), Input, Options).

-spec download_image(ctx:t(), controller_pb:download_image_request(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
download_image(Ctx, Input, Options) ->
    grpcbox_client:stream(Ctx, <<"/ControllerService/DownloadImage">>, Input, ?DEF(download_image_request, image_chunk, <<"DownloadImageRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetVolume">>, Input, ?DEF(get_volume_request, volume, <<"GetVolumeRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListVolumes">>, Input, ?DEF(list_volumes_request, list_volumes_response, <<"ListVolumesRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/CreateVolume">>, Input, ?DEF(create_volume_request, volume, <<"CreateVolumeRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/UpdateVolume">>, Input, ?DEF(update_volume_request, volume, <<"UpdateVolumeRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/DeleteVolume">>, Input, ?DEF(delete_volume_request, empty, <<"DeleteVolumeRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListVolumeAttachments">>, Input, ?DEF(list_volume_attachments_request, list_volume_attachments_response, <<"ListVolumeAttachmentsRequest">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetVolumeAttachment">>, Input, ?DEF(volume_attachment_identifier, volume_attachment, <<"VolumeAttachmentIdentifier">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/AttachVolume">>, Input, ?DEF(volume_attachment_identifier, volume_attachment, <<"VolumeAttachmentIdentifier">>), Options).

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
    grpcbox_client:unary(Ctx, <<"/ControllerService/DetachVolume">>, Input, ?DEF(volume_attachment_identifier, empty, <<"VolumeAttachmentIdentifier">>), Options).

%% @doc Unary RPC
-spec get_port_forwarding(controller_pb:port_forwarding_identifier()) ->
    {ok, controller_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_port_forwarding(Input) ->
    get_port_forwarding(ctx:new(), Input, #{}).

-spec get_port_forwarding(ctx:t() | controller_pb:port_forwarding_identifier(), controller_pb:port_forwarding_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_port_forwarding(Ctx, Input) when ?is_ctx(Ctx) ->
    get_port_forwarding(Ctx, Input, #{});
get_port_forwarding(Input, Options) ->
    get_port_forwarding(ctx:new(), Input, Options).

-spec get_port_forwarding(ctx:t(), controller_pb:port_forwarding_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_port_forwarding(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetPortForwarding">>, Input, ?DEF(port_forwarding_identifier, port_forwarding, <<"PortForwardingIdentifier">>), Options).

%% @doc Unary RPC
-spec list_port_forwardings(controller_pb:list_port_forwardings_request()) ->
    {ok, controller_pb:list_port_forwardings_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_port_forwardings(Input) ->
    list_port_forwardings(ctx:new(), Input, #{}).

-spec list_port_forwardings(ctx:t() | controller_pb:list_port_forwardings_request(), controller_pb:list_port_forwardings_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_port_forwardings_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_port_forwardings(Ctx, Input) when ?is_ctx(Ctx) ->
    list_port_forwardings(Ctx, Input, #{});
list_port_forwardings(Input, Options) ->
    list_port_forwardings(ctx:new(), Input, Options).

-spec list_port_forwardings(ctx:t(), controller_pb:list_port_forwardings_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_port_forwardings_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_port_forwardings(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListPortForwardings">>, Input, ?DEF(list_port_forwardings_request, list_port_forwardings_response, <<"ListPortForwardingsRequest">>), Options).

%% @doc Unary RPC
-spec put_port_forwarding(controller_pb:put_port_forwarding_request()) ->
    {ok, controller_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_port_forwarding(Input) ->
    put_port_forwarding(ctx:new(), Input, #{}).

-spec put_port_forwarding(ctx:t() | controller_pb:put_port_forwarding_request(), controller_pb:put_port_forwarding_request() | grpcbox_client:options()) ->
    {ok, controller_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_port_forwarding(Ctx, Input) when ?is_ctx(Ctx) ->
    put_port_forwarding(Ctx, Input, #{});
put_port_forwarding(Input, Options) ->
    put_port_forwarding(ctx:new(), Input, Options).

-spec put_port_forwarding(ctx:t(), controller_pb:put_port_forwarding_request(), grpcbox_client:options()) ->
    {ok, controller_pb:port_forwarding(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_port_forwarding(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/PutPortForwarding">>, Input, ?DEF(put_port_forwarding_request, port_forwarding, <<"PutPortForwardingRequest">>), Options).

%% @doc Unary RPC
-spec delete_port_forwarding(controller_pb:port_forwarding_identifier()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_port_forwarding(Input) ->
    delete_port_forwarding(ctx:new(), Input, #{}).

-spec delete_port_forwarding(ctx:t() | controller_pb:port_forwarding_identifier(), controller_pb:port_forwarding_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_port_forwarding(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_port_forwarding(Ctx, Input, #{});
delete_port_forwarding(Input, Options) ->
    delete_port_forwarding(ctx:new(), Input, Options).

-spec delete_port_forwarding(ctx:t(), controller_pb:port_forwarding_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_port_forwarding(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/DeletePortForwarding">>, Input, ?DEF(port_forwarding_identifier, empty, <<"PortForwardingIdentifier">>), Options).

%% @doc Unary RPC
-spec get_route_table(controller_pb:route_table_identifier()) ->
    {ok, controller_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route_table(Input) ->
    get_route_table(ctx:new(), Input, #{}).

-spec get_route_table(ctx:t() | controller_pb:route_table_identifier(), controller_pb:route_table_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route_table(Ctx, Input) when ?is_ctx(Ctx) ->
    get_route_table(Ctx, Input, #{});
get_route_table(Input, Options) ->
    get_route_table(ctx:new(), Input, Options).

-spec get_route_table(ctx:t(), controller_pb:route_table_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route_table(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetRouteTable">>, Input, ?DEF(route_table_identifier, route_table, <<"RouteTableIdentifier">>), Options).

%% @doc Unary RPC
-spec list_route_tables(controller_pb:list_route_tables_request()) ->
    {ok, controller_pb:list_route_tables_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_route_tables(Input) ->
    list_route_tables(ctx:new(), Input, #{}).

-spec list_route_tables(ctx:t() | controller_pb:list_route_tables_request(), controller_pb:list_route_tables_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_route_tables_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_route_tables(Ctx, Input) when ?is_ctx(Ctx) ->
    list_route_tables(Ctx, Input, #{});
list_route_tables(Input, Options) ->
    list_route_tables(ctx:new(), Input, Options).

-spec list_route_tables(ctx:t(), controller_pb:list_route_tables_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_route_tables_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_route_tables(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListRouteTables">>, Input, ?DEF(list_route_tables_request, list_route_tables_response, <<"ListRouteTablesRequest">>), Options).

%% @doc Unary RPC
-spec create_route_table(controller_pb:create_route_table_request()) ->
    {ok, controller_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_route_table(Input) ->
    create_route_table(ctx:new(), Input, #{}).

-spec create_route_table(ctx:t() | controller_pb:create_route_table_request(), controller_pb:create_route_table_request() | grpcbox_client:options()) ->
    {ok, controller_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_route_table(Ctx, Input) when ?is_ctx(Ctx) ->
    create_route_table(Ctx, Input, #{});
create_route_table(Input, Options) ->
    create_route_table(ctx:new(), Input, Options).

-spec create_route_table(ctx:t(), controller_pb:create_route_table_request(), grpcbox_client:options()) ->
    {ok, controller_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_route_table(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/CreateRouteTable">>, Input, ?DEF(create_route_table_request, route_table, <<"CreateRouteTableRequest">>), Options).

%% @doc Unary RPC
-spec delete_route_table(controller_pb:route_table_identifier()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route_table(Input) ->
    delete_route_table(ctx:new(), Input, #{}).

-spec delete_route_table(ctx:t() | controller_pb:route_table_identifier(), controller_pb:route_table_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route_table(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_route_table(Ctx, Input, #{});
delete_route_table(Input, Options) ->
    delete_route_table(ctx:new(), Input, Options).

-spec delete_route_table(ctx:t(), controller_pb:route_table_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route_table(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/DeleteRouteTable">>, Input, ?DEF(route_table_identifier, empty, <<"RouteTableIdentifier">>), Options).

%% @doc Unary RPC
-spec get_route(controller_pb:route_identifier()) ->
    {ok, controller_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route(Input) ->
    get_route(ctx:new(), Input, #{}).

-spec get_route(ctx:t() | controller_pb:route_identifier(), controller_pb:route_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route(Ctx, Input) when ?is_ctx(Ctx) ->
    get_route(Ctx, Input, #{});
get_route(Input, Options) ->
    get_route(ctx:new(), Input, Options).

-spec get_route(ctx:t(), controller_pb:route_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/GetRoute">>, Input, ?DEF(route_identifier, route, <<"RouteIdentifier">>), Options).

%% @doc Unary RPC
-spec list_routes(controller_pb:list_routes_request()) ->
    {ok, controller_pb:list_routes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_routes(Input) ->
    list_routes(ctx:new(), Input, #{}).

-spec list_routes(ctx:t() | controller_pb:list_routes_request(), controller_pb:list_routes_request() | grpcbox_client:options()) ->
    {ok, controller_pb:list_routes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_routes(Ctx, Input) when ?is_ctx(Ctx) ->
    list_routes(Ctx, Input, #{});
list_routes(Input, Options) ->
    list_routes(ctx:new(), Input, Options).

-spec list_routes(ctx:t(), controller_pb:list_routes_request(), grpcbox_client:options()) ->
    {ok, controller_pb:list_routes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_routes(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/ListRoutes">>, Input, ?DEF(list_routes_request, list_routes_response, <<"ListRoutesRequest">>), Options).

%% @doc Unary RPC
-spec put_route(controller_pb:put_route_request()) ->
    {ok, controller_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_route(Input) ->
    put_route(ctx:new(), Input, #{}).

-spec put_route(ctx:t() | controller_pb:put_route_request(), controller_pb:put_route_request() | grpcbox_client:options()) ->
    {ok, controller_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_route(Ctx, Input) when ?is_ctx(Ctx) ->
    put_route(Ctx, Input, #{});
put_route(Input, Options) ->
    put_route(ctx:new(), Input, Options).

-spec put_route(ctx:t(), controller_pb:put_route_request(), grpcbox_client:options()) ->
    {ok, controller_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_route(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/PutRoute">>, Input, ?DEF(put_route_request, route, <<"PutRouteRequest">>), Options).

%% @doc Unary RPC
-spec delete_route(controller_pb:route_identifier()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route(Input) ->
    delete_route(ctx:new(), Input, #{}).

-spec delete_route(ctx:t() | controller_pb:route_identifier(), controller_pb:route_identifier() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_route(Ctx, Input, #{});
delete_route(Input, Options) ->
    delete_route(ctx:new(), Input, Options).

-spec delete_route(ctx:t(), controller_pb:route_identifier(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/DeleteRoute">>, Input, ?DEF(route_identifier, empty, <<"RouteIdentifier">>), Options).

%% @doc Unary RPC
-spec sync_routes(controller_pb:sync_routes_request()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
sync_routes(Input) ->
    sync_routes(ctx:new(), Input, #{}).

-spec sync_routes(ctx:t() | controller_pb:sync_routes_request(), controller_pb:sync_routes_request() | grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
sync_routes(Ctx, Input) when ?is_ctx(Ctx) ->
    sync_routes(Ctx, Input, #{});
sync_routes(Input, Options) ->
    sync_routes(ctx:new(), Input, Options).

-spec sync_routes(ctx:t(), controller_pb:sync_routes_request(), grpcbox_client:options()) ->
    {ok, controller_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
sync_routes(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/ControllerService/SyncRoutes">>, Input, ?DEF(sync_routes_request, empty, <<"SyncRoutesRequest">>), Options).

