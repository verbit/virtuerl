%%%-------------------------------------------------------------------
%% @doc Client module for grpc service DomainService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(domain_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'DomainService').
-define(PROTO_MODULE, 'domain_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec get_domain(domain_pb:get_domain_request()) ->
    {ok, domain_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_domain(Input) ->
    get_domain(ctx:new(), Input, #{}).

-spec get_domain(ctx:t() | domain_pb:get_domain_request(), domain_pb:get_domain_request() | grpcbox_client:options()) ->
    {ok, domain_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    get_domain(Ctx, Input, #{});
get_domain(Input, Options) ->
    get_domain(ctx:new(), Input, Options).

-spec get_domain(ctx:t(), domain_pb:get_domain_request(), grpcbox_client:options()) ->
    {ok, domain_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/GetDomain">>, Input, ?DEF(get_domain_request, domain, <<"GetDomainRequest">>), Options).

%% @doc Unary RPC
-spec list_domains(domain_pb:list_domains_request()) ->
    {ok, domain_pb:list_domains_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_domains(Input) ->
    list_domains(ctx:new(), Input, #{}).

-spec list_domains(ctx:t() | domain_pb:list_domains_request(), domain_pb:list_domains_request() | grpcbox_client:options()) ->
    {ok, domain_pb:list_domains_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_domains(Ctx, Input) when ?is_ctx(Ctx) ->
    list_domains(Ctx, Input, #{});
list_domains(Input, Options) ->
    list_domains(ctx:new(), Input, Options).

-spec list_domains(ctx:t(), domain_pb:list_domains_request(), grpcbox_client:options()) ->
    {ok, domain_pb:list_domains_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_domains(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/ListDomains">>, Input, ?DEF(list_domains_request, list_domains_response, <<"ListDomainsRequest">>), Options).

%% @doc Unary RPC
-spec create_domain(domain_pb:create_domain_request()) ->
    {ok, domain_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_domain(Input) ->
    create_domain(ctx:new(), Input, #{}).

-spec create_domain(ctx:t() | domain_pb:create_domain_request(), domain_pb:create_domain_request() | grpcbox_client:options()) ->
    {ok, domain_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    create_domain(Ctx, Input, #{});
create_domain(Input, Options) ->
    create_domain(ctx:new(), Input, Options).

-spec create_domain(ctx:t(), domain_pb:create_domain_request(), grpcbox_client:options()) ->
    {ok, domain_pb:domain(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/CreateDomain">>, Input, ?DEF(create_domain_request, domain, <<"CreateDomainRequest">>), Options).

%% @doc Unary RPC
-spec delete_domain(domain_pb:delete_domain_request()) ->
    {ok, domain_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_domain(Input) ->
    delete_domain(ctx:new(), Input, #{}).

-spec delete_domain(ctx:t() | domain_pb:delete_domain_request(), domain_pb:delete_domain_request() | grpcbox_client:options()) ->
    {ok, domain_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_domain(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_domain(Ctx, Input, #{});
delete_domain(Input, Options) ->
    delete_domain(ctx:new(), Input, Options).

-spec delete_domain(ctx:t(), domain_pb:delete_domain_request(), grpcbox_client:options()) ->
    {ok, domain_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_domain(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/DeleteDomain">>, Input, ?DEF(delete_domain_request, empty, <<"DeleteDomainRequest">>), Options).

%% @doc 
-spec download_image(domain_pb:download_image_request()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
download_image(Input) ->
    download_image(ctx:new(), Input, #{}).

-spec download_image(ctx:t() | domain_pb:download_image_request(), domain_pb:download_image_request() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
download_image(Ctx, Input) when ?is_ctx(Ctx) ->
    download_image(Ctx, Input, #{});
download_image(Input, Options) ->
    download_image(ctx:new(), Input, Options).

-spec download_image(ctx:t(), domain_pb:download_image_request(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
download_image(Ctx, Input, Options) ->
    grpcbox_client:stream(Ctx, <<"/DomainService/DownloadImage">>, Input, ?DEF(download_image_request, image_chunk, <<"DownloadImageRequest">>), Options).

%% @doc Unary RPC
-spec get_network(domain_pb:get_network_request()) ->
    {ok, domain_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_network(Input) ->
    get_network(ctx:new(), Input, #{}).

-spec get_network(ctx:t() | domain_pb:get_network_request(), domain_pb:get_network_request() | grpcbox_client:options()) ->
    {ok, domain_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_network(Ctx, Input) when ?is_ctx(Ctx) ->
    get_network(Ctx, Input, #{});
get_network(Input, Options) ->
    get_network(ctx:new(), Input, Options).

-spec get_network(ctx:t(), domain_pb:get_network_request(), grpcbox_client:options()) ->
    {ok, domain_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_network(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/GetNetwork">>, Input, ?DEF(get_network_request, network, <<"GetNetworkRequest">>), Options).

%% @doc Unary RPC
-spec list_networks(domain_pb:list_networks_request()) ->
    {ok, domain_pb:list_networks_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_networks(Input) ->
    list_networks(ctx:new(), Input, #{}).

-spec list_networks(ctx:t() | domain_pb:list_networks_request(), domain_pb:list_networks_request() | grpcbox_client:options()) ->
    {ok, domain_pb:list_networks_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_networks(Ctx, Input) when ?is_ctx(Ctx) ->
    list_networks(Ctx, Input, #{});
list_networks(Input, Options) ->
    list_networks(ctx:new(), Input, Options).

-spec list_networks(ctx:t(), domain_pb:list_networks_request(), grpcbox_client:options()) ->
    {ok, domain_pb:list_networks_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_networks(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/ListNetworks">>, Input, ?DEF(list_networks_request, list_networks_response, <<"ListNetworksRequest">>), Options).

%% @doc Unary RPC
-spec create_network(domain_pb:create_network_request()) ->
    {ok, domain_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_network(Input) ->
    create_network(ctx:new(), Input, #{}).

-spec create_network(ctx:t() | domain_pb:create_network_request(), domain_pb:create_network_request() | grpcbox_client:options()) ->
    {ok, domain_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_network(Ctx, Input) when ?is_ctx(Ctx) ->
    create_network(Ctx, Input, #{});
create_network(Input, Options) ->
    create_network(ctx:new(), Input, Options).

-spec create_network(ctx:t(), domain_pb:create_network_request(), grpcbox_client:options()) ->
    {ok, domain_pb:network(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_network(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/CreateNetwork">>, Input, ?DEF(create_network_request, network, <<"CreateNetworkRequest">>), Options).

%% @doc Unary RPC
-spec delete_network(domain_pb:delete_network_request()) ->
    {ok, domain_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_network(Input) ->
    delete_network(ctx:new(), Input, #{}).

-spec delete_network(ctx:t() | domain_pb:delete_network_request(), domain_pb:delete_network_request() | grpcbox_client:options()) ->
    {ok, domain_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_network(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_network(Ctx, Input, #{});
delete_network(Input, Options) ->
    delete_network(ctx:new(), Input, Options).

-spec delete_network(ctx:t(), domain_pb:delete_network_request(), grpcbox_client:options()) ->
    {ok, domain_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_network(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/DomainService/DeleteNetwork">>, Input, ?DEF(delete_network_request, empty, <<"DeleteNetworkRequest">>), Options).

