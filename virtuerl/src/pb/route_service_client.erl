%%%-------------------------------------------------------------------
%% @doc Client module for grpc service RouteService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(route_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'RouteService').
-define(PROTO_MODULE, 'route_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec get_route_table(route_pb:route_table_identifier()) ->
    {ok, route_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route_table(Input) ->
    get_route_table(ctx:new(), Input, #{}).

-spec get_route_table(ctx:t() | route_pb:route_table_identifier(), route_pb:route_table_identifier() | grpcbox_client:options()) ->
    {ok, route_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route_table(Ctx, Input) when ?is_ctx(Ctx) ->
    get_route_table(Ctx, Input, #{});
get_route_table(Input, Options) ->
    get_route_table(ctx:new(), Input, Options).

-spec get_route_table(ctx:t(), route_pb:route_table_identifier(), grpcbox_client:options()) ->
    {ok, route_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route_table(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/GetRouteTable">>, Input, ?DEF(route_table_identifier, route_table, <<"RouteTableIdentifier">>), Options).

%% @doc Unary RPC
-spec list_route_tables(route_pb:list_route_tables_request()) ->
    {ok, route_pb:list_route_tables_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_route_tables(Input) ->
    list_route_tables(ctx:new(), Input, #{}).

-spec list_route_tables(ctx:t() | route_pb:list_route_tables_request(), route_pb:list_route_tables_request() | grpcbox_client:options()) ->
    {ok, route_pb:list_route_tables_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_route_tables(Ctx, Input) when ?is_ctx(Ctx) ->
    list_route_tables(Ctx, Input, #{});
list_route_tables(Input, Options) ->
    list_route_tables(ctx:new(), Input, Options).

-spec list_route_tables(ctx:t(), route_pb:list_route_tables_request(), grpcbox_client:options()) ->
    {ok, route_pb:list_route_tables_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_route_tables(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/ListRouteTables">>, Input, ?DEF(list_route_tables_request, list_route_tables_response, <<"ListRouteTablesRequest">>), Options).

%% @doc Unary RPC
-spec create_route_table(route_pb:create_route_table_request()) ->
    {ok, route_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_route_table(Input) ->
    create_route_table(ctx:new(), Input, #{}).

-spec create_route_table(ctx:t() | route_pb:create_route_table_request(), route_pb:create_route_table_request() | grpcbox_client:options()) ->
    {ok, route_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_route_table(Ctx, Input) when ?is_ctx(Ctx) ->
    create_route_table(Ctx, Input, #{});
create_route_table(Input, Options) ->
    create_route_table(ctx:new(), Input, Options).

-spec create_route_table(ctx:t(), route_pb:create_route_table_request(), grpcbox_client:options()) ->
    {ok, route_pb:route_table(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
create_route_table(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/CreateRouteTable">>, Input, ?DEF(create_route_table_request, route_table, <<"CreateRouteTableRequest">>), Options).

%% @doc Unary RPC
-spec delete_route_table(route_pb:route_table_identifier()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route_table(Input) ->
    delete_route_table(ctx:new(), Input, #{}).

-spec delete_route_table(ctx:t() | route_pb:route_table_identifier(), route_pb:route_table_identifier() | grpcbox_client:options()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route_table(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_route_table(Ctx, Input, #{});
delete_route_table(Input, Options) ->
    delete_route_table(ctx:new(), Input, Options).

-spec delete_route_table(ctx:t(), route_pb:route_table_identifier(), grpcbox_client:options()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route_table(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/DeleteRouteTable">>, Input, ?DEF(route_table_identifier, empty, <<"RouteTableIdentifier">>), Options).

%% @doc Unary RPC
-spec get_route(route_pb:route_identifier()) ->
    {ok, route_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route(Input) ->
    get_route(ctx:new(), Input, #{}).

-spec get_route(ctx:t() | route_pb:route_identifier(), route_pb:route_identifier() | grpcbox_client:options()) ->
    {ok, route_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route(Ctx, Input) when ?is_ctx(Ctx) ->
    get_route(Ctx, Input, #{});
get_route(Input, Options) ->
    get_route(ctx:new(), Input, Options).

-spec get_route(ctx:t(), route_pb:route_identifier(), grpcbox_client:options()) ->
    {ok, route_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
get_route(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/GetRoute">>, Input, ?DEF(route_identifier, route, <<"RouteIdentifier">>), Options).

%% @doc Unary RPC
-spec list_routes(route_pb:list_routes_request()) ->
    {ok, route_pb:list_routes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_routes(Input) ->
    list_routes(ctx:new(), Input, #{}).

-spec list_routes(ctx:t() | route_pb:list_routes_request(), route_pb:list_routes_request() | grpcbox_client:options()) ->
    {ok, route_pb:list_routes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_routes(Ctx, Input) when ?is_ctx(Ctx) ->
    list_routes(Ctx, Input, #{});
list_routes(Input, Options) ->
    list_routes(ctx:new(), Input, Options).

-spec list_routes(ctx:t(), route_pb:list_routes_request(), grpcbox_client:options()) ->
    {ok, route_pb:list_routes_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
list_routes(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/ListRoutes">>, Input, ?DEF(list_routes_request, list_routes_response, <<"ListRoutesRequest">>), Options).

%% @doc Unary RPC
-spec put_route(route_pb:put_route_request()) ->
    {ok, route_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_route(Input) ->
    put_route(ctx:new(), Input, #{}).

-spec put_route(ctx:t() | route_pb:put_route_request(), route_pb:put_route_request() | grpcbox_client:options()) ->
    {ok, route_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_route(Ctx, Input) when ?is_ctx(Ctx) ->
    put_route(Ctx, Input, #{});
put_route(Input, Options) ->
    put_route(ctx:new(), Input, Options).

-spec put_route(ctx:t(), route_pb:put_route_request(), grpcbox_client:options()) ->
    {ok, route_pb:route(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
put_route(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/PutRoute">>, Input, ?DEF(put_route_request, route, <<"PutRouteRequest">>), Options).

%% @doc Unary RPC
-spec delete_route(route_pb:route_identifier()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route(Input) ->
    delete_route(ctx:new(), Input, #{}).

-spec delete_route(ctx:t() | route_pb:route_identifier(), route_pb:route_identifier() | grpcbox_client:options()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_route(Ctx, Input, #{});
delete_route(Input, Options) ->
    delete_route(ctx:new(), Input, Options).

-spec delete_route(ctx:t(), route_pb:route_identifier(), grpcbox_client:options()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
delete_route(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/DeleteRoute">>, Input, ?DEF(route_identifier, empty, <<"RouteIdentifier">>), Options).

%% @doc Unary RPC
-spec sync(route_pb:sync_request()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
sync(Input) ->
    sync(ctx:new(), Input, #{}).

-spec sync(ctx:t() | route_pb:sync_request(), route_pb:sync_request() | grpcbox_client:options()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
sync(Ctx, Input) when ?is_ctx(Ctx) ->
    sync(Ctx, Input, #{});
sync(Input, Options) ->
    sync(ctx:new(), Input, Options).

-spec sync(ctx:t(), route_pb:sync_request(), grpcbox_client:options()) ->
    {ok, route_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
sync(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/RouteService/Sync">>, Input, ?DEF(sync_request, empty, <<"SyncRequest">>), Options).

