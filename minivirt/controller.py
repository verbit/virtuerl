import grpc
from google.protobuf import empty_pb2
from grpc import StatusCode

from minivirt import (
    controller_pb2_grpc,
    daemon_pb2,
    daemon_pb2_grpc,
    dns_pb2,
    dns_pb2_grpc,
    domain_pb2,
    network,
    route_pb2,
    route_pb2_grpc,
)
from minivirt.host import HostController
from minivirt.models import DNSRecord, Host
from minivirt.network import GenericNetworkController
from minivirt.route import GenericRouteController, GenericRouteTableController, SyncEventHandler


class ControllerSyncHandler(SyncEventHandler):
    def __init__(self, host_controller):
        self.host_controller = host_controller

    def handle_sync(self, session):
        channels = self.host_controller.channels()
        for channel in channels:
            client = daemon_pb2_grpc.DaemonServiceStub(channel)
            client.SyncRoutes(daemon_pb2.SyncRoutesRequest())


class NetworkSyncHandler(network.SyncEventHandler):
    def __init__(self, host_controller):
        self.host_controller = host_controller

    def handle_sync(self):
        channels = self.host_controller.channels()
        for channel in channels:
            client = daemon_pb2_grpc.DaemonServiceStub(channel)
            client.SyncNetworks(daemon_pb2.SyncNetworksRequest())


class Controller(
    controller_pb2_grpc.ControllerServiceServicer,
    dns_pb2_grpc.DNSServicer,
    route_pb2_grpc.RouteServiceServicer,
):
    def __init__(self, session_factory, host_controller: HostController, dns_controller):
        self.session_factory = session_factory
        self.host_controller = host_controller
        self.dns_controller = dns_controller
        sync_handler = ControllerSyncHandler(host_controller)
        network_sync_handler = NetworkSyncHandler(host_controller)
        self.route_table_controller = GenericRouteTableController(session_factory, sync_handler)
        self.route_controller = GenericRouteController(session_factory, sync_handler)
        self.network_controller = GenericNetworkController(session_factory, network_sync_handler)
        self.channel_cache = {}

    def _get_daemon_client(self, hostname=None):
        if not hostname:
            hostname = "default"
        return daemon_pb2_grpc.DaemonServiceStub(self.host_controller.channel(hostname))

    def GetDNSRecord(self, request, context):
        record = self.dns_controller.record(request.name, request.type)
        if record is None:
            context.set_code(StatusCode.NOT_FOUND)
            return empty_pb2.Empty()
        return dns_pb2.DNSRecord(
            name=record.name,
            type=record.type,
            ttl=record.ttl,
            records=record.records,
        )

    def ListDNSRecords(self, request, context):
        return dns_pb2.ListDNSRecordsResponse(
            dns_records=[
                dns_pb2.DNSRecord(
                    name=record.name,
                    type=record.type,
                    ttl=record.ttl,
                    records=record.records,
                )
                for record in self.dns_controller.records()
            ]
        )

    def PutDNSRecord(self, request, context):
        record = request.dns_record
        self.dns_controller.set(
            DNSRecord(
                name=record.name,
                type=record.type,
                ttl=record.ttl,
                records=record.records,
            )
        )
        return record

    def DeleteDNSRecord(self, request, context):
        self.dns_controller.remove(request.name, request.type)
        return empty_pb2.Empty()

    def GetRouteTable(self, request, context):
        table = self.route_table_controller.route_table(request.id)
        if table is None:
            context.set_code(StatusCode.NOT_FOUND)
            return empty_pb2.Empty()

        return route_pb2.RouteTable(
            id=table.id,
            name=table.name,
        )

    def ListRouteTables(self, request, context):
        tables = self.route_table_controller.route_tables()

        return route_pb2.ListRouteTablesResponse(
            route_tables=[
                route_pb2.RouteTable(
                    id=table.id,
                    name=table.name,
                )
                for table in tables
            ]
        )

    def CreateRouteTable(self, request, context):
        table = self.route_table_controller.create_route_table(request.route_table)
        return table

    def DeleteRouteTable(self, request, context):
        self.route_table_controller.remove_route_table(request.id)
        return empty_pb2.Empty()

    def GetRoute(self, request, context):
        route = self.route_controller.route(request.route_table_id, request.destination)
        return route_pb2.Route(
            route_table_id=route.route_table_id,
            destination=route.destination,
            gateways=route.gateways,
        )

    def ListRoutes(self, request, context):
        routes = self.route_controller.routes(request.route_table_id)

        return route_pb2.ListRoutesResponse(
            routes=[
                route_pb2.Route(
                    route_table_id=route.route_table_id,
                    destination=route.destination,
                    gateways=route.gateways,
                )
                for route in routes
            ]
        )

    def PutRoute(self, request, context):
        route = self.route_controller.put_route(request.route)
        return route

    def DeleteRoute(self, request, context):
        self.route_controller.remove_route(request.route_table_id, request.destination)
        return empty_pb2.Empty()

    def SyncRoutes(self, request, context):
        self.route_table_controller.sync()
        self.route_controller.sync()
        return empty_pb2.Empty()

    def GetNetwork(self, request, context):
        n = self.network_controller.network(request.uuid)
        return domain_pb2.Network(uuid=n.id, name=n.id, cidr=n.cidr)

    def ListNetworks(self, request, context):
        networks = self.network_controller.networks()
        return domain_pb2.ListNetworksResponse(
            networks=[domain_pb2.Network(uuid=n.id, name=n.id, cidr=n.cidr) for n in networks]
        )

    def CreateNetwork(self, request, context):
        n = request.network
        self.network_controller.put_network(n.name, n.cidr)
        return domain_pb2.Network(uuid=n.name, name=n.name, cidr=n.cidr)

    def DeleteNetwork(self, request, context):
        self.network_controller.remove_network(request.uuid)
        return empty_pb2.Empty()

    def Sync(self, request, context):
        # TODO: implement or remove
        return super().Sync(request, context)


def monkeypatch_controller():
    cls = Controller
    super_methods = {
        getattr(base, a) for base in cls.__bases__ for a in dir(base) if not a.startswith("__")
    }
    sub_methods = {getattr(cls, a) for a in dir(cls) if not a.startswith("__")}

    for method in super_methods & sub_methods:
        method_name = method.__name__

        def create_target_method(method_name):
            def target(self, request, context):
                client = self._get_daemon_client(request.host)
                try:
                    to_call = getattr(client, method_name)
                    if isinstance(to_call, grpc.UnaryUnaryMultiCallable):
                        return to_call(request)
                    elif isinstance(to_call, grpc.UnaryStreamMultiCallable):
                        return (chunk for chunk in to_call(request))
                    else:
                        assert False  # TODO
                except grpc.RpcError as e:
                    context.set_code(e.code())
                    context.set_details(e.details())
                    return empty_pb2.Empty()

            return target

        setattr(cls, method_name, create_target_method(method_name))


monkeypatch_controller()