import grpc
from google.protobuf import empty_pb2
from grpc import StatusCode

import controller_pb2_grpc
import daemon_pb2_grpc
import dns_pb2
import dns_pb2_grpc
import route_pb2
import route_pb2_grpc
from models import DNSRecord


class Controller(
    controller_pb2_grpc.ControllerServiceServicer,
    dns_pb2_grpc.DNSServicer,
    route_pb2_grpc.RouteServiceServicer,
):
    def __init__(self, channel, dns_controller, route_table_controller, route_controller):
        self.client = daemon_pb2_grpc.DaemonServiceStub(channel)
        self.dns_controller = dns_controller
        self.route_table_controller = route_table_controller
        self.route_controller = route_controller

    def GetDNSRecord(self, request, context):
        record = self.dns_controller.record(request.name, request.type)
        if record is None:
            context.set_code(StatusCode.NOT_FOUND)
            return
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
        try:
            return self.client.GetNetwork(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def ListNetworks(self, request, context):
        try:
            return self.client.ListNetworks(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def CreateNetwork(self, request, context):
        try:
            return self.client.CreateNetwork(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def DeleteNetwork(self, request, context):
        try:
            return self.client.DeleteNetwork(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def GetDomain(self, request, context):
        try:
            return self.client.GetDomain(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def ListDomains(self, request, context):
        try:
            return self.client.ListDomains(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def CreateDomain(self, request, context):
        try:
            return self.client.CreateDomain(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def DeleteDomain(self, request, context):
        try:
            return self.client.DeleteDomain(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def DownloadImage(self, request, context):
        try:
            for chunk in self.client.DownloadImage(request):
                yield chunk
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def GetVolume(self, request, context):
        try:
            return self.client.GetVolume(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def ListVolumes(self, request, context):
        try:
            return self.client.ListVolumes(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def CreateVolume(self, request, context):
        try:
            return self.client.CreateVolume(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def DeleteVolume(self, request, context):
        try:
            return self.client.DeleteVolume(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def ListVolumeAttachments(self, request, context):
        try:
            return self.client.ListVolumeAttachments(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def GetVolumeAttachment(self, request, context):
        try:
            return self.client.GetVolumeAttachment(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def AttachVolume(self, request, context):
        try:
            return self.client.AttachVolume(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def DetachVolume(self, request, context):
        try:
            return self.client.DetachVolume(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def GetPortForwarding(self, request, context):
        try:
            return self.client.GetPortForwarding(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def ListPortForwardings(self, request, context):
        try:
            return self.client.ListPortForwardings(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def PutPortForwarding(self, request, context):
        try:
            return self.client.PutPortForwarding(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())

    def DeletePortForwarding(self, request, context):
        try:
            return self.client.DeletePortForwarding(request)
        except grpc.RpcError as e:
            context.set_code(e.code())
            context.set_status(e.status())
