import ipaddress
import threading

from google.protobuf import empty_pb2
from pyroute2 import IPRoute
from pyroute2.netlink.rtnl import rtypes

import daemon_pb2_grpc
import route_pb2
import route_pb2_grpc


class IPRouteTableSynchronizer:
    id_range_min, id_range_max = 30069, 30169

    def __init__(self):
        self.lock = threading.Lock()

    def handle_sync(self, client: route_pb2_grpc.RouteServiceStub):
        with self.lock, IPRoute() as ip:
            tables = client.ListRouteTables(route_pb2.ListRouteTablesRequest()).route_tables
            table_ids = {t.id for t in tables}

            def filt(r):
                prio = r.get_attr("FRA_PRIORITY")
                return prio is not None and self.id_range_min <= prio <= self.id_range_max

            rules = filter(filt, ip.get_rules())
            rules = {r.get_attr("FRA_PRIORITY"): r for r in rules}

            # step 1: delete tables not in conf
            for prio in rules:
                if prio not in table_ids:
                    ip.rule("del", priority=prio)

            # step 2: update tables
            # TODO: this is broken
            for table in tables:
                if table.id not in rules or rules[table.id]["table"] != table.id:
                    try:
                        ip.rule("del", priority=table.id)
                    except:
                        pass
                    ip.rule(
                        "add",
                        priority=table.id,
                        table=table.id,
                    )


# TODO: RouteTableController
# try:
#     # FIXME: make "virbr0" configurable
#     ip.rule(
#         "add", table=TABLE_ID, priority=30069, iifname="virbr0"
#     )  # FIXME: check if rule identical
# except NetlinkError as e:
#     if e.code != 17:  # 17 = rule already exists
#         raise e


class IPRouteSynchronizer:
    id_range_min, id_range_max = 30069, 30169

    def __init__(self):
        self.lock = threading.Lock()

    def get_ip_routes(self, ip):
        filtered_routes = {}
        for r in ip.get_routes():
            table_id = r.get_attr("RTA_TABLE")
            if not self.id_range_min <= table_id < self.id_range_max:
                continue
            if r["family"] != 2 or r["type"] != rtypes["RTN_UNICAST"]:
                continue
            dst = r.get_attr("RTA_DST")
            if dst is None:
                continue
            dstnet = f"{dst}/{r['dst_len']}"
            addr = ipaddress.ip_network(dstnet)

            gateway = r.get_attr("RTA_GATEWAY")
            if gateway is not None:
                gateways = [gateway]
            else:
                routes = r.get_attr("RTA_MULTIPATH")
                gateways = [r.get_attr("RTA_GATEWAY") for r in routes]
            filtered_routes[addr] = (table_id, {ipaddress.ip_address(gw) for gw in gateways})

        return filtered_routes

    def remove_ip_route(self, ip, table_id, dst):
        ip.route("del", table=table_id, dst=str(dst))

    def put_ip_route(self, ip, table_id, dst, gws):
        try:
            ip.route("del", table=table_id, dst=str(dst))
        except:
            pass
        ip.route(
            "add",
            table=table_id,
            dst=str(dst),
            multipath=[{"gateway": str(gw)} for gw in gws],
        )

    def handle_sync(self, client: route_pb2_grpc.RouteServiceStub):
        with self.lock, IPRoute() as ip:
            tables = client.ListRouteTables(route_pb2.ListRouteTablesRequest()).route_tables
            routes = [
                client.ListRoutes(route_pb2.ListRoutesRequest(route_table_id=table.id)).routes
                for table in tables
            ]
            routes = [r for rs in routes for r in rs]
            routes = {
                ipaddress.IPv4Network(route.destination): (
                    route.route_table_id,
                    {ipaddress.IPv4Address(gw) for gw in route.gateways},
                )
                for route in routes
            }

            filtered_routes = self.get_ip_routes(ip)

            # step 1: delete routes not in conf
            for dst, (tid, gws) in filtered_routes.items():
                if dst not in routes:
                    self.remove_ip_route(ip, tid, dst)

            # step 2: update routes
            for dst, (tid, gws) in routes.items():
                if dst not in filtered_routes or filtered_routes[dst][1] != gws:
                    self.put_ip_route(ip, tid, dst, gws)


class DaemonService(daemon_pb2_grpc.DaemonServiceServicer):
    def __init__(self, channel):
        self.client = route_pb2_grpc.RouteServiceStub(channel)
        self.tables_synchronizer = IPRouteTableSynchronizer()
        self.routes_synchronizer = IPRouteSynchronizer()

    def SyncRoutes(self, request, context):
        self.tables_synchronizer.handle_sync(self.client)
        self.routes_synchronizer.handle_sync(self.client)
        return empty_pb2.Empty()
