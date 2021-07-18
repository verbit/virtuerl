import ipaddress
import json
import os
import threading
from dataclasses import asdict, dataclass, is_dataclass
from json import JSONEncoder
from typing import Any, Dict, Set

from pyroute2 import IPRoute, NetlinkError
from pyroute2.netlink.rtnl import rtypes


@dataclass
class RouteConfig:
    gateways: Set[ipaddress.IPv4Address]
    namespace: str


RoutesState = Dict[ipaddress.IPv4Network, RouteConfig]
AliasIPConf = Dict[ipaddress.IPv4Network, Set[ipaddress.IPv4Address]]

TABLE_ID = 69


class RoutesJSONEncoder(JSONEncoder):
    def default(self, o: Any) -> Any:
        if isinstance(o, ipaddress.IPv4Address) or isinstance(o, ipaddress.IPv4Network):
            return str(o)
        if is_dataclass(o):
            return asdict(o)
        return super().default(o)


def get_routes(ip: IPRoute) -> AliasIPConf:
    filtered_routes = {}
    for r in ip.get_routes(table=TABLE_ID):
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
        filtered_routes[addr] = {ipaddress.ip_address(gw) for gw in gateways}

    return filtered_routes


class AliasIPController:
    def __init__(self, state_dir, state_file_name="routes.json"):
        self.lock = threading.Lock()
        self.state_dir = state_dir
        self.state_file_path = os.path.join(state_dir, state_file_name)

    def _update_state_file(self, routes: RoutesState):
        with open(self.state_file_path, mode="w") as f:
            f.write(
                json.dumps(
                    {
                        str(k): {
                            "gateways": [str(gw) for gw in r.gateways],
                            "namespace": r.namespace,
                        }
                        for k, r in routes.items()
                    }
                )
            )
        self._sync(routes)

    def _read_state_file(self) -> RoutesState:
        if not os.path.isfile(self.state_file_path):
            return {}

        with open(self.state_file_path) as f:
            routes = json.load(f)
            return {
                ipaddress.ip_network(dst): RouteConfig(
                    gateways={ipaddress.ip_address(gw) for gw in d["gateways"]},
                    namespace=d["namespace"],
                )
                for dst, d in routes.items()
            }

    def set(self, dst, gateways, namespace):
        with self.lock:
            routes = self._read_state_file()
            routes[dst] = RouteConfig(gateways=gateways, namespace=namespace)
            self._update_state_file(routes)

    def remove(self, dst):
        with self.lock:
            routes = self._read_state_file()
            try:
                del routes[dst]
                self._update_state_file(routes)
            except ValueError:
                pass

    def get_routes(self, namespace=None) -> RoutesState:
        with self.lock:
            routes = self._read_state_file()
        if namespace is not None:
            routes = {dst: r for dst, r in routes.items() if r.namespace == namespace}
        return routes

    def get_route(self, dst) -> RouteConfig:
        routes = self.get_routes()
        return routes.get(dst)

    def _sync(self, route_confs: RoutesState):
        with IPRoute() as ip:
            filtered_routes = get_routes(ip)

            # step 1: delete routes not in conf
            for dst in filtered_routes:
                if dst not in route_confs:
                    ip.route("del", table=TABLE_ID, dst=str(dst))

            # step 2: update routes
            for dst, r in route_confs.items():
                if dst not in filtered_routes or filtered_routes[dst] != r.gateways:
                    try:
                        ip.route("del", table=TABLE_ID, dst=str(dst))
                    except:
                        pass
                    ip.route(
                        "add",
                        table=TABLE_ID,
                        dst=str(dst),
                        multipath=[{"gateway": str(gw)} for gw in r.gateways],
                    )
            try:
                # FIXME: make "virbr0" configurable
                ip.rule(
                    "add", table=TABLE_ID, priority=30069, iifname="virbr0"
                )  # FIXME: check if rule identical
            except NetlinkError as e:
                if e.code != 17:  # 17 = rule already exists
                    raise e

    def sync(self):
        with self.lock:
            routes = self._read_state_file()
            self._sync(routes)
