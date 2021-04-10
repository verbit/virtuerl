import ipaddress
import json
import os
import threading
from typing import Dict, List

from pyroute2 import IPRoute
from pyroute2.netlink.rtnl import rtypes

# FIXME: make value list a set
AliasIPConf = Dict[ipaddress.IPv4Network, List[ipaddress.IPv4Address]]


def get_routes(ip: IPRoute, subnet: ipaddress.IPv4Network) -> AliasIPConf:
    filtered_routes = {}
    for r in ip.get_routes():
        if r["family"] != 2 or r["type"] != rtypes["RTN_UNICAST"]:
            continue
        dst = r.get_attr("RTA_DST")
        if dst is None:
            continue
        dstnet = f"{dst}/{r['dst_len']}"
        addr = ipaddress.ip_network(dstnet)
        if not addr.subnet_of(subnet):
            continue

        gateway = r.get_attr("RTA_GATEWAY")
        if gateway is not None:
            gateways = [gateway]
        else:
            routes = r.get_attr("RTA_MULTIPATH")
            gateways = [r.get_attr("RTA_GATEWAY") for r in routes]
        filtered_routes[addr] = [ipaddress.ip_address(gw) for gw in gateways]

    return filtered_routes


class AliasIPController:
    def __init__(self, state_dir, state_file_name="routes.json"):
        self.lock = threading.Lock()
        self.state_dir = state_dir
        self.state_file_path = os.path.join(state_dir, state_file_name)
        self.cidr = ipaddress.ip_network("10.0.0.0/8")  # FIXME: make configurable

    def _update_state_file(self, routes):
        with open(self.state_file_path, mode="w") as f:
            f.write(
                json.dumps({str(k): [str(gw) for gw in gateways] for k, gateways in routes.items()})
            )
        self._sync(routes)

    def _read_state_file(self) -> AliasIPConf:
        if not os.path.isfile(self.state_file_path):
            return {}

        with open(self.state_file_path) as f:
            routes = json.load(f)
            return {
                ipaddress.ip_network(dst): [ipaddress.ip_address(gw) for gw in gateways]
                for dst, gateways in routes.items()
            }

    def set(self, dst, gateways):
        with self.lock:
            routes = self._read_state_file()
            routes[dst] = gateways
            self._update_state_file(routes)

    def remove(self, dst):
        with self.lock:
            routes = self._read_state_file()
            try:
                del routes[dst]
            except ValueError:
                pass
            self._update_state_file(routes)

    def get_routes(self):
        with self.lock:
            return self._read_state_file()

    def get_route(self, dst):
        routes = self.get_routes()
        return routes.get(dst, [])

    def _sync(self, route_confs: AliasIPConf):
        with IPRoute() as ip:
            filtered_routes = get_routes(ip, self.cidr)

            # step 1: delete routes not in conf
            for dst in filtered_routes:
                if dst not in route_confs:
                    ip.route("del", dst=str(dst))

            # step 2: update routes
            for dst, gateways in route_confs.items():
                if dst not in filtered_routes or filtered_routes[dst] != gateways:
                    try:
                        ip.route("del", dst=str(dst))
                    except:
                        pass
                    ip.route(
                        "add", dst=str(dst), multipath=[{"gateway": str(gw)} for gw in gateways]
                    )

    def sync(self):
        self.lock.acquire()
        routes = self._read_state_file()
        self._sync(routes)
        self.lock.release()


if __name__ == "__main__":
    subnet = ipaddress.ip_network("10.0.0.0/8")
    with IPRoute() as ip:
        print(get_routes(ip, subnet))
