import argparse
import logging
import os
import re
from concurrent import futures
from timeit import default_timer as timer

import grpc
import libvirt
from google.protobuf import empty_pb2
from grpc_reflection.v1alpha import reflection
from sqlalchemy import create_engine, event
from sqlalchemy.engine import Engine
from sqlalchemy.orm import scoped_session, sessionmaker
from sqlalchemy.pool import StaticPool

import daemon_pb2
import daemon_pb2_grpc
import dns_pb2_grpc
import domain_pb2
import domain_pb2_grpc
import port_forwarding_pb2_grpc
import route
import route_pb2_grpc
import volume_pb2_grpc
from daemon import DaemonService
from dns import DNSController, DNSService
from domain import DomainFacade, DomainService
from models import Base
from port_forwarding import (
    IPTablesPortForwardingSynchronizer,
    PortForwardingFacade,
    PortForwardingService,
)
from route import GenericRouteController, GenericRouteTableController, RouteService
from volume import VolumeFacade, VolumeService

libvirt.registerErrorHandler(lambda u, e: None, None)


class UnaryUnaryInterceptor(grpc.ServerInterceptor):
    def intercept_service(self, continuation, handler_call_details):
        next = continuation(handler_call_details)
        if next is None:
            return None
        if next.unary_unary is None:
            return next

        def letsgo(request, context):
            start = timer()
            try:
                response = next.unary_unary(request, context)
            except libvirt.libvirtError as e:
                status_code = grpc.StatusCode.INTERNAL
                if e.get_error_code() in [
                    libvirt.VIR_ERR_NO_DOMAIN,
                    libvirt.VIR_ERR_NO_STORAGE_VOL,
                ]:
                    status_code = grpc.StatusCode.NOT_FOUND
                context.set_code(status_code)
                context.set_details(f"{e} ({e.get_error_code()})")
                response = empty_pb2.Empty()

            logging.debug(f"{handler_call_details.method} [{(timer() - start)*1000:.3f} ms]")
            return response

        return grpc.unary_unary_rpc_method_handler(
            letsgo,
            request_deserializer=next.request_deserializer,
            response_serializer=next.response_serializer,
        )


def ensure_rfc1918_rules():
    import iptc
    from pyroute2 import IPSet

    with IPSet() as ips:
        try:
            ipset = ips.list("restvirt")[0]
        except:
            ips.create("restvirt", "hash:net")
            ipset = ips.list("restvirt")[0]

        addrs = ipset.get_attr("IPSET_ATTR_ADT").get_attrs("IPSET_ATTR_DATA")
        nets = set()
        for addr in addrs:
            ipv4 = addr.get_attr("IPSET_ATTR_IP_FROM").get_attr("IPSET_ATTR_IPADDR_IPV4")
            netbits = addr.get_attr("IPSET_ATTR_CIDR")
            nets.add(f"{ipv4}/{netbits}")

        rfc1918_nets = {"192.168.0.0/16", "172.16.0.0/12", "10.0.0.0/8"}
        for net in nets:
            if str(net) not in rfc1918_nets:
                ips.delete("restvirt", str(net), etype="net")
        for addr in rfc1918_nets:
            if addr not in nets:
                ips.add("restvirt", addr, etype="net")

    rule_exists = False
    for rule in iptc.easy.dump_chain("nat", "POSTROUTING"):
        if "set" in rule and rule["target"] == "MASQUERADE":
            if rule["set"] == [
                {"match-set": ["restvirt", "src"]},
                {"match-set": ["!", "restvirt", "dst"]},
            ]:
                rule_exists = True
                break

    if not rule_exists:
        iptc.easy.insert_rule(
            "nat",
            "POSTROUTING",
            {
                "set": [
                    {"match-set": ["restvirt", "src"]},
                    {"match-set": ["!", "restvirt", "dst"]},
                ],
                "target": "MASQUERADE",
            },
        )


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.DEBUG, format="%(asctime)s %(levelname)s:%(name)s:%(message)s"
    )

    p = re.compile(r"^(\S*):(\d+)$")

    def bind_address(s):
        match = p.match(s)
        if not match:
            raise argparse.ArgumentTypeError("invalid bind address: " + s)
        res = match.group(1), int(match.group(2))
        return res

    parser = argparse.ArgumentParser(
        description="restvirt", formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("--debug", action="store_true", help="run in debug mode")
    parser.add_argument(
        "-b", "--bind", type=bind_address, default=":8090", help="server bind address"
    )
    parser.add_argument("-c", "--config", default="/etc/restvirt", help="configuration folder")
    parser.add_argument("--server-cert")
    parser.add_argument("--server-key")
    parser.add_argument("--client-ca-cert")
    args = parser.parse_args()

    logging.debug(args)

    host, port = args.bind
    host = host or "0.0.0.0"

    if args.debug:
        logging.getLogger("sqlalchemy.engine").setLevel(logging.INFO)

    @event.listens_for(Engine, "connect")
    def set_sqlite_pragma(dbapi_connection, connection_record):
        cursor = dbapi_connection.cursor()
        cursor.execute("PRAGMA foreign_keys=ON")
        cursor.close()

    engine = create_engine(
        f"sqlite:///{os.path.join(args.config, 'controller.sqlite3')}",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
        future=True,
    )
    Base.metadata.create_all(engine)
    session_factory = scoped_session(sessionmaker(engine, future=True))

    ensure_rfc1918_rules()

    dns_controller = DNSController(session_factory)
    dns_controller.start()

    server = grpc.server(
        futures.ThreadPoolExecutor(max_workers=10), interceptors=[UnaryUnaryInterceptor()]
    )
    le_channel = grpc.insecure_channel("localhost:8095")
    dns_pb2_grpc.add_DNSServicer_to_server(DNSService(dns_controller), server)
    domain_pb2_grpc.add_DomainServiceServicer_to_server(DomainFacade(le_channel), server)
    port_forwarding_pb2_grpc.add_PortForwardingServiceServicer_to_server(
        PortForwardingFacade(le_channel), server
    )

    le_client = daemon_pb2_grpc.DaemonServiceStub(le_channel)

    class RouteSyncEventHandler(route.SyncEventHandler):
        def handle_sync(self, session):
            le_client.SyncRoutes(daemon_pb2.SyncRoutesRequest())

    sync_handler = RouteSyncEventHandler()

    route_table_controller = GenericRouteTableController(session_factory, sync_handler)
    route_controller = GenericRouteController(session_factory, sync_handler)
    route_pb2_grpc.add_RouteServiceServicer_to_server(
        RouteService(route_table_controller, route_controller), server
    )
    volume_pb2_grpc.add_VolumeServiceServicer_to_server(VolumeFacade(le_channel), server)

    server_key_pair_provided = args.server_cert is not None and args.server_key is not None
    assert server_key_pair_provided or (args.server_cert is None and args.server_key is None)
    assert args.client_ca_cert is None or server_key_pair_provided

    if server_key_pair_provided:
        with open(args.server_cert, "rb") as cert, open(args.server_key, "rb") as key:
            key_pair = (key.read(), cert.read())

        root_certificate = None
        require_client_auth = args.client_ca_cert is not None
        if require_client_auth:
            with open(args.client_ca_cert, "rb") as ca_cert:
                root_certificate = ca_cert.read()

        creds = grpc.ssl_server_credentials(
            [key_pair],
            root_certificates=root_certificate,
            require_client_auth=require_client_auth,
        )

        server.add_secure_port(f"{host}:{port}", creds)
    else:
        server.add_insecure_port(f"{host}:{port}")
    server.add_insecure_port("localhost:8094")
    reflection.enable_server_reflection(
        [
            service_descriptor.full_name
            for service_descriptor in domain_pb2.DESCRIPTOR.services_by_name.values()
        ]
        + [reflection.SERVICE_NAME],
        server,
    )
    server.start()

    # Setup and start daemon server
    daemon = grpc.server(
        futures.ThreadPoolExecutor(max_workers=10), interceptors=[UnaryUnaryInterceptor()]
    )
    daemon.add_insecure_port("localhost:8095")
    daemon_pb2_grpc.add_DaemonServiceServicer_to_server(
        DaemonService(grpc.insecure_channel("localhost:8094")), daemon
    )
    domain_pb2_grpc.add_DomainServiceServicer_to_server(DomainService(), daemon)
    volume_pb2_grpc.add_VolumeServiceServicer_to_server(VolumeService(), daemon)
    port_forwarding_service = PortForwardingService(
        session_factory, IPTablesPortForwardingSynchronizer()
    )
    port_forwarding_service.sync()
    port_forwarding_pb2_grpc.add_PortForwardingServiceServicer_to_server(
        port_forwarding_service, daemon
    )
    daemon.start()

    server.wait_for_termination()
    daemon.wait_for_termination()
