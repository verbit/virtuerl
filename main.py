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
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import StaticPool

import controller_pb2_grpc
import daemon_pb2_grpc
import dns_pb2_grpc
import domain_pb2
import domain_pb2_grpc
import host_pb2
import host_pb2_grpc
import port_forwarding_pb2_grpc
import route_pb2_grpc
import volume_pb2_grpc
from controller import Controller
from daemon import DaemonService
from dns import DNSController
from host import HostController, HostService
from models import Base
from port_forwarding import IPTablesPortForwardingSynchronizer

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


@event.listens_for(Engine, "connect")
def set_sqlite_pragma(dbapi_connection, connection_record):
    cursor = dbapi_connection.cursor()
    cursor.execute("PRAGMA foreign_keys=ON")
    cursor.close()


def start_controller(args):
    host, port = args.bind
    host = host or "0.0.0.0"

    if args.debug:
        logging.getLogger("sqlalchemy.engine").setLevel(logging.INFO)

    engine = create_engine(
        f"sqlite:///{os.path.join(args.config, 'controller.sqlite3')}",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
        future=True,
    )
    Base.metadata.create_all(engine)
    session_factory = sessionmaker(engine, future=True)

    dns_controller = DNSController(session_factory)
    dns_controller.start()

    server = grpc.server(
        futures.ThreadPoolExecutor(max_workers=10), interceptors=[UnaryUnaryInterceptor()]
    )

    host_controller = HostController(session_factory)
    controller = Controller(
        session_factory=session_factory,
        host_controller=host_controller,
        dns_controller=dns_controller,
    )
    controller_pb2_grpc.add_ControllerServiceServicer_to_server(controller, server)
    dns_pb2_grpc.add_DNSServicer_to_server(controller, server)
    domain_pb2_grpc.add_DomainServiceServicer_to_server(controller, server)
    port_forwarding_pb2_grpc.add_PortForwardingServiceServicer_to_server(controller, server)
    route_pb2_grpc.add_RouteServiceServicer_to_server(controller, server)
    volume_pb2_grpc.add_VolumeServiceServicer_to_server(controller, server)
    host_pb2_grpc.add_HostServiceServicer_to_server(
        HostService(host_controller, session_factory), server
    )

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
    server.wait_for_termination()


def start_daemon(args):
    addr, port = args.bind
    port = port or 0
    addr = addr or "0.0.0.0"

    if args.debug:
        logging.getLogger("sqlalchemy.engine").setLevel(logging.INFO)

    engine = create_engine(
        f"sqlite:///{os.path.join(args.config, 'daemon.sqlite3')}",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
        future=True,
    )
    Base.metadata.create_all(engine)
    session_factory = sessionmaker(engine, future=True)

    daemon = grpc.server(
        futures.ThreadPoolExecutor(max_workers=10), interceptors=[UnaryUnaryInterceptor()]
    )
    daemon_port = daemon.add_insecure_port(f"{addr}:{port}")
    controller_host, controller_port = args.controller
    controller_channel = grpc.insecure_channel(f"{controller_host}:{controller_port}")
    daemon_service = DaemonService(
        session_factory,
        IPTablesPortForwardingSynchronizer(),
        controller_channel,
    )
    daemon_pb2_grpc.add_DaemonServiceServicer_to_server(daemon_service, daemon)
    host_client = host_pb2_grpc.HostServiceStub(controller_channel)
    hosts = host_client.ListHosts(host_pb2.ListHostsRequest()).hosts
    for host in hosts:
        host_client.Deregister(host)
    token = host_client.CreateBootstrapToken(host_pb2.CreateBootstrapTokenRequest()).token
    host_client.Register(
        host_pb2.RegisterHostRequest(
            token=token,
            host=host_pb2.Host(
                name="default",
                address=f"{addr}:{daemon_port}",
            ),
        )
    )
    daemon_service.sync()
    daemon.start()
    daemon.wait_for_termination()


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
    subparsers = parser.add_subparsers()

    controller_parser = subparsers.add_parser("controller")
    controller_parser.add_argument("--debug", action="store_true", help="run in debug mode")
    controller_parser.add_argument(
        "-b", "--bind", type=bind_address, default=":8090", help="controller bind address"
    )
    controller_parser.add_argument(
        "-c", "--config", default="/etc/restvirt", help="configuration folder"
    )
    controller_parser.add_argument("--server-cert")
    controller_parser.add_argument("--server-key")
    controller_parser.add_argument("--client-ca-cert")
    controller_parser.set_defaults(func=start_controller)

    daemon_parser = subparsers.add_parser("daemon")
    daemon_parser.add_argument("--debug", action="store_true", help="run in debug mode")
    daemon_parser.add_argument(
        "-b", "--bind", type=bind_address, default="localhost:8099", help="daemon bind address"
    )
    daemon_parser.add_argument(
        "-a", "--controller", type=bind_address, default="localhost:8094", help="controller address"
    )
    daemon_parser.add_argument(
        "-c", "--config", default="/etc/restvirt", help="configuration folder"
    )
    daemon_parser.add_argument("--server-cert")
    daemon_parser.add_argument("--server-key")
    daemon_parser.add_argument("--client-ca-cert")
    daemon_parser.set_defaults(func=start_daemon)

    args = parser.parse_args()
    logging.debug(args)
    args.func(args)
