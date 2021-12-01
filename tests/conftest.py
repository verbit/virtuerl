import sys
from concurrent import futures

import grpc
import pytest
from google.protobuf import empty_pb2
from sqlalchemy import create_engine, event
from sqlalchemy.engine import Engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import StaticPool

import controller_pb2_grpc
import daemon_pb2_grpc
import host_pb2
import host_pb2_grpc
from controller import Controller
from dns import DNSController
from host import HostController, HostService
from models import Base
from port_forwarding import IPTablesPortForwardingSynchronizer

OPERATING_SYSTEMS = {"darwin", "linux", "windows"}


def pytest_addoption(parser):
    parser.addoption("--pool-dir", default="/data/restvirt")


@pytest.fixture(scope="session")
def pool_dir(pytestconfig):
    return pytestconfig.getoption("pool_dir")


def pytest_configure(config):
    for os in OPERATING_SYSTEMS:
        config.addinivalue_line("markers", f"{os}: mark tests to only run on {os} operating system")


def pytest_runtest_setup(item):
    supported_platforms = OPERATING_SYSTEMS.intersection(mark.name for mark in item.iter_markers())
    plat = sys.platform
    if supported_platforms and plat not in supported_platforms:
        pytest.skip("cannot run on platform {}".format(plat))


def pytest_collection_modifyitems(items):
    for item in items:
        os = item.nodeid.split("_")[-1]
        if os in OPERATING_SYSTEMS:
            item.add_marker(os)


@event.listens_for(Engine, "connect")
def set_sqlite_pragma(dbapi_connection, connection_record):
    cursor = dbapi_connection.cursor()
    cursor.execute("PRAGMA foreign_keys=ON")
    cursor.close()


@pytest.fixture
def engine():
    engine = create_engine(
        "sqlite:///:memory:",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
        echo=True,
        future=True,
    )
    Base.metadata.create_all(engine)
    yield engine
    Base.metadata.drop_all(engine)


@pytest.fixture
def session_factory(engine):
    return sessionmaker(engine, future=True)


@pytest.fixture
def dns_controller(session_factory):
    return DNSController(session_factory)


class DaemonDummy(daemon_pb2_grpc.DaemonServiceServicer):
    def SyncRoutes(self, request, context):
        return empty_pb2.Empty()


@pytest.fixture
def controller_channel(session_factory, dns_controller):
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    port = server.add_insecure_port("localhost:0")
    channel = grpc.insecure_channel(f"localhost:{port}")

    host_controller = HostController(session_factory)
    controller_pb2_grpc.add_ControllerServiceServicer_to_server(
        Controller(session_factory, host_controller, dns_controller),
        server,
    )
    host_pb2_grpc.add_HostServiceServicer_to_server(
        HostService(host_controller, session_factory), server
    )

    server.start()
    yield channel
    server.stop(1)


@pytest.fixture
def host_client(controller_channel):
    return host_pb2_grpc.HostServiceStub(controller_channel)


@pytest.fixture
def controller_client_dummy(controller_channel, host_client):
    daemon = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    daemon_port = daemon.add_insecure_port("localhost:0")

    daemon_pb2_grpc.add_DaemonServiceServicer_to_server(DaemonDummy(), daemon)

    token = host_client.CreateBootstrapToken(host_pb2.CreateBootstrapTokenRequest()).token
    host_client.Register(
        host_pb2.RegisterHostRequest(
            token=token,
            host=host_pb2.Host(
                name="test",
                address=f"localhost:{daemon_port}",
            ),
        )
    )

    daemon.start()
    yield controller_pb2_grpc.ControllerServiceStub(controller_channel)
    daemon.stop(1)


@pytest.fixture
def controller_client(session_factory, controller_channel, host_client):
    from daemon import DaemonService

    daemon = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    daemon_port = daemon.add_insecure_port("localhost:0")

    daemon_pb2_grpc.add_DaemonServiceServicer_to_server(
        DaemonService(session_factory, IPTablesPortForwardingSynchronizer(), controller_channel),
        daemon,
    )

    token = host_client.CreateBootstrapToken(host_pb2.CreateBootstrapTokenRequest()).token
    host_client.Register(
        host_pb2.RegisterHostRequest(
            token=token,
            host=host_pb2.Host(
                name="test",
                address=f"localhost:{daemon_port}",
            ),
        )
    )

    daemon.start()
    yield controller_pb2_grpc.ControllerServiceStub(controller_channel)
    daemon.stop(1)
