import sys
from concurrent import futures

import grpc
import pytest
from sqlalchemy import create_engine, event
from sqlalchemy.engine import Engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import StaticPool

import controller_pb2_grpc
import daemon_pb2
import daemon_pb2_grpc
from controller import Controller
from dns import DNSController
from models import Base
from port_forwarding import IPTablesPortForwardingSynchronizer
from route import GenericRouteController, GenericRouteTableController, SyncEventHandler

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
    pass


@pytest.fixture
def controller_client_dummy(session_factory, dns_controller):
    daemon = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    daemon_port = daemon.add_insecure_port("localhost:0")
    daemon_channel = grpc.insecure_channel(f"localhost:{daemon_port}")

    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    port = server.add_insecure_port("localhost:0")
    channel = grpc.insecure_channel(f"localhost:{port}")

    daemon_pb2_grpc.add_DaemonServiceServicer_to_server(DaemonDummy(), daemon)

    route_table_controller = GenericRouteTableController(session_factory)
    route_controller = GenericRouteController(session_factory)
    controller_pb2_grpc.add_ControllerServiceServicer_to_server(
        Controller(daemon_channel, dns_controller, route_table_controller, route_controller), server
    )

    daemon.start()
    server.start()

    yield controller_pb2_grpc.ControllerServiceStub(channel)

    server.stop(1)
    daemon.stop(1)


@pytest.fixture
def controller_client(session_factory, dns_controller):
    from daemon import DaemonService

    daemon = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    daemon_port = daemon.add_insecure_port("localhost:0")
    daemon_channel = grpc.insecure_channel(f"localhost:{daemon_port}")

    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    port = server.add_insecure_port("localhost:0")
    channel = grpc.insecure_channel(f"localhost:{port}")

    daemon_pb2_grpc.add_DaemonServiceServicer_to_server(
        DaemonService(session_factory, IPTablesPortForwardingSynchronizer(), channel), daemon
    )

    daemon_client = daemon_pb2_grpc.DaemonServiceStub(daemon_channel)

    class RouteSyncEventHandler(SyncEventHandler):
        def handle_sync(self, session):
            daemon_client.SyncRoutes(daemon_pb2.SyncRoutesRequest())

    sync_handler = RouteSyncEventHandler()
    route_table_controller = GenericRouteTableController(session_factory, sync_handler)
    route_controller = GenericRouteController(session_factory, sync_handler)
    controller_pb2_grpc.add_ControllerServiceServicer_to_server(
        Controller(daemon_channel, dns_controller, route_table_controller, route_controller), server
    )

    daemon.start()
    server.start()

    yield controller_pb2_grpc.ControllerServiceStub(channel)

    server.stop(1)
    daemon.stop(1)
