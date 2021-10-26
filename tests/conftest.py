import logging
import sys
from concurrent import futures

import grpc
import pytest
from sqlalchemy import create_engine, event
from sqlalchemy.engine import Engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import StaticPool

import route_pb2
import route_pb2_grpc
from models import Base
from route import RouteService, SyncEventHandler

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
