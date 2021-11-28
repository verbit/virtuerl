from concurrent import futures
from datetime import datetime, timedelta

import grpc
import pytest
from conftest import DaemonDummy

import controller_pb2_grpc
import daemon_pb2_grpc
import host_pb2
import host_pb2_grpc
from controller import Controller
from host import HostService
from route import GenericRouteController, GenericRouteTableController


@pytest.fixture
def client(session_factory, dns_controller):
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
    host_pb2_grpc.add_HostServiceServicer_to_server(HostService(session_factory), server)

    daemon.start()
    server.start()

    yield host_pb2_grpc.HostServiceStub(channel)

    server.stop(1)
    daemon.stop(1)


def test_register(client: host_pb2_grpc.HostServiceStub):
    assert len(client.ListHosts(host_pb2.ListHostsRequest()).hosts) == 0

    token = client.CreateBootstrapToken(host_pb2.CreateBootstrapTokenRequest()).token
    client.Register(
        host_pb2.RegisterHostRequest(
            token=token,
            host=host_pb2.Host(
                name="yourmom",
                address="localhost:8333",
            ),
        )
    )
    hosts = client.ListHosts(host_pb2.ListHostsRequest()).hosts
    assert len(hosts) == 1
    host = hosts[0]
    assert host.address == "localhost:8333"

    client.Deregister(host_pb2.Host(name="yourmom"))
    assert len(client.ListHosts(host_pb2.ListHostsRequest()).hosts) == 0


def test_register_expired(client: host_pb2_grpc.HostServiceStub):
    token = client.CreateBootstrapToken(
        host_pb2.CreateBootstrapTokenRequest(
            expires_at=(datetime.utcnow() - timedelta(seconds=10)).isoformat()
        )
    ).token
    with pytest.raises(grpc.RpcError) as e:
        client.Register(
            host_pb2.RegisterHostRequest(
                token=token,
                host=host_pb2.Host(
                    name="yourmom",
                    address="localhost:8333",
                ),
            )
        )
    assert e.value.code() == grpc.StatusCode.INVALID_ARGUMENT
