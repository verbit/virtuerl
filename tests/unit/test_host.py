from datetime import datetime, timedelta

import grpc
import pytest

from minivirt import host_pb2, host_pb2_grpc


@pytest.fixture
def client(host_client):
    return host_client


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
