import logging
from concurrent import futures

import grpc
import pytest
from sqlalchemy.orm import sessionmaker

import port_forwarding_pb2
import port_forwarding_pb2_grpc
from port_forwarding import IPTablesPortForwardingSynchronizer, PortForwardingService
from route import SyncEventHandler


@pytest.fixture
def client(engine):
    logging.basicConfig()
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    Session = sessionmaker(engine, future=True)
    port_forwarding_pb2_grpc.add_PortForwardingServiceServicer_to_server(
        PortForwardingService(Session, SyncEventHandler()), server
    )
    port = server.add_insecure_port("localhost:0")
    server.start()
    channel = grpc.insecure_channel(f"localhost:{port}")
    stub = port_forwarding_pb2_grpc.PortForwardingServiceStub(channel)
    yield stub
    server.stop(1)


def test_port_forwarding_get(client: port_forwarding_pb2_grpc.PortForwardingServiceStub):
    identifier = port_forwarding_pb2.PortForwardingIdentifier(protocol="tcp", source_port=2020)
    with pytest.raises(grpc.RpcError) as e:
        client.GetPortForwarding(identifier)
    assert e.value.code() == grpc.StatusCode.NOT_FOUND

    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=2020,
        target_ip="192.168.1.69",
        target_port=2021,
    )
    client.PutPortForwarding(port_forwarding_pb2.PutPortForwardingRequest(port_forwarding=fwd))
    forwarding = client.GetPortForwarding(identifier)
    assert forwarding.target_ip == "192.168.1.69"
    assert forwarding.target_port == 2021


def test_port_forwarding_put(client: port_forwarding_pb2_grpc.PortForwardingServiceStub):
    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=2020,
        target_ip="192.168.1.69",
        target_port=2021,
    )
    client.PutPortForwarding(port_forwarding_pb2.PutPortForwardingRequest(port_forwarding=fwd))
    fwds = client.ListPortForwardings(
        port_forwarding_pb2.ListPortForwardingsRequest()
    ).port_forwardings
    assert len(fwds) == 1
    assert fwds[0].target_ip == "192.168.1.69"

    fwds = client.ListPortForwardings(
        port_forwarding_pb2.ListPortForwardingsRequest()
    ).port_forwardings
    assert len(fwds) == 1
    assert fwds[0].target_ip == "192.168.1.69"

    fwd.target_ip = "192.168.1.70"
    client.PutPortForwarding(port_forwarding_pb2.PutPortForwardingRequest(port_forwarding=fwd))
    fwds = client.ListPortForwardings(
        port_forwarding_pb2.ListPortForwardingsRequest()
    ).port_forwardings
    assert len(fwds) == 1
    assert fwds[0].target_ip == "192.168.1.70"


@pytest.fixture
def client_iptables(engine):
    logging.basicConfig()
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    Session = sessionmaker(engine, future=True)
    port_forwarding_pb2_grpc.add_PortForwardingServiceServicer_to_server(
        PortForwardingService(Session, IPTablesPortForwardingSynchronizer()), server
    )
    port = server.add_insecure_port("localhost:0")
    server.start()
    channel = grpc.insecure_channel(f"localhost:{port}")
    stub = port_forwarding_pb2_grpc.PortForwardingServiceStub(channel)
    yield stub
    server.stop(1)


def test_port_forwarding_linux(client_iptables: port_forwarding_pb2_grpc.PortForwardingServiceStub):
    import iptc

    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=2020,
        target_ip="192.168.1.69",
        target_port=2021,
    )
    client_iptables.PutPortForwarding(
        port_forwarding_pb2.PutPortForwardingRequest(port_forwarding=fwd)
    )

    CHAIN_NAME = "RESTVIRT"
    rules = iptc.easy.dump_chain("filter", CHAIN_NAME)
    assert len(rules) == 1
    rule = rules[0]
    assert rule["protocol"] == "tcp"
    assert rule["tcp"]["dport"] == "2021"
    assert rule["dst"] == "192.168.1.69/32"

    rules = iptc.easy.dump_chain("nat", CHAIN_NAME)
    assert len(rules) == 1
    rule = rules[0]
    assert rule["protocol"] == "tcp"
    assert rule["tcp"]["dport"] == "2020"
    assert rule["target"]["DNAT"]["to-destination"] == "192.168.1.69:2021"

    client_iptables.DeletePortForwarding(
        port_forwarding_pb2.PortForwardingIdentifier(protocol="tcp", source_port=2020)
    )
