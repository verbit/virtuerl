import grpc
import pytest

import port_forwarding_pb2
import port_forwarding_pb2_grpc


@pytest.fixture
def client(controller_client):
    return controller_client


def test_port_forwarding_get_linux(client: port_forwarding_pb2_grpc.PortForwardingServiceStub):
    identifier = port_forwarding_pb2.PortForwardingIdentifier(
        host="test", protocol="tcp", source_port=2020
    )
    with pytest.raises(grpc.RpcError) as e:
        client.GetPortForwarding(identifier)
    assert e.value.code() == grpc.StatusCode.NOT_FOUND

    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=2020,
        target_ip="192.168.1.69",
        target_port=2021,
    )
    client.PutPortForwarding(
        port_forwarding_pb2.PutPortForwardingRequest(host="test", port_forwarding=fwd)
    )
    forwarding = client.GetPortForwarding(identifier)
    assert forwarding.target_ip == "192.168.1.69"
    assert forwarding.target_port == 2021


def test_port_forwarding_put_linux(client: port_forwarding_pb2_grpc.PortForwardingServiceStub):
    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=2020,
        target_ip="192.168.1.69",
        target_port=2021,
    )
    client.PutPortForwarding(
        port_forwarding_pb2.PutPortForwardingRequest(host="test", port_forwarding=fwd)
    )
    fwds = client.ListPortForwardings(
        port_forwarding_pb2.ListPortForwardingsRequest(host="test")
    ).port_forwardings
    assert len(fwds) == 1
    assert fwds[0].target_ip == "192.168.1.69"

    fwds = client.ListPortForwardings(
        port_forwarding_pb2.ListPortForwardingsRequest(host="test")
    ).port_forwardings
    assert len(fwds) == 1
    assert fwds[0].target_ip == "192.168.1.69"

    fwd.target_ip = "192.168.1.70"
    client.PutPortForwarding(
        port_forwarding_pb2.PutPortForwardingRequest(host="test", port_forwarding=fwd)
    )
    fwds = client.ListPortForwardings(
        port_forwarding_pb2.ListPortForwardingsRequest(host="test")
    ).port_forwardings
    assert len(fwds) == 1
    assert fwds[0].target_ip == "192.168.1.70"


def test_port_forwarding_linux(client: port_forwarding_pb2_grpc.PortForwardingServiceStub):
    import iptc

    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=2020,
        target_ip="192.168.1.69",
        target_port=2021,
    )
    client.PutPortForwarding(
        port_forwarding_pb2.PutPortForwardingRequest(host="test", port_forwarding=fwd)
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

    client.DeletePortForwarding(
        port_forwarding_pb2.PortForwardingIdentifier(host="test", protocol="tcp", source_port=2020)
    )
