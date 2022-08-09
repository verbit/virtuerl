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
    import nftables

    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=2020,
        target_ip="192.168.1.69",
        target_port=2021,
    )
    client.PutPortForwarding(
        port_forwarding_pb2.PutPortForwardingRequest(host="test", port_forwarding=fwd)
    )

    table_name = "restvirt"
    nft = nftables.Nftables()
    nft.set_json_output(True)
    nft.set_stateless_output(True)

    _, output, _ = nft.json_cmd(
        {
            "nftables": [
                {
                    "list": {
                        "table": {
                            "family": "inet",
                            "name": table_name,
                        }
                    }
                }
            ]
        }
    )
    output = output["nftables"]
    all_rules = [rule["rule"] for rule in output if "rule" in rule]

    rules = [rule for rule in all_rules if rule["chain"] == "forward"]
    assert len(rules) == 1
    rule = rules[0]
    assert rule["expr"] == [
        {
            "match": {
                "left": {"payload": {"field": "dport", "protocol": "tcp"}},
                "op": "==",
                "right": 2021,
            }
        },
        {
            "match": {
                "left": {"payload": {"field": "daddr", "protocol": "ip"}},
                "op": "==",
                "right": "192.168.1.69",
            }
        },
        {"counter": {"bytes": 0, "packets": 0}},
        {"accept": None},
    ]

    rules = [rule for rule in all_rules if rule["chain"] == "prerouting"]
    assert len(rules) == 1
    rule = rules[0]
    assert rule["expr"] == [
        {
            "match": {
                "left": {"payload": {"field": "dport", "protocol": "tcp"}},
                "op": "==",
                "right": 2020,
            }
        },
        {"counter": {"bytes": 0, "packets": 0}},
        {"dnat": {"addr": "192.168.1.69", "family": "ip", "port": 2021}},
    ]

    client.DeletePortForwarding(
        port_forwarding_pb2.PortForwardingIdentifier(host="test", protocol="tcp", source_port=2020)
    )
