import datetime
from time import sleep
from urllib.parse import urljoin
from urllib.request import urlopen

import grpc
import pytest

import controller_pb2_grpc
import domain_pb2
import port_forwarding_pb2


@pytest.fixture
def client():
    channel = grpc.insecure_channel("localhost:8094")
    return controller_pb2_grpc.ControllerServiceStub(channel)


def test_create_domain_linux(client: controller_pb2_grpc.ControllerServiceStub):
    network = client.CreateNetwork(
        domain_pb2.CreateNetworkRequest(
            network=domain_pb2.Network(
                name="restvirt",
                cidr="192.168.69.0/24",
            )
        ),
        wait_for_ready=True,
    )
    dom = client.CreateDomain(
        domain_pb2.CreateDomainRequest(
            domain=domain_pb2.Domain(
                name="test",
                vcpu=1,
                memory=512,
                private_ip="192.168.69.69",
                network=network.name,
                user_data="""#cloud-config

packages:
  - nginx

runcmd:
  - service nginx restart
""",
            ),
        )
    )

    fwd = port_forwarding_pb2.PortForwarding(
        protocol="tcp",
        source_port=8080,
        target_ip="192.168.69.69",
        target_port=80,
    )
    client.PutPortForwarding(port_forwarding_pb2.PutPortForwardingRequest(port_forwarding=fwd))

    response = wait_for_http("http://192.168.69.1:8080")
    assert "Welcome to nginx!" in response

    client.DeletePortForwarding(
        port_forwarding_pb2.PortForwardingIdentifier(protocol="tcp", source_port=8080)
    )
    client.DeleteDomain(domain_pb2.DeleteDomainRequest(uuid=dom.uuid))
    client.DeleteNetwork(domain_pb2.DeleteNetworkRequest(uuid=network.uuid))


def wait_for_http(server, path="/", timeout=datetime.timedelta(seconds=180)):
    end_time = datetime.datetime.now() + timeout
    while True:
        try:
            with urlopen(urljoin(server, path)) as resp:
                return resp.read().decode()
        except:
            now = datetime.datetime.now()
            if now >= end_time:
                raise Exception("Timed out")
            sleep(min((end_time - now).total_seconds(), 10))
