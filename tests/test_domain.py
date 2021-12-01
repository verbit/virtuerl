import datetime
from time import sleep
from urllib.parse import urljoin
from urllib.request import urlopen

import pytest

import domain_pb2
import domain_pb2_grpc


@pytest.fixture
def conn():
    import libvirt

    conn = libvirt.open("qemu:///system?socket=/var/run/libvirt/libvirt-sock")
    yield conn
    conn.close()


@pytest.fixture
def client(controller_client, pool_dir, conn):
    yield controller_client

    img_pool = conn.storagePoolLookupByName("restvirtimages")
    for vol in img_pool.listAllVolumes():
        vol.delete()
    img_pool.destroy()
    img_pool.delete()
    img_pool.undefine()

    for dom in conn.listAllDomains():
        try:
            dom.destroy()
        except:
            pass
        dom.undefine()


def test_libvirt_linux():
    import libvirt

    libvirt.getVersion()


def test_network_linux(client: domain_pb2_grpc.DomainServiceStub):
    network = client.CreateNetwork(
        domain_pb2.CreateNetworkRequest(
            network=domain_pb2.Network(
                name="restvirt",
                cidr="192.168.69.0/24",
            )
        )
    )

    resp = client.GetNetwork(domain_pb2.GetNetworkRequest(uuid=network.uuid))
    assert resp.name == "restvirt"
    assert resp.cidr == "192.168.69.0/24"

    client.DeleteNetwork(domain_pb2.DeleteNetworkRequest(uuid=network.uuid))


def test_create_domain_linux(client: domain_pb2_grpc.DomainServiceStub):
    from main import ensure_rfc1918_rules

    ensure_rfc1918_rules()

    network = client.CreateNetwork(
        domain_pb2.CreateNetworkRequest(
            network=domain_pb2.Network(
                name="restvirt",
                cidr="192.168.69.0/24",
            )
        )
    )
    dom = client.CreateDomain(
        domain_pb2.CreateDomainRequest(
            host="test",
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

    response = wait_for_http("http://192.168.69.69")
    assert "Welcome to nginx!" in response

    client.DeleteDomain(domain_pb2.DeleteDomainRequest(host="test", uuid=dom.uuid))
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
