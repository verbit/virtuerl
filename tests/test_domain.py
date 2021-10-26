import datetime
import logging
from concurrent import futures
from time import sleep
from urllib.parse import urljoin
from urllib.request import urlopen

import grpc
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
def client(engine, pool_dir, conn):
    from main import DomainService

    logging.basicConfig()
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    domain_pb2_grpc.add_DomainServiceServicer_to_server(DomainService(pool_dir=pool_dir), server)
    port = server.add_insecure_port("localhost:0")
    server.start()
    channel = grpc.insecure_channel(f"localhost:{port}")
    stub = domain_pb2_grpc.DomainServiceStub(channel)
    yield stub
    server.stop(1)

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


def test_create_domain_linux(client: domain_pb2_grpc.DomainServiceStub):
    from main import ensure_rfc1918_rules

    ensure_rfc1918_rules()

    dom = client.CreateDomain(
        domain_pb2.CreateDomainRequest(
            domain=domain_pb2.Domain(
                name="test",
                vcpu=1,
                memory=512,
                private_ip="192.168.123.55",
                user_data="""#cloud-config

packages:
  - nginx

runcmd:
  - service nginx restart
""",
            )
        )
    )

    response = wait_for_http("http://192.168.123.55")
    assert "Welcome to nginx!" in response

    client.DeleteDomain(domain_pb2.DeleteDomainRequest(uuid=dom.uuid))


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
