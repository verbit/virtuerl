import datetime
import io
from time import sleep
from urllib.parse import urljoin
from urllib.request import urlopen

import grpc
import pytest
from fabric import Connection
from paramiko.ed25519key import Ed25519Key

from minivirt import controller_pb2_grpc, domain_pb2, port_forwarding_pb2, volume_pb2


@pytest.fixture
def client():
    channel = grpc.secure_channel(
        "localhost:8093",
        grpc.ssl_channel_credentials(
            root_certificates=b"""-----BEGIN CERTIFICATE-----
MIICCjCCAY+gAwIBAgIUSl7KWjtgvG9rNMz7hhYRKy5LsB8wCgYIKoZIzj0EAwIw
RDELMAkGA1UEBhMCREUxDzANBgNVBAgMBkJlcmxpbjERMA8GA1UECgwIbWluaXZp
cnQxETAPBgNVBAMMCG1pbml2aXJ0MB4XDTIzMDcwODIzMjUyMFoXDTMzMDcwNTIz
MjUyMFowRDELMAkGA1UEBhMCREUxDzANBgNVBAgMBkJlcmxpbjERMA8GA1UECgwI
bWluaXZpcnQxETAPBgNVBAMMCG1pbml2aXJ0MHYwEAYHKoZIzj0CAQYFK4EEACID
YgAEI3nOFzsWO3w8qGLSjDSiX3OWCH7qBRcTjt/luPjXLqe3DVcFQPLYN31PaggR
o0jCjrKklxqtzHmmMdMRnyoRPbQOQPRa9N177a2s97M5ZJQVkeFL8WRUf7x1P0Cd
SZvco0IwQDAdBgNVHQ4EFgQUde9ormRTZys4Nt81qAPcSm1qQWMwDwYDVR0TAQH/
BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAYYwCgYIKoZIzj0EAwIDaQAwZgIxAJTpUOUz
RJwMLNUEa4qIgdamMuqyl5h6ghT9zX5BLsX3cFs+MqJ/J0HcKDJd81lVlQIxAPCG
hy/zgj6wy28S8GWe8KdbZ73BJtC5MyOu6hHD9EpZ1hT8K3q0VIwMEyUMmbv3Xw==
-----END CERTIFICATE-----
""",
            certificate_chain=b"""-----BEGIN CERTIFICATE-----
MIICeTCCAf+gAwIBAgIUVG1gWhFwRQ5kzxwRzF6V5h6D8ZYwCgYIKoZIzj0EAwIw
RDELMAkGA1UEBhMCREUxDzANBgNVBAgMBkJlcmxpbjERMA8GA1UECgwIbWluaXZp
cnQxETAPBgNVBAMMCG1pbml2aXJ0MB4XDTIzMDcwODIzMzA0MloXDTI1MDcwNzIz
MzA0MlowRDELMAkGA1UEBhMCREUxDzANBgNVBAgMBkJlcmxpbjERMA8GA1UECgwI
bWluaXZpcnQxETAPBgNVBAMMCG1pbml2aXJ0MHYwEAYHKoZIzj0CAQYFK4EEACID
YgAEHWV9eL3/egpqcgTaMDPWga2xpfTZCc66yNxkVGPsw5BWE/EXvWtuUCjDmHWo
HOdrbt7iI9lA9VnSwlC9PeIvX4lK2dXNOpn3GJlZ8JkjpZZBg0mxaUt6vQMyGSco
cOOAo4GxMIGuMB8GA1UdIwQYMBaAFHXvaK5kU2crODbfNagD3EptakFjMAwGA1Ud
EwEB/wQCMAAwDgYDVR0PAQH/BAQDAgWgMCAGA1UdJQEB/wQWMBQGCCsGAQUFBwMB
BggrBgEFBQcDAjAsBgNVHREEJTAjgglsb2NhbGhvc3SHBH8AAAGHEAAAAAAAAAAA
AAAAAAAAAAEwHQYDVR0OBBYEFDtlzMapZ4eV0/m8ina1GM3biELeMAoGCCqGSM49
BAMCA2gAMGUCMECXLmHsWMTaFRK+qWaBRZMLuhFNixMsSmmHHIqGlvIrWFa5MiN6
RaZ7aTGa/HMKZAIxAPRJZ11Vp1BNBszSiswk32hsck4JP9h1hn00IMu33iK0+q22
Jv73oZy3l4gQmLlCDg==
-----END CERTIFICATE-----
""",
            private_key=b"""-----BEGIN PRIVATE KEY-----
MIG2AgEAMBAGByqGSM49AgEGBSuBBAAiBIGeMIGbAgEBBDCIJQuTNENeEnhjCtdV
GXoFgh69LQ+Ms5B/i0jWMSqMALL26uG6NNM4nvHgIMbChKGhZANiAAQdZX14vf96
CmpyBNowM9aBrbGl9NkJzrrI3GRUY+zDkFYT8Re9a25QKMOYdagc52tu3uIj2UD1
WdLCUL094i9fiUrZ1c06mfcYmVnwmSOllkGDSbFpS3q9AzIZJyhw44A=
-----END PRIVATE KEY-----
""",
        ),
    )
    return controller_pb2_grpc.ControllerServiceStub(channel)


def test_create_domain_linux(client: controller_pb2_grpc.ControllerServiceStub):
    network = client.CreateNetwork(
        domain_pb2.CreateNetworkRequest(
            network=domain_pb2.Network(
                name="restvirt",
                cidr="192.168.69.0/24",
                cidr6="fd8d:dd47:05bc:5307::/64",
            )
        )
    )
    dom = client.CreateDomain(
        domain_pb2.CreateDomainRequest(
            domain=domain_pb2.Domain(
                name="test",
                vcpu=1,
                memory=512,
                private_ip="192.168.69.69",
                ipv6_address="fd8d:dd47:05bc:5307::10",
                network=network.uuid,
                user_data="""#cloud-config

users:
  - name: tester
    shell: /bin/bash
    sudo: ALL=(ALL) NOPASSWD:ALL
    ssh_authorized_keys:
      - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBDCT3LrJenezXzP9T6519IgpVCP1uv6f5iQwZ+IDdFc

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

    private_ssh_key = """-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACAQwk9y6yXp3s18z/U+udfSIKVQj9br+n+YkMGfiA3RXAAAAJhEeSndRHkp
3QAAAAtzc2gtZWQyNTUxOQAAACAQwk9y6yXp3s18z/U+udfSIKVQj9br+n+YkMGfiA3RXA
AAAEC9jx0pxfKwyPq3RpOsCef7UA2pqBzAj2+bqVTix2f0fRDCT3LrJenezXzP9T6519Ig
pVCP1uv6f5iQwZ+IDdFcAAAAFWlseWFASWx5YXMtaU1hYy5sb2NhbA==
-----END OPENSSH PRIVATE KEY-----
"""
    with io.StringIO(private_ssh_key) as f:
        pkey = Ed25519Key.from_private_key(f)
    conn = Connection("192.168.69.69", "tester", connect_kwargs={"pkey": pkey})
    conn.config.run.in_stream = False

    old_root_vol_size = int(conn.run("lsblk /dev/vda -bndo SIZE").stdout)
    assert old_root_vol_size == 20 * 1024**3

    volumes = client.ListVolumes(volume_pb2.ListVolumesRequest())
    volumes = {v.name: v for v in volumes.volumes}
    root_vol_name = f"{dom.name}-root.qcow2"
    assert root_vol_name in volumes

    wait_until_stopped(client, dom.uuid)
    client.UpdateVolume(
        volume_pb2.UpdateVolumeRequest(
            volume=volume_pb2.Volume(id=root_vol_name, size=30 * 1024**3)
        )
    )

    client.StartDomain(domain_pb2.StartDomainRequest(uuid=dom.uuid))
    response = wait_for_http("http://192.168.69.1:8080")
    assert "Welcome to nginx!" in response
    response = wait_for_http("http://[fd8d:dd47:05bc:5307::10]")
    assert "Welcome to nginx!" in response

    new_root_vol_size = int(conn.run("lsblk /dev/vda -bndo SIZE").stdout)
    assert new_root_vol_size == 30 * 1024**3

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


def wait_until_stopped(client, domain_uuid, timeout=datetime.timedelta(seconds=180)):
    end_time = datetime.datetime.now() + timeout
    while True:
        dom = client.GetDomain(domain_pb2.GetDomainRequest(uuid=domain_uuid))
        if dom.state == "SHUTOFF":
            break

        client.StopDomain(domain_pb2.StopDomainRequest(uuid=domain_uuid))
        now = datetime.datetime.now()
        if now >= end_time:
            raise Exception("Timed out")
        sleep(min((end_time - now).total_seconds(), 10))
