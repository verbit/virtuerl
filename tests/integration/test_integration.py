import datetime
import io
from time import sleep
from urllib.parse import urljoin
from urllib.request import urlopen

import grpc
import pytest
from fabric import Connection
from paramiko.ed25519key import Ed25519Key

import controller_pb2_grpc
import domain_pb2
import port_forwarding_pb2
import volume_pb2


@pytest.fixture
def insecure_client():
    channel = grpc.insecure_channel("localhost:8094")
    return controller_pb2_grpc.ControllerServiceStub(channel)


@pytest.fixture
def client():
    channel = grpc.secure_channel(
        "localhost:8093",
        grpc.ssl_channel_credentials(
            root_certificates=b"""-----BEGIN CERTIFICATE-----
    MIICBDCCAYqgAwIBAgIRAOfU+EvUkVhYLwFKwS1rp1kwCgYIKoZIzj0EAwMwQzEL
    MAkGA1UEBhMCREUxDzANBgNVBAgTBkJlcmxpbjEPMA0GA1UEBxMGQmVybGluMRIw
    EAYDVQQKEwlvc2FmdC5kZXYwHhcNMjIwODIxMTEwNjA2WhcNMzIxMTI2MTEwNjA2
    WjBDMQswCQYDVQQGEwJERTEPMA0GA1UECBMGQmVybGluMQ8wDQYDVQQHEwZCZXJs
    aW4xEjAQBgNVBAoTCW9zYWZ0LmRldjB2MBAGByqGSM49AgEGBSuBBAAiA2IABMSJ
    ajmDRZT8lFk/mUh32cIpoa/0TmWOtyyP8ivEjerFpiTnX+o6Umu/66R5p52D9D0d
    +3L8Hs4DR4xfbugECKdMBlBQUl3al1VZKLSM+S+sAiqQxgsP+ZvXRCjsM/DEsaNC
    MEAwDgYDVR0PAQH/BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFOEI
    RRLKbTHzEmS6nKa2BkD1rrh3MAoGCCqGSM49BAMDA2gAMGUCMQDYfKvPNIkv3veR
    tmr4InE2Nay07ePvRn4dIXIBOIAXqjzsaZeVBpNfV2K5qi+4kW4CMGxeiGH+4HA0
    LclhLWjLmOZnTRAT8chHkNh8z+3EXt4cmJ3wpLTTkuhxmYQwS6mBrA==
    -----END CERTIFICATE-----
    """,
            certificate_chain=b"""-----BEGIN CERTIFICATE-----
    MIICEjCCAZegAwIBAgIQEU/jyKE3flG6MjI9hGa02zAKBggqhkjOPQQDAzBDMQsw
    CQYDVQQGEwJERTEPMA0GA1UECBMGQmVybGluMQ8wDQYDVQQHEwZCZXJsaW4xEjAQ
    BgNVBAoTCW9zYWZ0LmRldjAeFw0yMjA4MjExMTA2MDZaFw0zMjExMjYxMTA2MDZa
    MFIxCzAJBgNVBAYTAkRFMQ8wDQYDVQQIEwZCZXJsaW4xDzANBgNVBAcTBkJlcmxp
    bjESMBAGA1UEChMJb3NhZnQuZGV2MQ0wCwYDVQQDEwRpbHlhMHYwEAYHKoZIzj0C
    AQYFK4EEACIDYgAE7mFedSe/iD42OGn+npI0o6WaZEODuA2sgcHFZjaxwgQc3dPB
    ZWAyjyhFny7f2ACImHeEe6HXEXG1oH5J+oVbNX672vLlZop9AtOKdbBWGvJRKlaJ
    oOglPkH8atNbULpuo0EwPzAOBgNVHQ8BAf8EBAMCB4AwDAYDVR0TAQH/BAIwADAf
    BgNVHSMEGDAWgBThCEUSym0x8xJkupymtgZA9a64dzAKBggqhkjOPQQDAwNpADBm
    AjEAnnKXJnBPQRHO+JfkjnkLDHPkp1/dZUdRdSk+Frd2wWwZ9XzkbPckart2ebT7
    yHxnAjEAuNjvKHsS4PMjioUIa0l9BU1Jg/jRGM+5sO19E0Ci0TrLUOO9cUX1+0yN
    cPIi/hFA
    -----END CERTIFICATE-----
    """,
            private_key=b"""-----BEGIN EC PRIVATE KEY-----
    MIGkAgEBBDBwYW1GfUENxdgwxcf76pAlXBCf/e3PVvaWt/DRNmSJsLY1X7jnxoz4
    /zL2+F6vQLWgBwYFK4EEACKhZANiAATuYV51J7+IPjY4af6ekjSjpZpkQ4O4DayB
    wcVmNrHCBBzd08FlYDKPKEWfLt/YAIiYd4R7odcRcbWgfkn6hVs1frva8uVmin0C
    04p1sFYa8lEqVomg6CU+Qfxq01tQum4=
    -----END EC PRIVATE KEY-----
    """,
        ),
    )
    return controller_pb2_grpc.ControllerServiceStub(channel)


def test_create_domain_linux(insecure_client: controller_pb2_grpc.ControllerServiceStub):
    network = insecure_client.CreateNetwork(
        domain_pb2.CreateNetworkRequest(
            network=domain_pb2.Network(
                name="restvirt",
                cidr="192.168.69.0/24",
            )
        )
    )
    dom = insecure_client.CreateDomain(
        domain_pb2.CreateDomainRequest(
            domain=domain_pb2.Domain(
                name="test",
                vcpu=1,
                memory=512,
                private_ip="192.168.69.69",
                network=network.name,
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
    insecure_client.PutPortForwarding(
        port_forwarding_pb2.PutPortForwardingRequest(port_forwarding=fwd)
    )

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

    volumes = insecure_client.ListVolumes(volume_pb2.ListVolumesRequest())
    volumes = {v.name: v for v in volumes.volumes}
    root_vol_name = f"{dom.name}-root.qcow2"
    assert root_vol_name in volumes

    wait_until_stopped(insecure_client, dom.uuid)
    insecure_client.UpdateVolume(
        volume_pb2.UpdateVolumeRequest(
            volume=volume_pb2.Volume(id=root_vol_name, size=30 * 1024**3)
        )
    )

    insecure_client.StartDomain(domain_pb2.StartDomainRequest(uuid=dom.uuid))
    response = wait_for_http("http://192.168.69.1:8080")
    assert "Welcome to nginx!" in response

    new_root_vol_size = int(conn.run("lsblk /dev/vda -bndo SIZE").stdout)
    assert new_root_vol_size == 30 * 1024**3

    insecure_client.DeletePortForwarding(
        port_forwarding_pb2.PortForwardingIdentifier(protocol="tcp", source_port=8080)
    )
    insecure_client.DeleteDomain(domain_pb2.DeleteDomainRequest(uuid=dom.uuid))
    insecure_client.DeleteNetwork(domain_pb2.DeleteNetworkRequest(uuid=network.uuid))


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
