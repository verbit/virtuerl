import grpc
import pytest
from dnslib import CLASS, QTYPE, RCODE, RR, DNSQuestion, DNSRecord
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import StaticPool

import dns_pb2
import dns_pb2_grpc
import models
from dns_controller import DNSController
from models import Base

from .conftest import dummy_controller


class DNSClient:
    def __init__(self, host="localhost", port=53):
        self.host = host
        self.port = port

    def query(self, name, qtype, qclass="IN"):
        q = DNSRecord(q=DNSQuestion(name, getattr(QTYPE, qtype), getattr(CLASS, qclass)))
        raw = q.send(self.host, self.port)
        return DNSRecord.parse(raw)


@pytest.fixture
def dns_client(dns_controller):
    yield DNSClient()


@pytest.fixture
def client(controller_client_dummy):
    return controller_client_dummy


def test_dns_put(client: dns_pb2_grpc.DNSStub, dns_client):
    client.PutDNSRecord(
        dns_pb2.PutDNSRecordRequest(
            dns_record=dns_pb2.DNSRecord(
                name="mydomain.internal",
                type="A",
                ttl=60,
                records=["192.168.1.1"],
            )
        )
    )
    records = client.ListDNSRecords(dns_pb2.ListDNSRecordsRequest()).dns_records
    assert len(records) == 1
    assert dns_client.query("mydomain.internal", "A").rr == RR.fromZone(
        "mydomain.internal. 60 IN A 192.168.1.1"
    )

    client.PutDNSRecord(
        dns_pb2.PutDNSRecordRequest(
            dns_record=dns_pb2.DNSRecord(
                name="mydomain.internal",
                type="A",
                ttl=120,
                records=["192.168.1.2"],
            )
        )
    )
    records = client.ListDNSRecords(dns_pb2.ListDNSRecordsRequest()).dns_records
    assert len(records) == 1
    assert dns_client.query("mydomain.internal", "A").rr == RR.fromZone(
        "mydomain.internal. 120 IN A 192.168.1.2"
    )


def test_dns_get(client: dns_pb2_grpc.DNSStub):
    client.PutDNSRecord(
        dns_pb2.PutDNSRecordRequest(
            dns_record=dns_pb2.DNSRecord(
                name="mydomain.internal",
                type="A",
                ttl=120,
                records=["192.168.1.2"],
            )
        )
    )

    record = client.GetDNSRecord(dns_pb2.DNSRecordIdentifier(name="mydomain.internal", type="A"))
    assert record.records == ["192.168.1.2"]

    with pytest.raises(grpc.RpcError) as e:
        client.GetDNSRecord(dns_pb2.DNSRecordIdentifier(name="non-existing.internal", type="A"))
    assert e.value.code() == grpc.StatusCode.NOT_FOUND


def test_dns_forward(client: dns_pb2_grpc.DNSStub, dns_client):
    assert dns_client.query("google.com", "A").header.rcode == RCODE.NOERROR
    assert dns_client.query("non-existing.internal", "A").header.rcode == RCODE.NXDOMAIN


def test_dns_delete(client: dns_pb2_grpc.DNSStub):
    client.PutDNSRecord(
        dns_pb2.PutDNSRecordRequest(
            dns_record=dns_pb2.DNSRecord(
                name="mydomain.internal",
                type="A",
                ttl=120,
                records=["192.168.1.2"],
            )
        )
    )

    client.DeleteDNSRecord(dns_pb2.DNSRecordIdentifier(name="mydomain.internal", type="A"))
    records = client.ListDNSRecords(dns_pb2.ListDNSRecordsRequest()).dns_records
    assert len(records) == 0


def test_dns_delegation():
    engine = create_engine(
        "sqlite:///:memory:",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
        echo=False,
        future=True,
    )
    Base.metadata.create_all(engine)
    sessfact = sessionmaker(engine)
    upstream_dns = DNSController(sessfact)
    with sessfact() as session:
        session.merge(
            models.DNSRecord(name="test.delegated.internal", type="A", records=["192.168.11.11"])
        )
        session.commit()
    upstream_dns.start()

    with dummy_controller(dns_port=30053) as client:
        client.PutDNSRecord(
            dns_pb2.PutDNSRecordRequest(
                dns_record=dns_pb2.DNSRecord(
                    name="delegated.internal",
                    type="NS",
                    ttl=120,
                    records=["127.0.0.1"],
                )
            )
        )

        dns_client = DNSClient(port=30053)
        resp = dns_client.query("test.delegated.internal", "A")
        assert len(resp.ar) == 0
        assert len(resp.auth) == 1
        assert resp.auth[0].rtype == QTYPE.NS
        assert resp.auth[0].rname == "delegated.internal"
        assert str(resp.auth[0].rdata) == "127.0.0.1."

    upstream_dns.stop()
