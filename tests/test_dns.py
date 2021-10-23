import logging
from concurrent import futures

import grpc
import pytest
from dnslib import CLASS, QTYPE, RR, DNSQuestion, DNSRecord
from sqlalchemy.orm import sessionmaker

import dns_pb2
import dns_pb2_grpc
from dns import DNSController, DNSService


class DNSClient:
    def __init__(self, host="localhost", port=53):
        self.host = host
        self.port = port

    def query(self, name, qtype, qclass="IN"):
        q = DNSRecord(q=DNSQuestion(name, getattr(QTYPE, qtype), getattr(CLASS, qclass)))
        raw = q.send(self.host, self.port)
        return DNSRecord.parse(raw)


@pytest.fixture
def dns_controller(engine):
    session_factory = sessionmaker(engine, future=True)
    return DNSController(session_factory)


@pytest.fixture
def dns_client(dns_controller):
    port = dns_controller.start(port=0)
    yield DNSClient(port=port)
    dns_controller.stop()


@pytest.fixture
def client(dns_controller):
    logging.basicConfig()
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    dns_pb2_grpc.add_DNSServicer_to_server(DNSService(dns_controller), server)
    port = server.add_insecure_port("localhost:0")
    server.start()
    channel = grpc.insecure_channel(f"localhost:{port}")
    stub = dns_pb2_grpc.DNSStub(channel)
    yield stub
    server.stop(1)


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
