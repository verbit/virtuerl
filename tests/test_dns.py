import grpc
import pytest
from dnslib import CLASS, QTYPE, RR, DNSQuestion, DNSRecord

import dns_pb2
import dns_pb2_grpc


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
    port = dns_controller.start(port=0)
    yield DNSClient(port=port)
    dns_controller.stop()


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
