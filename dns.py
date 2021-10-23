from dnslib import QTYPE, RCODE, RR, copy
from dnslib.server import DNSServer
from google.protobuf import empty_pb2
from grpc import StatusCode
from sqlalchemy import delete, select

import dns_pb2
import dns_pb2_grpc
from models import DNSRecord


class DNSController:
    def __init__(self, session_factory):
        self.session_factory = session_factory
        self.server = None

    def records(self):
        with self.session_factory() as session:
            return session.execute(select(DNSRecord)).scalars().all()

    def record(self, name, type):
        with self.session_factory() as session:
            return (
                session.execute(
                    select(DNSRecord).filter(
                        DNSRecord.name == name,
                        DNSRecord.type == type,
                    )
                )
                .scalars()
                .one_or_none()
            )

    def load_zone_list(self):
        zone_file = "\n".join(
            [f"{f.name} {f.ttl} {f.type} {' '.join(f.records)}" for f in self.records()]
        )
        return [(rr.rname, QTYPE[rr.rtype], rr) for rr in RR.fromZone(zone_file)]

    def set(self, record: DNSRecord):
        with self.session_factory() as session:
            session.merge(record)
            session.commit()

    def remove(self, name, type):
        with self.session_factory() as session:
            session.execute(
                delete(DNSRecord).where(
                    DNSRecord.name == name,
                    DNSRecord.type == type,
                )
            )
            session.commit()

    def resolve(self, request, handler):
        zone = self.load_zone_list()

        reply = None
        qname = request.q.qname
        qtype = QTYPE[request.q.qtype]
        for name, rtype, rr in zone:
            # Check if label & type match
            if qname.matchGlob(name):
                reply = request.reply()
                if qtype == rtype or qtype == "ANY" or rtype == "CNAME":
                    # Since we have a glob match fix reply label
                    a = copy.copy(rr)
                    a.rname = qname
                    reply.add_answer(a)
                    # Check for A/AAAA records associated with reply and
                    # add in additional section
                    if rtype in ["CNAME", "NS", "MX", "PTR"]:
                        for a_name, a_rtype, a_rr in zone:
                            if a_name == rr.rdata.label and a_rtype in ["A", "AAAA"]:
                                reply.add_answer(a_rr)
        if reply is None:
            reply = request.reply()
            reply.header.rcode = RCODE.NXDOMAIN
        return reply

    def start(self, port=53):
        self.server = DNSServer(self, port=port)
        self.server.start_thread()
        return self.server.server.server_address[1]

    def stop(self):
        if self.server is not None:
            self.server.stop()
            self.server = None


class DNSService(dns_pb2_grpc.DNSServicer):
    def __init__(self, dns_controller):
        self.dns_controller = dns_controller

    def GetDNSRecord(self, request, context):
        record = self.dns_controller.record(request.name, request.type)
        if record is None:
            context.set_code(StatusCode.NOT_FOUND)
            return
        return dns_pb2.DNSRecord(
            name=record.name,
            type=record.type,
            ttl=record.ttl,
            records=record.records,
        )

    def ListDNSRecords(self, request, context):
        return dns_pb2.ListDNSRecordsResponse(
            dns_records=[
                dns_pb2.DNSRecord(
                    name=record.name,
                    type=record.type,
                    ttl=record.ttl,
                    records=record.records,
                )
                for record in self.dns_controller.records()
            ]
        )

    def PutDNSRecord(self, request, context):
        record = request.dns_record
        self.dns_controller.set(
            DNSRecord(
                name=record.name,
                type=record.type,
                ttl=record.ttl,
                records=record.records,
            )
        )
        return record

    def DeleteDNSRecord(self, request, context):
        self.dns_controller.remove(request.name, request.type)
        return empty_pb2.Empty()
