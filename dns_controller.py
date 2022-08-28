import socket

import dnslib
from dns import resolver
from dnslib import QTYPE, RCODE, RR, copy
from dnslib.server import DNSServer
from sqlalchemy import delete, select

from models import DNSRecord


class DNSController:
    def __init__(self, session_factory):
        self.session_factory = session_factory
        self.server = None

        r = resolver.Resolver()
        self.upstream = r.nameservers[0]

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
            # check for zone delegation
            for name, rtype, rr in zone:
                if rtype == "NS" and qname.matchSuffix(name):
                    if reply is None:
                        reply = request.reply()
                    reply.add_auth(copy.copy(rr))

        if reply is None:
            try:
                reply = dnslib.DNSRecord.parse(request.send(self.upstream, 53, timeout=3))
            except socket.timeout:
                reply.header.rcode = RCODE.SERVFAIL
        return reply

    def start(self, port=53):
        self.server = DNSServer(self, port=port)
        self.server.start_thread()
        return self.server.server.server_address[1]

    def stop(self):
        if self.server is not None:
            self.server.stop()
            self.server = None
