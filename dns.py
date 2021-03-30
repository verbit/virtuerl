import json
import os
import threading
from dataclasses import dataclass, asdict
from typing import List

from dnslib import RR, QTYPE, copy, RCODE
from dnslib.server import DNSServer


@dataclass
class DNSRecord:
    name: str
    type: str
    ttl: int
    records: List[str]


class DNSController:
    def __init__(self, state_dir, state_file_name="dns.json"):
        self.lock = threading.Lock()
        self.state_dir = state_dir
        self.state_file_path = os.path.join(state_dir, state_file_name)

        self._read_state_file()
        self.server = None

    def _update_zone(self):
        zone_file = "\n".join(
            [f"{f.name} {f.ttl} {f.type} {' '.join(f.records)}" for f in self.records.values()]
        )
        self.zone = [(rr.rname, QTYPE[rr.rtype], rr) for rr in RR.fromZone(zone_file)]

    def _update_state_file(self):
        with open(self.state_file_path, mode="w") as f:
            f.write(json.dumps([asdict(v) for v in self.records.values()]))

        self._update_zone()

    def _read_state_file(self):
        if not os.path.isfile(self.state_file_path):
            self.records = {}
        else:
            with open(self.state_file_path) as f:
                mappings = json.load(f)
                mappings = [DNSRecord(**r) for r in mappings]
                self.records = {(r.name, r.type): r for r in mappings}

        self._update_zone()

    def set(self, record: DNSRecord):
        with self.lock:
            self.records[(record.name, record.type)] = record
            self._update_state_file()

    def remove(self, name, type):
        with self.lock:
            try:
                del self.records[(name, type)]
                self._update_state_file()
            except ValueError:
                pass

    def get_mappings(self):
        with self.lock:
            return list(self.records.values())

    def get_mapping(self, name, type):
        with self.lock:
            return self.records[(name, type)]

    def resolve(self, request, handler):
        with self.lock:
            zone = self.zone.copy()

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

    def stop(self):
        if self.server is not None:
            self.server.stop()
            self.server = None
