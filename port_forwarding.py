import threading

from google.protobuf import empty_pb2
from grpc import StatusCode
from sqlalchemy import delete, select

import port_forwarding_pb2
import port_forwarding_pb2_grpc
from models import PortForwarding


class IPTablesPortForwardingSynchronizer:
    def __init__(self):
        self.lock = threading.Lock()

    def handle_sync(self, session):
        import iptc

        with self.lock:
            forwardings = session.execute(select(PortForwarding).filter()).scalars().all()

            restvirt_chain = "RESTVIRT"

            # nat table
            if not iptc.easy.has_chain("nat", restvirt_chain):
                iptc.easy.add_chain("nat", restvirt_chain)

            nat_rule = {"target": restvirt_chain}
            if not iptc.easy.has_rule("nat", "PREROUTING", nat_rule):
                iptc.easy.insert_rule("nat", "PREROUTING", nat_rule)
            if not iptc.easy.has_rule("nat", "OUTPUT", nat_rule):
                iptc.easy.insert_rule("nat", "OUTPUT", nat_rule)

            state_rules = [_forwarding_to_nat_rule(f) for f in forwardings]

            # step 1: delete iptables rules that are not in the state
            for rule in iptc.easy.dump_chain("nat", restvirt_chain):
                if rule not in state_rules:
                    iptc.easy.delete_rule("nat", restvirt_chain, rule)

            # step 2: add iptables rule from state
            for rule in state_rules:
                if not iptc.easy.has_rule("nat", restvirt_chain, rule):
                    iptc.easy.insert_rule("nat", restvirt_chain, rule)

                    # nat rules
            if not iptc.easy.has_chain("nat", restvirt_chain):
                iptc.easy.add_chain("nat", restvirt_chain)

            # filter table
            if not iptc.easy.has_chain("filter", restvirt_chain):
                iptc.easy.add_chain("filter", restvirt_chain)

            nat_rule = {"target": restvirt_chain}
            if not iptc.easy.has_rule("filter", "FORWARD", nat_rule):
                iptc.easy.insert_rule("filter", "FORWARD", nat_rule)

            state_rules = [_forwarding_to_fwd_rule(f) for f in forwardings]

            # step 1: delete iptables rules that are not in the state
            for rule in iptc.easy.dump_chain("filter", restvirt_chain):
                if rule not in state_rules:
                    iptc.easy.delete_rule("filter", restvirt_chain, rule)

            # step 2: add iptables rule from state
            for rule in state_rules:
                if not iptc.easy.has_rule("filter", restvirt_chain, rule):
                    iptc.easy.insert_rule("filter", restvirt_chain, rule)


# FIXME: limit to eno2 src port
def _forwarding_to_nat_rule(f):
    prot = f.protocol
    return {
        "protocol": prot,
        prot: {
            "dport": str(f.source_port),
        },
        "target": {
            "DNAT": {
                "to_destination": f"{f.target_ip}:{f.target_port}",
            }
        },
    }


# FIXME: limit to eno2 src port
def _forwarding_to_fwd_rule(f):
    prot = f.protocol
    return {
        "protocol": prot,
        "out-interface": "virbr0",
        "dst": f.target_ip,
        prot: {
            "dport": str(f.target_port),
        },
        "target": "ACCEPT",
    }


class PortForwardingService(port_forwarding_pb2_grpc.PortForwardingServiceServicer):
    def __init__(self, session_factory, sync_handler):
        self.session_factory = session_factory
        self.sync_handler = sync_handler

    def GetPortForwarding(self, request, context):
        with self.session_factory() as session:
            forwarding = (
                session.execute(
                    select(PortForwarding).filter(
                        PortForwarding.protocol == request.protocol,
                        PortForwarding.source_port == request.source_port,
                    )
                )
                .scalars()
                .one_or_none()
            )
        if forwarding is None:
            context.set_code(StatusCode.NOT_FOUND)
            return
        return port_forwarding_pb2.PortForwarding(
            protocol=forwarding.protocol,
            source_port=forwarding.source_port,
            target_ip=forwarding.target_ip,
            target_port=forwarding.target_port,
        )

    def ListPortForwardings(self, request, context):
        with self.session_factory() as session:
            fwds = session.execute(select(PortForwarding)).scalars().all()
        return port_forwarding_pb2.ListPortForwardingsResponse(
            port_forwardings=[
                port_forwarding_pb2.PortForwarding(
                    protocol=fwd.protocol,
                    source_port=fwd.source_port,
                    target_ip=fwd.target_ip,
                    target_port=fwd.target_port,
                )
                for fwd in fwds
            ]
        )

    def PutPortForwarding(self, request, context):
        f = request.port_forwarding
        route = PortForwarding(
            protocol=f.protocol,
            source_port=f.source_port,
            target_ip=f.target_ip,
            target_port=f.target_port,
        )

        with self.session_factory() as session:
            session.merge(route)
            session.commit()
            self.sync_handler.handle_sync(session)

        return f

    def DeletePortForwarding(self, request, context):
        with self.session_factory() as session:
            session.execute(
                delete(PortForwarding).where(
                    PortForwarding.protocol == request.protocol,
                    PortForwarding.source_port == request.source_port,
                )
            )
            session.commit()
            self.sync_handler.handle_sync(session)

        return empty_pb2.Empty()

    def sync(self):
        with self.session_factory() as session:
            self.sync_handler.handle_sync(session)
