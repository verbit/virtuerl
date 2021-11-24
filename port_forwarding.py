import threading

from sqlalchemy import select

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
