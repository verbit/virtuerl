import json
import os
import threading

import iptc


class PortForwardingController:
    def __init__(self, state_dir, state_file_name="forwardings.json"):
        self.lock = threading.Lock()
        self.state_dir = state_dir
        self.state_file_path = os.path.join(state_dir, state_file_name)

    def _update_state_file(self, forwardings):

        # f = tempfile.NamedTemporaryFile('w+', delete=False)
        # try:
        #     f.write(json.dumps(forwardings))
        #     f.flush()
        #     f.close()
        #     os.rename(f.name, self.state_file_path)
        #     self._sync(forwardings)
        # finally:
        #     try:
        #         os.remove(f.name)
        #     except:
        #         pass
        with open(self.state_file_path, mode="w") as f:
            f.write(json.dumps(forwardings))
        self._sync(forwardings)

    def _read_state_file(self):
        if not os.path.isfile(self.state_file_path):
            return []

        with open(self.state_file_path) as f:
            forwardings = json.load(f)
            return forwardings

    def add(self, forwarding):
        with self.lock:
            forwardings = self._read_state_file()
            forwardings.append(forwarding)
            self._update_state_file(forwardings)

    def remove(self, source_port, protocol):
        with self.lock:
            forwardings = self._read_state_file()
            try:
                idx = [(f["source_port"], f["protocol"]) for f in forwardings].index(
                    (source_port, protocol)
                )
                del forwardings[idx]
            except ValueError:
                pass
            self._update_state_file(forwardings)

    def get_forwardings(self):
        with self.lock:
            return self._read_state_file()

    def get_forwarding(self, source_port, protocol):
        forwardings = self.get_forwardings()
        for forwarding in forwardings:
            if forwarding["source_port"] == source_port and forwarding["protocol"] == protocol:
                return forwarding
        return None

    def _sync(self, forwardings):
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

    def sync(self):
        self.lock.acquire()
        forwardings = self._read_state_file()
        self._sync(forwardings)
        self.lock.release()


# FIXME: limit to eno2 src port
def _forwarding_to_nat_rule(f):
    prot = f["protocol"]
    return {
        "protocol": prot,
        prot: {
            "dport": str(f["source_port"]),
        },
        "target": {
            "DNAT": {
                "to_destination": f"{f['target_ip']}:{f['target_port']}",
            }
        },
    }


# FIXME: limit to eno2 src port
def _forwarding_to_fwd_rule(f):
    prot = f["protocol"]
    return {
        "protocol": prot,
        "out-interface": "virbr0",
        "dst": f["target_ip"],
        prot: {
            "dport": str(f["target_port"]),
        },
        "target": "ACCEPT",
    }
