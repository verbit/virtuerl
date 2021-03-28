import json
import os
import threading

import libvirt
import xmltodict


class DNSController:
    def __init__(self, net, state_dir, state_file_name="dns.json"):
        self.net = net
        self.lock = threading.Lock()
        self.state_dir = state_dir
        self.state_file_path = os.path.join(state_dir, state_file_name)

    def _update_state_file(self, mappings):

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
            f.write(json.dumps(mappings))
        self._sync(mappings)

    def _read_state_file(self):
        if not os.path.isfile(self.state_file_path):
            return {}

        with open(self.state_file_path) as f:
            mappings = json.load(f)
            return mappings

    def set(self, name, ip):
        with self.lock:
            mappings = self._read_state_file()
            mappings[name] = ip
            self._update_state_file(mappings)

    def remove(self, name):
        with self.lock:
            mappings = self._read_state_file()
            try:
                del mappings[name]
            except ValueError:
                pass
            self._update_state_file(mappings)

    def get_mappings(self):
        with self.lock:
            return self._read_state_file()

    def get_mapping(self, name):
        mappings = self.get_mappings()
        return mappings.get(name)

    def _sync(self, mappings):
        # step 1: remove all dns mappings in libvirt
        d = xmltodict.parse(self.net.XMLDesc(), force_list=["host", "hostname"])
        dns_entry = d["network"].get("dns")
        if dns_entry is not None:
            libvirt_mappings = {}
            for m in dns_entry["host"]:
                hostnames = libvirt_mappings.setdefault(m["@ip"], [])
                hostnames += m["hostname"]

            for ip in libvirt_mappings:
                self.net.update(
                    libvirt.VIR_NETWORK_UPDATE_COMMAND_DELETE,
                    libvirt.VIR_NETWORK_SECTION_DNS_HOST,
                    -1,
                    f"<host ip='{ip}'/>",
                    libvirt.VIR_NETWORK_UPDATE_AFFECT_LIVE
                    | libvirt.VIR_NETWORK_UPDATE_AFFECT_CONFIG,
                )

        # step 2: set dns mappings from state file
        libvirt_mappings = {}
        for host, ip in mappings.items():
            libvirt_mappings.setdefault(ip, []).append(host)

        for ip, hosts in libvirt_mappings.items():
            hostnames_xml = "".join(
                [f"<hostname>{name}</hostname>" for name in set(libvirt_mappings[ip])]
            )
            self.net.update(
                libvirt.VIR_NETWORK_UPDATE_COMMAND_ADD_LAST,
                libvirt.VIR_NETWORK_SECTION_DNS_HOST,
                -1,
                f"<host ip='{ip}'>{hostnames_xml}</host>",
                libvirt.VIR_NETWORK_UPDATE_AFFECT_LIVE | libvirt.VIR_NETWORK_UPDATE_AFFECT_CONFIG,
            )

    def sync(self):
        with self.lock:
            mappings = self._read_state_file()
            self._sync(mappings)
