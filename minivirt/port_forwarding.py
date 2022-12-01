import ipaddress
import threading

import nftables
from sqlalchemy import select

from minivirt import domain_pb2
from minivirt.models import PortForwarding


class IPTablesPortForwardingSynchronizer:
    def __init__(self, controller, dns_addr=None):
        self.controller = controller
        self.lock = threading.Lock()
        self.dns_addr = dns_addr

    def handle_sync(self, session):
        with self.lock:
            forwardings = session.execute(select(PortForwarding).filter()).scalars().all()

            table_name = "restvirt"

            nft = nftables.Nftables()
            nft.set_json_output(True)

            rfc1918_nets = [
                {
                    "prefix": {
                        "addr": str(net.network_address),
                        "len": net.prefixlen,
                    }
                }
                for net in [
                    ipaddress.ip_network(n)
                    for n in ["192.168.0.0/16", "172.16.0.0/12", "10.0.0.0/8"]
                ]
            ]
            commands = [
                {"add": {"table": {"name": table_name, "family": "inet"}}},
                {"delete": {"table": {"name": table_name, "family": "inet"}}},
                {"add": {"table": {"name": table_name, "family": "inet"}}},
                {
                    "add": {
                        "chain": {
                            "table": table_name,
                            "family": "inet",
                            "name": "output",
                            "type": "nat",
                            "hook": "output",
                            "prio": -105,
                            "policy": "accept",
                        }
                    }
                },
                {
                    "add": {
                        "chain": {
                            "table": table_name,
                            "family": "inet",
                            "name": "prerouting",
                            "type": "nat",
                            "hook": "prerouting",
                            "prio": -105,
                            "policy": "accept",
                        }
                    }
                },
                {
                    "add": {
                        "chain": {
                            "table": table_name,
                            "family": "inet",
                            "name": "postrouting",
                            "type": "nat",
                            "hook": "postrouting",
                            "prio": 95,
                            "policy": "accept",
                        }
                    }
                },
                {
                    "add": {
                        "chain": {
                            "table": table_name,
                            "family": "inet",
                            "name": "forward",
                            "type": "filter",
                            "hook": "forward",
                            "prio": -5,
                            "policy": "accept",
                        }
                    }
                },
                {
                    "add": {  # ensure rfc1918 rules
                        "rule": {
                            "table": table_name,
                            "family": "inet",
                            "chain": "postrouting",
                            "expr": [
                                {
                                    "match": {
                                        "op": "in",
                                        "left": {"payload": {"protocol": "ip", "field": "saddr"}},
                                        "right": {"set": rfc1918_nets},
                                    }
                                },
                                {
                                    "match": {
                                        "op": "!=",
                                        "left": {"payload": {"protocol": "ip", "field": "daddr"}},
                                        "right": {"set": rfc1918_nets},
                                    }
                                },
                                {
                                    "counter": None,
                                },
                                {"masquerade": None},
                            ],
                        }
                    }
                },
            ]

            # FIXME: limit to eno2 src port
            for f in forwardings:
                commands.extend(
                    [
                        {
                            "add": {
                                "rule": {
                                    "table": table_name,
                                    "family": "inet",
                                    "chain": "output",
                                    "expr": [
                                        {
                                            "match": {
                                                "op": "==",
                                                "left": {
                                                    "payload": {
                                                        "protocol": f.protocol,
                                                        "field": "dport",
                                                    }
                                                },
                                                "right": f.source_port,
                                            }
                                        },
                                        {
                                            "counter": None,
                                        },
                                        {
                                            "dnat": {
                                                "family": "ip",
                                                "addr": f.target_ip,
                                                "port": f.target_port,
                                            }
                                        },
                                    ],
                                }
                            }
                        },
                        {
                            "add": {
                                "rule": {
                                    "table": table_name,
                                    "family": "inet",
                                    "chain": "prerouting",
                                    "expr": [
                                        {
                                            "match": {
                                                "op": "==",
                                                "left": {
                                                    "payload": {
                                                        "protocol": f.protocol,
                                                        "field": "dport",
                                                    }
                                                },
                                                "right": f.source_port,
                                            }
                                        },
                                        {
                                            "counter": None,
                                        },
                                        {
                                            "dnat": {
                                                "family": "ip",
                                                "addr": f.target_ip,
                                                "port": f.target_port,
                                            }
                                        },
                                    ],
                                }
                            }
                        },
                        {
                            "add": {
                                "rule": {
                                    "table": table_name,
                                    "family": "inet",
                                    "chain": "forward",
                                    "expr": [
                                        {
                                            "match": {
                                                "op": "==",
                                                "left": {
                                                    "payload": {
                                                        "protocol": f.protocol,
                                                        "field": "dport",
                                                    }
                                                },
                                                "right": f.target_port,
                                            }
                                        },
                                        {
                                            "match": {
                                                "op": "==",
                                                "left": {
                                                    "payload": {"protocol": "ip", "field": "daddr"}
                                                },
                                                "right": f.target_ip,
                                            }
                                        },
                                        {
                                            "counter": None,
                                        },
                                        {"accept": None},
                                    ],
                                }
                            }
                        },
                    ]
                )

            networks = self.controller.ListNetworks(domain_pb2.ListNetworksRequest()).networks
            for network in networks:
                net = ipaddress.IPv4Network(network.cidr)

                dns_addr = self.dns_addr
                if self.dns_addr is None:
                    dns_addr = str(net[1])

                for chain in ["output", "prerouting"]:
                    for protocol in ["udp", "tcp"]:
                        commands.extend(
                            [
                                {
                                    "add": {  # FIXME: use configurable target IP:port
                                        "rule": {
                                            "table": table_name,
                                            "family": "inet",
                                            "chain": chain,
                                            "expr": [
                                                {
                                                    "match": {
                                                        "op": "==",
                                                        "left": {
                                                            "payload": {
                                                                "protocol": protocol,
                                                                "field": "dport",
                                                            }
                                                        },
                                                        "right": 53,
                                                    }
                                                },
                                                {
                                                    "match": {
                                                        "op": "==",
                                                        "left": {
                                                            "payload": {
                                                                "protocol": "ip",
                                                                "field": "daddr",
                                                            }
                                                        },
                                                        "right": str(net[1]),
                                                    }
                                                },
                                                {
                                                    "counter": None,
                                                },
                                                {
                                                    "dnat": {
                                                        "family": "ip",
                                                        "addr": dns_addr,
                                                        "port": 5354,
                                                    }
                                                },
                                            ],
                                        }
                                    }
                                },
                            ]
                        )

            rc, output, error = nft.json_cmd({"nftables": commands})
            # FIXME: replace with logging
            print(rc)
            print(output)
            print(error)
            assert rc == 0
