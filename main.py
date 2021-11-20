import argparse
import logging
import os
import re
import string
from concurrent import futures
from timeit import default_timer as timer

import grpc
import libvirt
import xmltodict
from google.protobuf import empty_pb2
from grpc_reflection.v1alpha import reflection
from sqlalchemy import create_engine, event
from sqlalchemy.engine import Engine
from sqlalchemy.orm import scoped_session, sessionmaker
from sqlalchemy.pool import StaticPool

import dns_pb2_grpc
import domain_pb2
import domain_pb2_grpc
import port_forwarding_pb2_grpc
import route_pb2_grpc
import volume_pb2
import volume_pb2_grpc
from dns import DNSController, DNSService
from domain import DomainService
from models import Base
from port_forwarding import IPTablesPortForwardingSynchronizer, PortForwardingService
from route import (
    GenericRouteController,
    GenericRouteTableController,
    IPRouteSynchronizer,
    IPRouteTableSynchronizer,
    RouteService,
)

libvirt.registerErrorHandler(lambda u, e: None, None)


def _volume_to_dict(vol):
    _, cap, _ = vol.info()
    name = vol.name()
    return {
        "id": name,
        "name": name,
        "size": cap,
    }


def _get_attachments(domain):
    domain_dict = xmltodict.parse(domain.XMLDesc())
    disks = domain_dict["domain"]["devices"]["disk"]
    volume_ids = [
        d["alias"]["@name"][3:]
        for d in disks
        if d["@device"] == "disk" and d["alias"]["@name"].startswith("ua-")
    ]
    attachments = [
        {
            "volume_id": vid,
            "disk_address": _disk_address(domain_dict, vid),
        }
        for vid in volume_ids
    ]

    return attachments


def _get_all_attachments(domains, vol):
    vol_id = vol.name()
    attachments = [(domain, _get_attachments(domain)) for domain in domains]
    attachments = [(d, da) for d, das in attachments for da in das]
    filtered_domains = [(d, da) for (d, da) in attachments if da["volume_id"] == vol_id]
    filtered_domains = [
        {"domain_id": d.UUIDString(), "disk_address": da["disk_address"]}
        for d, da in filtered_domains
    ]
    return filtered_domains


def _disk_address(domain_dict, volume_id):
    disks = domain_dict["domain"]["devices"]["disk"]

    da = [d["address"] for d in disks if d["alias"]["@name"] == f"ua-{volume_id}"][0]
    daddr = [int(da[f"@{k}"], 16) for k in ["domain", "bus", "slot", "function"]]
    return f"{da['@type']}-{daddr[0]:04x}:{daddr[1]:02x}:{daddr[2]:02x}.{daddr[3]:x}"


def disk_address(domain, volume_id):
    domain_dict = xmltodict.parse(domain.XMLDesc())
    return _disk_address(domain_dict, volume_id)


class VolumeService(volume_pb2_grpc.VolumeServiceServicer):
    def __init__(self):
        self.conn = libvirt.open("qemu:///system?socket=/var/run/libvirt/libvirt-sock")

    def GetVolume(self, request, context):
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(id)
        return volume_pb2.Volume(**_volume_to_dict(vol))

    def ListVolumes(self, request, context):
        pool = self.conn.storagePoolLookupByName("volumes")
        vols = pool.listAllVolumes()
        vol_dicts = [_volume_to_dict(vol) for vol in vols]
        return volume_pb2.ListVolumesResponse(volumes=[volume_pb2.Volume(**d) for d in vol_dicts])

    def CreateVolume(self, request, context):
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.createXML(
            f"""<volume>
  <name>{request.volume.name}</name>
  <capacity unit='bytes'>{request.volume.size}</capacity>
  <target>
    <format type='qcow2'/>
  </target>
</volume>"""
        )
        return volume_pb2.Volume(
            id=vol.name(),
            name=request.volume.name,
            size=request.volume.size,
        )

    def DeleteVolume(self, request, context):
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(id)
        if _get_all_attachments(self.conn.listAllDomains(), vol):
            raise Exception("volume is attached z")
        vol.delete()

    def ListVolumeAttachments(self, request, context):
        domain = self.conn.lookupByUUIDString(request.domain_id)
        return volume_pb2.ListVolumeAttachmentsResponse(
            attachments=[
                volume_pb2.VolumeAttachment(domain_id=request.domain_id, **a)
                for a in _get_attachments(domain)
            ]
        )

    def GetVolumeAttachment(self, request, context):
        domain = self.conn.lookupByUUIDString(request.domain_id)
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(request.volume_id)

        return volume_pb2.VolumeAttachment(
            domain_id=domain.UUIDString(),
            volume_id=vol.name(),
            disk_address=disk_address(domain, request.volume_id),
        )

    def AttachVolume(self, request, context):
        domain = self.conn.lookupByUUIDString(request.domain_id)
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(request.volume_id)

        domain_dict = xmltodict.parse(domain.XMLDesc())
        disks = domain_dict["domain"]["devices"]["disk"]

        volumes_ids = [
            d["alias"]["@name"][3:]
            for d in disks
            if d["@device"] == "disk" and d["alias"]["@name"].startswith("ua-")
        ]

        if request.volume_id in volumes_ids:
            return volume_pb2.VolumeAttachment(
                domain_id=request.domain_id,
                volume_id=request.volume_id,
                disk_address=disk_address(domain, request.volume_id),
            )

        disk_shortnames = [d["target"]["@dev"][-1:] for d in disks]
        disk_letter = sorted(set(string.ascii_lowercase).difference(disk_shortnames))[0]
        domain.attachDeviceFlags(
            f"""<disk type='file' device='disk'>
   <driver name='qemu' type='qcow2'/>
   <source file='{vol.path()}'/>
   <target dev='vd{disk_letter}' bus='virtio'/>
   <alias name='ua-{request.volume_id}'/>
 </disk>
 """,
            libvirt.VIR_DOMAIN_AFFECT_LIVE | libvirt.VIR_DOMAIN_AFFECT_CONFIG,
        )

        return volume_pb2.VolumeAttachment(
            domain_id=request.domain_id,
            volume_id=request.volume_id,
            disk_address=disk_address(domain, request.volume_id),
        )

    def DetachVolume(self, request, context):
        domain = self.conn.lookupByUUIDString(request.domain_id)
        try:
            domain.detachDeviceAlias(
                f"ua-{request.volume_id}",
                libvirt.VIR_DOMAIN_AFFECT_LIVE | libvirt.VIR_DOMAIN_AFFECT_CONFIG,
            )
        except:
            # TODO: check for string "no device found with alias"
            pass


class UnaryUnaryInterceptor(grpc.ServerInterceptor):
    def intercept_service(self, continuation, handler_call_details):
        next = continuation(handler_call_details)
        if next is None:
            return None
        if next.unary_unary is None:
            return next

        def letsgo(request, context):
            start = timer()
            try:
                response = next.unary_unary(request, context)
            except libvirt.libvirtError as e:
                status_code = grpc.StatusCode.INTERNAL
                if e.get_error_code() in [
                    libvirt.VIR_ERR_NO_DOMAIN,
                    libvirt.VIR_ERR_NO_STORAGE_VOL,
                ]:
                    status_code = grpc.StatusCode.NOT_FOUND
                context.set_code(status_code)
                context.set_details(f"{e} ({e.get_error_code()})")
                response = empty_pb2.Empty()

            logging.debug(f"{handler_call_details.method} [{(timer() - start)*1000:.3f} ms]")
            return response

        return grpc.unary_unary_rpc_method_handler(
            letsgo,
            request_deserializer=next.request_deserializer,
            response_serializer=next.response_serializer,
        )


def ensure_rfc1918_rules():
    import iptc
    from pyroute2 import IPSet

    with IPSet() as ips:
        try:
            ipset = ips.list("restvirt")[0]
        except:
            ips.create("restvirt", "hash:net")
            ipset = ips.list("restvirt")[0]

        addrs = ipset.get_attr("IPSET_ATTR_ADT").get_attrs("IPSET_ATTR_DATA")
        nets = set()
        for addr in addrs:
            ipv4 = addr.get_attr("IPSET_ATTR_IP_FROM").get_attr("IPSET_ATTR_IPADDR_IPV4")
            netbits = addr.get_attr("IPSET_ATTR_CIDR")
            nets.add(f"{ipv4}/{netbits}")

        rfc1918_nets = {"192.168.0.0/16", "172.16.0.0/12", "10.0.0.0/8"}
        for net in nets:
            if str(net) not in rfc1918_nets:
                ips.delete("restvirt", str(net), etype="net")
        for addr in rfc1918_nets:
            if addr not in nets:
                ips.add("restvirt", addr, etype="net")

    rule_exists = False
    for rule in iptc.easy.dump_chain("nat", "POSTROUTING"):
        if "set" in rule and rule["target"] == "MASQUERADE":
            if rule["set"] == [
                {"match-set": ["restvirt", "src"]},
                {"match-set": ["!", "restvirt", "dst"]},
            ]:
                rule_exists = True
                break

    if not rule_exists:
        iptc.easy.insert_rule(
            "nat",
            "POSTROUTING",
            {
                "set": [
                    {"match-set": ["restvirt", "src"]},
                    {"match-set": ["!", "restvirt", "dst"]},
                ],
                "target": "MASQUERADE",
            },
        )


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.DEBUG, format="%(asctime)s %(levelname)s:%(name)s:%(message)s"
    )

    p = re.compile(r"^(\S*):(\d+)$")

    def bind_address(s):
        match = p.match(s)
        if not match:
            raise argparse.ArgumentTypeError("invalid bind address: " + s)
        res = match.group(1), int(match.group(2))
        return res

    parser = argparse.ArgumentParser(
        description="restvirt", formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("--debug", action="store_true", help="run in debug mode")
    parser.add_argument(
        "-b", "--bind", type=bind_address, default=":8090", help="server bind address"
    )
    parser.add_argument("-c", "--config", default="/etc/restvirt", help="configuration folder")
    parser.add_argument("--server-cert")
    parser.add_argument("--server-key")
    parser.add_argument("--client-ca-cert")
    args = parser.parse_args()

    logging.debug(args)

    host, port = args.bind
    host = host or "0.0.0.0"

    if args.debug:
        logging.getLogger("sqlalchemy.engine").setLevel(logging.INFO)

    @event.listens_for(Engine, "connect")
    def set_sqlite_pragma(dbapi_connection, connection_record):
        cursor = dbapi_connection.cursor()
        cursor.execute("PRAGMA foreign_keys=ON")
        cursor.close()

    engine = create_engine(
        f"sqlite:///{os.path.join(args.config, 'controller.sqlite3')}",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
        future=True,
    )
    Base.metadata.create_all(engine)
    session_factory = scoped_session(sessionmaker(engine, future=True))

    ensure_rfc1918_rules()

    dns_controller = DNSController(session_factory)
    dns_controller.start()

    server = grpc.server(
        futures.ThreadPoolExecutor(max_workers=10), interceptors=[UnaryUnaryInterceptor()]
    )
    dns_pb2_grpc.add_DNSServicer_to_server(DNSService(dns_controller), server)
    domain_pb2_grpc.add_DomainServiceServicer_to_server(DomainService(), server)
    port_forwarding_service = PortForwardingService(
        session_factory, IPTablesPortForwardingSynchronizer()
    )
    port_forwarding_service.sync()
    port_forwarding_pb2_grpc.add_PortForwardingServiceServicer_to_server(
        port_forwarding_service, server
    )
    route_table_controller = GenericRouteTableController(
        session_factory, IPRouteTableSynchronizer()
    )
    route_table_controller.sync()
    route_controller = GenericRouteController(session_factory, IPRouteSynchronizer())
    route_controller.sync()
    route_pb2_grpc.add_RouteServiceServicer_to_server(
        RouteService(route_table_controller, route_controller), server
    )
    volume_pb2_grpc.add_VolumeServiceServicer_to_server(VolumeService(), server)

    server_key_pair_provided = args.server_cert is not None and args.server_key is not None
    assert server_key_pair_provided or (args.server_cert is None and args.server_key is None)
    assert args.client_ca_cert is None or server_key_pair_provided

    if server_key_pair_provided:
        with open(args.server_cert, "rb") as cert, open(args.server_key, "rb") as key:
            key_pair = (key.read(), cert.read())

        root_certificate = None
        require_client_auth = args.client_ca_cert is not None
        if require_client_auth:
            with open(args.client_ca_cert, "rb") as ca_cert:
                root_certificate = ca_cert.read()

        creds = grpc.ssl_server_credentials(
            [key_pair],
            root_certificates=root_certificate,
            require_client_auth=require_client_auth,
        )

        server.add_secure_port(f"{host}:{port}", creds)
    else:
        server.add_insecure_port(f"{host}:{port}")
    reflection.enable_server_reflection(
        [
            service_descriptor.full_name
            for service_descriptor in domain_pb2.DESCRIPTOR.services_by_name.values()
        ]
        + [reflection.SERVICE_NAME],
        server,
    )
    server.start()
    server.wait_for_termination()
