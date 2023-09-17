import ipaddress
import os
import string
import threading
import uuid
from datetime import datetime, timezone

import libvirt
import requests
import xmltodict
from google.protobuf import empty_pb2, timestamp_pb2
from grpc import StatusCode
from pyroute2 import IPRoute
from pyroute2.netlink.rtnl import rtypes
from requests.exceptions import ConnectionError, HTTPError
from sqlalchemy import delete, select

from minivirt import (
    controller_pb2_grpc,
    daemon_pb2_grpc,
    domain_pb2,
    port_forwarding_pb2,
    route_pb2,
    volume_pb2,
)
from minivirt.image import create_cloud_config_image
from minivirt.models import Domain, PortForwarding


def libvirt_state_to_string(state):
    if state == libvirt.VIR_DOMAIN_NOSTATE:
        return "NOSTATE"
    elif state == libvirt.VIR_DOMAIN_RUNNING:
        return "RUNNING"
    elif state == libvirt.VIR_DOMAIN_BLOCKED:
        return "BLOCKED"
    elif state == libvirt.VIR_DOMAIN_PAUSED:
        return "PAUSED"
    elif state == libvirt.VIR_DOMAIN_SHUTDOWN:
        return "SHUTDOWN"
    elif state == libvirt.VIR_DOMAIN_CRASHED:
        return "CRASHED"
    elif state == libvirt.VIR_DOMAIN_PMSUSPENDED:
        return "PMSUSPENDED"
    elif state == libvirt.VIR_DOMAIN_SHUTOFF:
        return "SHUTOFF"
    return "UNKNOWN"


def domain_to_dict(domain):
    domain_dict = xmltodict.parse(domain.XMLDesc(libvirt.VIR_DOMAIN_XML_INACTIVE))
    d = domain_dict["domain"]
    res = {
        "uuid": d["uuid"],
        "name": d["name"],
        "vcpu": int(d["vcpu"]["#text"]),
        "memory": int(d["memory"]["#text"]) // 1024,
        "nested_virtualization": d["cpu"]["@mode"] == "host-model",
    }
    try:
        res["network"] = d["devices"]["interface"]["source"]["@network"]
    except:
        pass
    try:  # FIXME: once all have create metadata, remove this try/catch
        dt = datetime.fromisoformat(d["metadata"]["restvirt:metadata"]["created"])
        res["created_at"] = timestamp_pb2.Timestamp(
            seconds=int(dt.timestamp())
        )  # we don't need sub-second precision
    except Exception as e:
        print(e)
    return res


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


class IPRouteTableSynchronizer:
    id_range_min, id_range_max = 30069, 30169

    def __init__(self):
        self.lock = threading.Lock()

    def handle_sync(self, client: controller_pb2_grpc.ControllerServiceStub):
        with self.lock, IPRoute() as ip:
            tables = client.ListRouteTables(route_pb2.ListRouteTablesRequest()).route_tables
            table_ids = {t.id for t in tables}

            def filt(r):
                prio = r.get_attr("FRA_PRIORITY")
                return prio is not None and self.id_range_min <= prio <= self.id_range_max

            rules = filter(filt, ip.get_rules())
            rules = {r.get_attr("FRA_PRIORITY"): r for r in rules}

            # step 1: delete tables not in conf
            for prio in rules:
                if prio not in table_ids:
                    ip.rule("del", priority=prio)

            # step 2: update tables
            # TODO: this is broken
            for table in tables:
                if table.id not in rules or rules[table.id]["table"] != table.id:
                    try:
                        ip.rule("del", priority=table.id)
                    except:
                        pass
                    ip.rule(
                        "add",
                        priority=table.id,
                        table=table.id,
                    )


# TODO: RouteTableController
# try:
#     # FIXME: make "virbr0" configurable
#     ip.rule(
#         "add", table=TABLE_ID, priority=30069, iifname="virbr0"
#     )  # FIXME: check if rule identical
# except NetlinkError as e:
#     if e.code != 17:  # 17 = rule already exists
#         raise e


class IPRouteSynchronizer:
    id_range_min, id_range_max = 30069, 30169

    def __init__(self):
        self.lock = threading.Lock()

    def get_ip_routes(self, ip):
        filtered_routes = {}
        for r in ip.get_routes():
            table_id = r.get_attr("RTA_TABLE")
            if not self.id_range_min <= table_id < self.id_range_max:
                continue
            if r["family"] != 2 or r["type"] != rtypes["RTN_UNICAST"]:
                continue
            dst = r.get_attr("RTA_DST")
            if dst is None:
                continue
            dstnet = f"{dst}/{r['dst_len']}"
            addr = ipaddress.ip_network(dstnet)

            gateway = r.get_attr("RTA_GATEWAY")
            if gateway is not None:
                gateways = [gateway]
            else:
                routes = r.get_attr("RTA_MULTIPATH")
                gateways = [r.get_attr("RTA_GATEWAY") for r in routes]
            filtered_routes[addr] = (table_id, {ipaddress.ip_address(gw) for gw in gateways})

        return filtered_routes

    def remove_ip_route(self, ip, table_id, dst):
        ip.route("del", table=table_id, dst=str(dst))

    def put_ip_route(self, ip, table_id, dst, gws):
        try:
            ip.route("del", table=table_id, dst=str(dst))
        except:
            pass
        ip.route(
            "add",
            table=table_id,
            dst=str(dst),
            multipath=[{"gateway": str(gw)} for gw in gws],
        )

    def handle_sync(self, client: controller_pb2_grpc.ControllerServiceStub):
        with self.lock, IPRoute() as ip:
            tables = client.ListRouteTables(route_pb2.ListRouteTablesRequest()).route_tables
            routes = [
                client.ListRoutes(route_pb2.ListRoutesRequest(route_table_id=table.id)).routes
                for table in tables
            ]
            routes = [r for rs in routes for r in rs]
            routes = {
                ipaddress.IPv4Network(route.destination): (
                    route.route_table_id,
                    {ipaddress.IPv4Address(gw) for gw in route.gateways},
                )
                for route in routes
            }

            filtered_routes = self.get_ip_routes(ip)

            # step 1: delete routes not in conf
            for dst, (tid, gws) in filtered_routes.items():
                if dst not in routes:
                    self.remove_ip_route(ip, tid, dst)

            # step 2: update routes
            for dst, (tid, gws) in routes.items():
                if dst not in filtered_routes or filtered_routes[dst][1] != gws:
                    self.put_ip_route(ip, tid, dst, gws)


def create_storage_pool(conn, name, pool_dir, location=None):
    if location is None:
        location = name

    try:
        conn.storagePoolLookupByName(name)
    except libvirt.libvirtError as e:
        if e.get_error_code() == libvirt.VIR_ERR_NO_STORAGE_POOL:
            pool = conn.storagePoolDefineXML(
                f"""<pool type="dir">
              <name>{name}</name>
              <target>
                <path>{os.path.join(pool_dir, location)}</path>
              </target>
            </pool>"""
            )
            pool.build()
            pool.create()
        else:
            raise e


def get_root_volume(conn, domain_name):
    try:
        pool = conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(f"{domain_name}-root.qcow2")
    except:
        # FIXME: can be removed once not in use
        pool = conn.storagePoolLookupByName("restvirtimages")
        vol = pool.storageVolLookupByName(f"{domain_name}-root.qcow2")
    return vol


def _network_prefix(net_elem):
    # FIXME: remove/simplify me, once all network use prefix
    if "@netmask" in net_elem:
        return ipaddress.ip_network("0.0.0.0/" + net_elem["@netmask"]).prefixlen
    return net_elem["@prefix"]


def _network_to_pb(net):
    net_dict = xmltodict.parse(net.XMLDesc(), force_list=("ip",))["network"]
    cidr6 = ""
    if len(net_dict["ip"]) > 1:
        cidr6 = str(
            ipaddress.ip_network(
                f'{net_dict["ip"][1]["@address"]}/{_network_prefix(net_dict["ip"][1])}',
                strict=False,
            )
        )
    return domain_pb2.Network(
        uuid=net_dict["uuid"],
        name=net_dict["name"],
        cidr=str(
            ipaddress.ip_network(
                f'{net_dict["ip"][0]["@address"]}/{_network_prefix(net_dict["ip"][0])}',
                strict=False,
            )
        ),
        cidr6=cidr6,
    )


class DaemonService(daemon_pb2_grpc.DaemonServiceServicer):
    def __init__(
        self, session_factory, port_fwd_sync_handler, controller_channel, pool_dir="/data/restvirt"
    ):
        self.session_factory = session_factory
        self.port_fwd_sync_handler = port_fwd_sync_handler
        self.controller = controller_pb2_grpc.ControllerServiceStub(controller_channel)

        self.tables_synchronizer = IPRouteTableSynchronizer()
        self.routes_synchronizer = IPRouteSynchronizer()

        self.conn = libvirt.open("qemu:///system?socket=/var/run/libvirt/libvirt-sock")

        self.lock = threading.Lock()

        create_storage_pool(self.conn, "restvirtimages", pool_dir, "images")
        create_storage_pool(self.conn, "volumes", pool_dir)

    def GetNetwork(self, request, context):
        net = self.conn.networkLookupByUUIDString(request.uuid)
        return _network_to_pb(net)

    def ListNetworks(self, request, context):
        return domain_pb2.ListNetworksResponse(
            networks=[_network_to_pb(net) for net in (self.conn.listAllNetworks())]
        )

    def CreateNetwork(self, request, context):
        network = request.network

        try:
            net_json = {}
            if network.cidr:
                net_json["cidr4"] = network.cidr
            if network.cidr6:
                net_json["cidr6"] = network.cidr6
            res = requests.post(
                "http://localhost:8080/networks",
                json={"network": net_json},
            )
            res.raise_for_status()
            virtuerl_net = res.json()
            net_id = virtuerl_net["id"]
        except HTTPError as e:
            raise e

        return domain_pb2.Network(
            uuid=net_id,
            name="name",
            cidr=network.cidr,
            cidr6=network.cidr6,
        )

    def DeleteNetwork(self, request, context):
        try:
            net = self.conn.networkLookupByUUIDString(request.uuid)
            try:
                net.destroy()
            except:
                pass
            net.undefine()
        except:
            pass

        requests.delete(f"http://localhost:8080/networks/{request.uuid}")

        return empty_pb2.Empty()

    def StartDomain(self, request, context):
        domain = self.conn.lookupByUUIDString(request.uuid)
        domain.create()
        return empty_pb2.Empty()

    def StopDomain(self, request, context):
        domain = self.conn.lookupByUUIDString(request.uuid)
        if request.force:
            domain.destroy()
        else:
            domain.shutdown()
        return empty_pb2.Empty()

    def _get_domain(self, uuid):
        domain_lv = self.conn.lookupByUUIDString(uuid)
        domain_dict = domain_to_dict(domain_lv)
        try:
            net = self.conn.networkLookupByName(domain_dict["network"])
            domain_dict["network"] = net.UUIDString()
        except:
            res = requests.get(f"http://localhost:8080/domains/{uuid}")
            resj = res.json()
            domain_dict["network"] = resj["network_id"]
        state, _ = domain_lv.state()
        domain_dict["state"] = libvirt_state_to_string(state)

        with self.session_factory() as session:
            domain = (
                session.execute(
                    select(Domain).filter(
                        Domain.id == uuid,
                    )
                )
                .scalars()
                .one_or_none()
            )
            domain_dict["os_type"] = domain.os_type
            domain_dict["private_ip"] = domain.private_ip
            domain_dict["ipv6_address"] = domain.ipv6_address
            domain_dict["user_data"] = domain.user_data

        return domain_dict

    def GetDomain(self, request, context):
        return domain_pb2.Domain(**self._get_domain(request.uuid))

    def ListDomains(self, request, context):
        domains = self.conn.listAllDomains()
        ds = [domain_pb2.Domain(**domain_to_dict(d)) for d in domains]
        return domain_pb2.ListDomainsResponse(domains=ds)

    def CreateDomain(self, request, context):
        domreq = request.domain

        req_net = {"domain": {"network_id": domreq.network}}
        private_ip = domreq.private_ip
        if private_ip:
            req_net["domain"]["ipv4_addr"] = private_ip
        ipv6_address = domreq.ipv6_address
        if ipv6_address:
            req_net["domain"]["ipv6_addr"] = ipv6_address

        try:
            res = requests.post("http://localhost:8080/domains", json=req_net)
            res.raise_for_status()
            virtuerl_dom = res.json()
            dom_id = virtuerl_dom["id"]
            tap_name = virtuerl_dom["tap_name"]
            ip_addr = virtuerl_dom["ipv4_addr"]
            if ipv6_address:
                ip6_addr = virtuerl_dom["ipv6_addr"]
        except HTTPError as e:
            raise e

        gateway6 = None
        net6 = None

        try:
            lvnet = self.conn.networkLookupByUUIDString(domreq.network)

            net_dict = xmltodict.parse(lvnet.XMLDesc(), force_list=("ip",))
            net_def = net_dict["network"]["ip"][0]
            gateway = ipaddress.ip_address(net_def["@address"])
            net = ipaddress.ip_network(f"{gateway}/{_network_prefix(net_def)}", strict=False)

            if len(net_dict["network"]["ip"]) > 1:
                net6_def = net_dict["network"]["ip"][1]
                gateway6 = ipaddress.ip_address(net6_def["@address"])
                net6 = ipaddress.ip_network(f'{gateway6}/{net6_def["@prefix"]}', strict=False)

            is_virtuerl_net = False
        except libvirt.libvirtError as e:
            if e.get_error_code() != libvirt.VIR_ERR_NO_NETWORK:
                raise e
            private_ip = ip_addr

            vres = requests.get(f"http://localhost:8080/networks/{domreq.network}")
            vres.raise_for_status()
            vresj = vres.json()
            cidrs = [ipaddress.ip_network(cidr, strict=False) for cidr in vresj["cidrs"]]
            cidr4 = [cidr for cidr in cidrs if isinstance(cidr, ipaddress.IPv4Network)]
            cidr6 = [cidr for cidr in cidrs if isinstance(cidr, ipaddress.IPv6Network)]
            net = cidr4[0]
            gateway = net[1]
            if ipv6_address:
                net6 = cidr6[0]
                gateway6 = net6[1]

            is_virtuerl_net = True

        dom_uuid = dom_id
        os_type = domreq.os_type
        if os_type is None:
            os_type = "linux"
        user_data = domreq.user_data
        if user_data is None:
            user_data = ""
        with self.lock:
            with self.session_factory() as session:
                domains = session.execute(select(Domain)).scalars().all()

                ips = set([dom.private_ip for dom in domains])
                if not private_ip:
                    available = {str(h) for h in net.hosts()}
                    available -= {str(net.network_address), str(net.broadcast_address)}
                    available -= ips
                    private_ip = available.pop()
                else:
                    assert private_ip not in ips

                domain = Domain(
                    id=str(dom_uuid),
                    os_type=os_type,
                    private_ip=private_ip,
                    ipv6_address=ipv6_address,
                    user_data=user_data,
                )
                session.add(domain)
                session.commit()

        ip = ipaddress.ip_address(private_ip)
        ip6 = ipaddress.ip_address(ipv6_address) if ipv6_address else None
        mac = "{:02x}:{:02x}:{:02x}:{:02x}:{:02x}:{:02x}".format(
            0x52, 0x54, 0x00, ip.packed[-3], ip.packed[-2], ip.packed[-1]
        )

        if is_virtuerl_net:
            netxml = f"""<interface type='ethernet'>
  <mac address='{mac}'/>
  <target dev='{tap_name}' managed='no'/>
  <model type='virtio'/>
</interface>
"""
        else:
            netxml = f"""<interface type='network'>
  <mac address='{mac}'/>
  <source network='{lvnet.name()}'/>
  <model type='virtio'/>
</interface>
"""

        img_pool = self.conn.storagePoolLookupByName("restvirtimages")
        if not domreq.base_image:
            base_img_name = "debian-12-generic-amd64-20240102-1614.qcow2"
            try:
                base_img = img_pool.storageVolLookupByName(base_img_name)
            except:
                from urllib.request import urlopen

                res = urlopen(
                    f"https://cloud.debian.org/images/cloud/bookworm/20240102-1614/debian-12-generic-amd64-20240102-1614.qcow2"
                )
                size = int(res.getheader("Content-length"))
                base_img = img_pool.createXML(
                    f"""<volume type='file'>
  <name>{base_img_name}</name>
  <capacity unit='B'>{size}</capacity>
  <target>
    <format type='qcow2'/>
  </target>
</volume>"""
                )
                stream = self.conn.newStream()
                base_img.upload(stream, 0, size)
                while True:
                    chunk = res.read(64 * 1024)
                    if not chunk:
                        break
                    stream.send(chunk)
                stream.finish()
        else:
            base_img = img_pool.storageVolLookupByName(domreq.base_image)

        vol_pool = self.conn.storagePoolLookupByName("volumes")
        vol = vol_pool.createXML(
            f"""<volume type='file'>
  <name>{domreq.name}-root.qcow2</name>
  <capacity unit='GiB'>{20}</capacity>
  <backingStore>
    <path>{base_img.path()}</path>
    <format type='qcow2'/>
  </backingStore>
  <target>
    <format type='qcow2'/>
  </target>
</volume>"""
        )
        root_image_path = vol.path()

        ccfg_raw = create_cloud_config_image(
            domain_id=dom_uuid,
            user_data=user_data,
            mac=mac,
            network=net,
            network6=net6,
            address=ip,
            address6=ip6,
            gateway=net[1],
            gateway6=gateway6,
            name=domreq.name,
        )
        stream = self.conn.newStream()
        ccfg_vol = img_pool.createXML(
            f"""<volume type='file'>
  <name>{domreq.name}-cloud-init.img</name>
  <capacity unit='B'>{len(ccfg_raw)}</capacity>
  <target>
    <format type='raw'/>
  </target>
</volume>"""
        )
        ccfg_vol.upload(stream, 0, len(ccfg_raw))
        stream.send(ccfg_raw)
        stream.finish()

        creation_timestamp = datetime.now(timezone.utc)
        dom = self.conn.defineXML(
            f"""<domain type='kvm'>
  <uuid>{dom_uuid}</uuid>
  <metadata>
    <restvirt:metadata xmlns:restvirt="https://restvirt.io/xml">
        <created>{creation_timestamp.isoformat()}</created>
    </restvirt:metadata>
  </metadata>
  <features>
    <acpi/>
  </features>
  {"<cpu mode='host-model'></cpu>" if domreq.nested_virtualization else ""}
  <name>{domreq.name}</name>
  <vcpu>{domreq.vcpu}</vcpu>
  <memory unit='MiB'>{domreq.memory}</memory>
  <os>
    <type>hvm</type>
  </os>
  <devices>
    <disk type='file' device='disk'>
      <driver name='qemu' type='qcow2'/>
      <source file='{root_image_path}'/>
      <target dev='vda' bus='virtio'/>
    </disk>
    <disk type='file' device='cdrom'>
      <driver name='qemu' type='raw'/>
      <source file='{ccfg_vol.path()}'/>
      <target dev='vde' bus='sata'/>
    </disk>
    {netxml}
    <console type='pty'/>
  </devices>
</domain>"""
        )

        dom.setAutostart(True)
        dom.create()

        return domain_pb2.Domain(**self._get_domain(dom.UUIDString()))

    def DeleteDomain(self, request, context):
        with self.session_factory() as session:
            session.execute(delete(Domain).where(Domain.id == request.uuid))
            session.commit()

        dom = self.conn.lookupByUUIDString(str(request.uuid))
        name = dom.name()
        try:
            dom.destroy()
        except libvirt.libvirtError as e:
            if "Requested operation is not valid: domain is not running" in str(e):
                pass
            else:
                raise e
        dom.undefine()

        pool = self.conn.storagePoolLookupByName("restvirtimages")
        vol = get_root_volume(self.conn, name)
        vol.delete()
        vol = pool.storageVolLookupByName(f"{name}-cloud-init.img")
        vol.delete()

        requests.delete(f"http://localhost:8080/domains/{str(request.uuid)}")

        return empty_pb2.Empty()

    def DownloadImage(self, request, context):
        dom = self.conn.lookupByUUIDString(request.domain_id)
        name = dom.name()
        vol = get_root_volume(self.conn, name)
        stream = self.conn.newStream()
        vol.download(stream, 0, 0)
        while True:
            bytes = stream.recv(64 * 1024)
            if not bytes:
                break
            yield domain_pb2.ImageChunk(bytes=bytes)
        stream.finish()

    def GetVolume(self, request, context):
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(request.uuid)
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

    def UpdateVolume(self, request, context):
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(request.volume.id)
        _, current_capacity, _ = vol.info()
        desired_capacity = request.volume.size
        if desired_capacity < current_capacity:
            raise Exception("shrinking volumes is not supported")
        vol.resize(desired_capacity)
        return volume_pb2.Volume(**_volume_to_dict(vol))

    def DeleteVolume(self, request, context):
        pool = self.conn.storagePoolLookupByName("volumes")
        vol = pool.storageVolLookupByName(request.uuid)
        if _get_all_attachments(self.conn.listAllDomains(), vol):
            raise Exception("volume is attached z")
        vol.delete()
        return empty_pb2.Empty()

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
        alias = f"ua-{request.volume_id}"
        try:
            domain.detachDeviceAlias(
                alias,
                libvirt.VIR_DOMAIN_AFFECT_LIVE | libvirt.VIR_DOMAIN_AFFECT_CONFIG,
            )
        except libvirt.libvirtError as e:
            if f"no device found with alias {alias}" not in e.get_error_message():
                raise e
        return empty_pb2.Empty()

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
            return empty_pb2.Empty()
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
            self.port_fwd_sync_handler.handle_sync(session, self.conn)

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
            self.port_fwd_sync_handler.handle_sync(session, self.conn)

        return empty_pb2.Empty()

    def sync(self):
        with self.session_factory() as session:
            self.port_fwd_sync_handler.handle_sync(session, self.conn)

    def SyncRoutes(self, request, context):
        self.tables_synchronizer.handle_sync(self.controller)
        self.routes_synchronizer.handle_sync(self.controller)
        return empty_pb2.Empty()
