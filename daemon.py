import ipaddress
import os
import string
import threading
import uuid
from datetime import datetime, timezone

import libvirt
import xmltodict
from google.protobuf import empty_pb2, timestamp_pb2
from grpc import StatusCode
from pyroute2 import IPRoute
from pyroute2.netlink.rtnl import rtypes
from sqlalchemy import delete, select

import controller_pb2_grpc
import daemon_pb2_grpc
import domain_pb2
import port_forwarding_pb2
import route_pb2
import volume_pb2
from image import (
    create_cloud_config_image,
    read_ip_from_cloud_config_image,
    read_user_data_from_cloud_config_image,
)
from models import PortForwarding


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
        "network": d["devices"]["interface"]["source"]["@network"],
        "nested_virtualization": d["cpu"]["@mode"] == "host-model",
    }
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


class NetworkSynchronizer:
    import libvirt

    def __init__(
        self,
        controller: controller_pb2_grpc.ControllerServiceStub,
        libvirt_connection: libvirt.virConnect,
    ):
        self.controller = controller
        self.libvirt_connection = libvirt_connection

    def sync(self):
        # net = self.conn.networkLookupByUUIDString(request.uuid)
        # net_dict = xmltodict.parse(net.XMLDesc())["network"]
        # net_def = net_dict["ip"]
        # gateway = ipaddress.IPv4Address(net_def["@address"])
        # net = ipaddress.IPv4Network(f'{gateway}/{net_def["@netmask"]}', strict=False)
        #
        # return domain_pb2.Network(
        #     uuid=request.uuid,
        #     name=net_dict["name"],
        #     cidr=net.with_prefixlen,
        # )

        lv_networks = self.libvirt_connection.listAllNetworks()
        lv_network_names_map = {n.name(): n for n in lv_networks}
        networks = self.controller.ListNetworks(domain_pb2.ListNetworksRequest()).networks
        network_names_map = {n.name: n for n in networks}

        for lv_network_name, lv_network in lv_network_names_map.items():
            if lv_network_name not in network_names_map:
                lv_network.destroy()
                lv_network.undefine()

        for network_name, network in network_names_map.items():
            if network_name not in lv_network_names_map:
                net = ipaddress.IPv4Network(network.cidr)
                # TODO: forwarder domain addr should point to the controller
                lvnet = self.libvirt_connection.networkDefineXML(
                    f"""<network>
  <name>{network.name}</name>
  <forward mode='open'/>
  <bridge stp='on' delay='0'/>
  <dns enable='no'>
  </dns>
  <ip address='{net[1]}' netmask='{net.netmask}'>
  </ip>
</network>
"""
                )
                lvnet.create()
                lvnet.setAutostart(True)
                # network.uuid = lvnet.UUIDString()


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
        self.network_synchronizer = NetworkSynchronizer(self.controller, self.conn)

        self.lock = threading.Lock()
        domains = self.conn.listAllDomains()
        self.ips = {self._get_domain(d.UUIDString())["private_ip"] for d in domains}

        create_storage_pool(self.conn, "restvirtimages", pool_dir, "images")
        create_storage_pool(self.conn, "volumes", pool_dir)

    def _get_domain(self, uuid):
        domain = self.conn.lookupByUUIDString(uuid)
        domain_dict = domain_to_dict(domain)
        state, _ = domain.state()
        pool = self.conn.storagePoolLookupByName("restvirtimages")
        vol = pool.storageVolLookupByName(f"{domain_dict['name']}-cloud-init.img")
        stream = self.conn.newStream()
        vol.download(stream, 0, 0)
        vol.info()
        res = bytearray()
        while True:
            bytes = stream.recv(64 * 1024)
            res += bytes
            if not len(bytes):
                break
        stream.finish()
        private_ip = read_ip_from_cloud_config_image(res)
        user_data = read_user_data_from_cloud_config_image(res)
        domain_dict["state"] = libvirt_state_to_string(state)
        domain_dict["private_ip"] = private_ip
        domain_dict["user_data"] = user_data
        return domain_dict

    def GetDomain(self, request, context):
        return domain_pb2.Domain(**self._get_domain(request.uuid))

    def ListDomains(self, request, context):
        domains = self.conn.listAllDomains()
        ds = [domain_pb2.Domain(**domain_to_dict(d)) for d in domains]
        return domain_pb2.ListDomainsResponse(domains=ds)

    def CreateDomain(self, request, context):
        domreq = request.domain

        network_name = domreq.network
        if not network_name:
            network_name = "default"  # FIXME: only for backwards-compatibility

        net = self.conn.networkLookupByName(network_name)
        net_dict = xmltodict.parse(net.XMLDesc())
        net_def = net_dict["network"]["ip"]
        gateway = ipaddress.IPv4Address(net_def["@address"])
        net = ipaddress.IPv4Network(f'{gateway}/{net_def["@netmask"]}', strict=False)

        private_ip = domreq.private_ip
        if not private_ip:
            available = {str(h) for h in net.hosts()}
            available -= {str(net.network_address), str(net.broadcast_address)}
            with self.lock:
                print(self.ips)
                available -= self.ips
                private_ip = available.pop()
                self.ips.add(private_ip)
                print(self.ips)
        else:
            with self.lock:
                self.ips.add(private_ip)

        ip = ipaddress.ip_address(private_ip)

        pool = self.conn.storagePoolLookupByName("restvirtimages")
        if not domreq.base_image:
            base_img_name = "jammy-amd64-20220808.qcow2"
            try:
                base_img = pool.storageVolLookupByName(base_img_name)
            except:
                from urllib.request import urlopen

                res = urlopen(
                    f"https://cloud-images.ubuntu.com/releases/22.04/release-20220808/ubuntu-22.04-server-cloudimg-amd64.img"
                )
                size = int(res.getheader("Content-length"))
                base_img = pool.createXML(
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
            base_img = pool.storageVolLookupByName(domreq.base_image)

        vol = pool.createXML(
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

        dom_uuid = uuid.uuid4()

        mac = "{:02x}:{:02x}:{:02x}:{:02x}:{:02x}:{:02x}".format(
            0x52, 0x54, 0x00, ip.packed[-3], ip.packed[-2], ip.packed[-1]
        )
        ccfg_raw = create_cloud_config_image(
            domain_id=dom_uuid,
            user_data=domreq.user_data,
            mac=mac,
            network=net,
            address=ip,
            gateway=net[1],
            name=domreq.name,
        )
        stream = self.conn.newStream()
        ccfg_vol = pool.createXML(
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
    <interface type='network'>
      <mac address='{mac}'/>
      <source network='{network_name}'/>
      <model type='virtio'/>
    </interface>
    <console type='pty'/>
  </devices>
</domain>"""
        )

        dom.setAutostart(True)
        dom.create()

        return domain_pb2.Domain(**self._get_domain(dom.UUIDString()))

    def DeleteDomain(self, request, context):
        dom = self.conn.lookupByUUIDString(str(request.uuid))
        name = dom.name()
        ip = self._get_domain(dom.UUIDString())["private_ip"]
        try:
            dom.destroy()
        except libvirt.libvirtError as e:
            if "Requested operation is not valid: domain is not running" in str(e):
                pass
            else:
                raise e
        dom.undefine()

        with self.lock:
            self.ips.remove(ip)

        pool = self.conn.storagePoolLookupByName("restvirtimages")
        vol = pool.storageVolLookupByName(f"{name}-root.qcow2")
        vol.delete()
        vol = pool.storageVolLookupByName(f"{name}-cloud-init.img")
        vol.delete()

        return empty_pb2.Empty()

    def DownloadImage(self, request, context):
        dom = self.conn.lookupByUUIDString(request.domain_id)
        name = dom.name()
        pool = self.conn.storagePoolLookupByName("restvirtimages")
        vol = pool.storageVolLookupByName(f"{name}-root.qcow2")
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
            self.port_fwd_sync_handler.handle_sync(session)

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
            self.port_fwd_sync_handler.handle_sync(session)

        return empty_pb2.Empty()

    def sync(self):
        with self.session_factory() as session:
            self.port_fwd_sync_handler.handle_sync(session)
        self.network_synchronizer.sync()

    def SyncRoutes(self, request, context):
        self.tables_synchronizer.handle_sync(self.controller)
        self.routes_synchronizer.handle_sync(self.controller)
        return empty_pb2.Empty()

    def SyncNetworks(self, request, context):
        self.network_synchronizer.sync()
        return empty_pb2.Empty()
