import ipaddress
import os
import threading
import uuid

import libvirt
import xmltodict
from google.protobuf import empty_pb2

import domain_pb2
import domain_pb2_grpc
from image import (
    create_cloud_config_image,
    read_ip_from_cloud_config_image,
    read_user_data_from_cloud_config_image,
)


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
    return {
        "uuid": d["uuid"],
        "name": d["name"],
        "vcpu": int(d["vcpu"]["#text"]),
        "memory": int(d["memory"]["#text"]) // 1024,
        "network": d["devices"]["interface"]["source"]["@network"],
        "nested_virtualization": d["cpu"]["@mode"] == "host-model",
    }


class DomainService(domain_pb2_grpc.DomainServiceServicer):
    def __init__(self, pool_dir="/data/restvirt"):
        self.conn = libvirt.open("qemu:///system?socket=/var/run/libvirt/libvirt-sock")
        self.lock = threading.Lock()
        domains = self.conn.listAllDomains()
        self.ips = {self._get_domain(d.UUIDString())["private_ip"] for d in domains}

        try:
            self.conn.storagePoolLookupByName("restvirtimages")
        except libvirt.libvirtError as e:
            print(e.get_error_code())
            if e.get_error_code() == libvirt.VIR_ERR_NO_STORAGE_POOL:
                pool = self.conn.storagePoolDefineXML(
                    f"""<pool type="dir">
      <name>restvirtimages</name>
      <target>
        <path>{os.path.join(pool_dir, 'images')}</path>
      </target>
    </pool>"""
                )
                pool.build()
                pool.create()
            else:
                raise e

    def GetNetwork(self, request, context):
        net = self.conn.networkLookupByUUIDString(request.uuid)
        net_dict = xmltodict.parse(net.XMLDesc())["network"]
        net_def = net_dict["ip"]
        gateway = ipaddress.IPv4Address(net_def["@address"])
        net = ipaddress.IPv4Network(f'{gateway}/{net_def["@netmask"]}', strict=False)

        return domain_pb2.Network(
            uuid=request.uuid,
            name=net_dict["name"],
            cidr=net.with_prefixlen,
        )

    def ListNetworks(self, request, context):
        return super().ListNetworks(request, context)

    def CreateNetwork(self, request, context):
        network = request.network
        net = ipaddress.IPv4Network(network.cidr)
        lvnet = self.conn.networkDefineXML(
            f"""<network>
  <name>{network.name}</name>
  <forward mode='open'/>
  <bridge stp='on' delay='0'/>
  <dns>
    <forwarder domain='internal' addr='127.0.0.1'/>
  </dns>
  <ip address='{net[1]}' netmask='{net.netmask}'>
  </ip>
</network>
"""
        )
        lvnet.create()
        lvnet.setAutostart(True)
        network.uuid = lvnet.UUIDString()
        return network

    def DeleteNetwork(self, request, context):
        net = self.conn.networkLookupByUUIDString(request.uuid)
        net.destroy()
        net.undefine()
        return empty_pb2.Empty()

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
            base_img_name = "focal-amd64-20211021.qcow2"
            try:
                base_img = pool.storageVolLookupByName(base_img_name)
            except:
                from urllib.request import urlopen

                res = urlopen(
                    f"https://cloud-images.ubuntu.com/releases/focal/release-20211021/ubuntu-20.04-server-cloudimg-amd64.img"
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

        dom = self.conn.defineXML(
            f"""<domain type='kvm'>
  <uuid>{dom_uuid}</uuid>
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
