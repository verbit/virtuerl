import os
import string
from ipaddress import ip_address

import libvirt
import xmltodict
from flask import Flask, jsonify, request
from werkzeug.exceptions import HTTPException

import image
from forwarding import PortForwardingController
from image import (
    read_ip_from_cloud_config_image,
    read_user_data_from_cloud_config_image,
    create_cloud_config_image,
    IMAGES_ROOT,
)
from version import __version__

app = Flask(__name__)
pwc = PortForwardingController()
conn = libvirt.open("qemu:///session?socket=/var/run/libvirt/libvirt-sock")


def libvirt_state_to_string(state):
    if state == libvirt.VIR_DOMAIN_NOSTATE:
        return "NOSTATE"
    elif state == libvirt.VIR_DOMAIN_RUNNING:
        return "RUNNING"
    elif state == libvirt.VIR_DOMAIN_BLOCKED:
        return "BLOCKED"
    elif state == libvirt.VIR_DOMAIN_PAUSED:
        return "PAUSED "
    elif state == libvirt.VIR_DOMAIN_SHUTDOWN:
        return "SHUTDOWN"
    elif state == libvirt.VIR_DOMAIN_CRASHED:
        return "CRASHED"
    elif state == libvirt.VIR_DOMAIN_PMSUSPENDED:
        return "PMSUSPENDED"
    elif state == libvirt.VIR_DOMAIN_SHUTOFF:
        return "SHUTOFF"
    return "UNKNOWN"


def get_domain(uuid):
    domain = conn.lookupByUUIDString(uuid)
    domain_dict = xmltodict.parse(domain.XMLDesc())
    state, _ = domain.state()
    private_ip = read_ip_from_cloud_config_image(domain_dict["domain"]["name"])
    user_data = read_user_data_from_cloud_config_image(domain_dict["domain"]["name"])
    return {
        "id": int(domain_dict["domain"]["@id"]),
        "uuid": domain_dict["domain"]["uuid"],
        "name": domain_dict["domain"]["name"],
        "vcpu": int(domain_dict["domain"]["vcpu"]["#text"]),
        "memory": int(domain_dict["domain"]["memory"]["#text"]) // 1024,
        # "network": domain_dict["domain"]["devices"]["interface"]["source"]["@network"],
        # "bridge": domain_dict["domain"]["devices"]["interface"]["source"]["@bridge"],
        "state": libvirt_state_to_string(state),
        "private_ip": private_ip,
        "user_data": user_data,
    }


@app.errorhandler(libvirt.libvirtError)
def libvirt_error(e):
    return jsonify(error=str(e), type=str(type(e)), error_code=e.get_error_code()), 500


@app.errorhandler(HTTPException)
def http_error(e):
    if hasattr(e, "original_exception"):
        e = e.original_exception
    code = 500
    if hasattr(e, "code") and e.code is not None:
        code = e.code
    return jsonify(error=str(e)), code


@app.route("/info")
def info():
    return jsonify(version=__version__)


@app.route("/domains", methods=["POST"])
def create_domain():
    req_json = request.json

    pool = conn.storagePoolLookupByName("restvirtimages")

    vol = pool.createXML(
        f"""<volume type='file'>
  <name>{req_json['name']}-root.qcow2</name>
  <capacity unit='GiB'>{20}</capacity>
  <backingStore>
    <path>{os.path.join(IMAGES_ROOT, 'focal-server-cloudimg-amd64.img')}</path>
    <format type='qcow2'/>
  </backingStore>
  <target>
    <format type='qcow2'/>
  </target>
</volume>"""
    )
    root_image_path = vol.path()

    ip_br = ip_address(req_json["private_ip"])
    mac_bridge = "{:02x}:{:02x}:{:02x}:{:02x}:{:02x}:{:02x}".format(
        0x52, 0x54, 0x00, 0x00, ip_br.packed[-2], ip_br.packed[-1]
    )
    packed = bytearray(ip_br.packed)
    packed[-2] = 122
    ip_gw = ip_address(bytes(packed))
    mac_gateway = "{:02x}:{:02x}:{:02x}:{:02x}:{:02x}:{:02x}".format(
        0x52, 0x54, 0x00, 0x01, ip_gw.packed[-2], ip_gw.packed[-1]
    )
    cloud_config_image_path = image._cloud_config_path(req_json["name"])

    dom = conn.defineXML(
        f"""<domain type='kvm'>
  <features>
    <acpi/>
  </features>
  <name>{req_json['name']}</name>
  <vcpu>{req_json['vcpu']}</vcpu>
  <memory unit='MiB'>{req_json['memory']}</memory>
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
      <source file='{cloud_config_image_path}'/>
      <target dev='vde' bus='sata'/>
    </disk>
    <interface type='bridge'>
        <source bridge='br2'/>
        <mac address='{mac_bridge}'/>
    </interface>
    <interface type='network'>
        <mac address='{mac_gateway}'/>
        <source network='default'/>
    </interface>
    <console type='pty'/>
  </devices>
</domain>"""
    )

    create_cloud_config_image(
        dom.UUIDString(), req_json["user_data"], mac_bridge, str(ip_br), mac_gateway, str(ip_gw), req_json["name"]
    )
    pool.refresh()

    dom.setAutostart(True)
    dom.create()

    return jsonify(uuid=dom.UUIDString())


@app.route("/domains")
def domains():
    domains = conn.listAllDomains()
    return jsonify(domains=[xmltodict.parse(d.XMLDesc()) for d in domains])


@app.route("/domains/<uuid:id>")
def domain(id):
    return jsonify(**get_domain(str(id)))


@app.route("/domains/<uuid:id>", methods=["DELETE"])
def delete_domain(id):
    dom = conn.lookupByUUIDString(str(id))
    name = dom.name()
    dom.destroy()
    dom.undefine()

    pool = conn.storagePoolLookupByName("restvirtimages")
    vol = pool.storageVolLookupByName(f"{name}-root.qcow2")
    vol.delete()
    vol = pool.storageVolLookupByName(f"{name}-cloud-init.img")
    vol.delete()

    return jsonify()


def _volume_to_dict(vol):
    _, cap, _ = vol.info()
    name = vol.name()
    return {
        "id": name,
        "name": name,
        "size": cap,
    }


@app.route("/volumes")
def volumes():
    pool = conn.storagePoolLookupByName("volumes")
    vols = pool.listAllVolumes()
    vol_dicts = [_volume_to_dict(vol) for vol in vols]
    return jsonify(volumes=vol_dicts)


@app.route("/volumes/<id>")
def get_volume(id):
    pool = conn.storagePoolLookupByName("volumes")
    vol = pool.storageVolLookupByName(id)
    return jsonify(**_volume_to_dict(vol))


@app.route("/volumes", methods=["POST"])
def create_volume():
    req_json = request.json
    pool = conn.storagePoolLookupByName("volumes")
    vol = pool.createXML(
        f"""<volume>
  <name>{req_json['name']}</name>
  <capacity unit='bytes'>{req_json['size']}</capacity>
  <target>
    <format type='qcow2'/>
  </target>
</volume>"""
    )
    return jsonify(id=vol.name())


@app.route("/volumes/<id>", methods=["DELETE"])
def delete_volume(id):
    pool = conn.storagePoolLookupByName("volumes")
    vol = pool.storageVolLookupByName(id)
    if _get_all_attachments(conn.listAllDomains(), vol):
        return jsonify(error="volume is attached"), 409
    vol.delete()
    return jsonify()


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


@app.route("/volumes/<id>/domains")
def domain_attachments(id):
    domains = conn.listAllDomains()
    pool = conn.storagePoolLookupByName("volumes")
    vol = pool.storageVolLookupByName(id)
    return jsonify(domains=_get_all_attachments(domains, vol))


def _disk_address(domain_dict, volume_id):
    disks = domain_dict["domain"]["devices"]["disk"]

    da = [d["address"] for d in disks if d["alias"]["@name"] == f"ua-{volume_id}"][0]
    daddr = [int(da[f"@{k}"], 16) for k in ["domain", "bus", "slot", "function"]]
    return f"{da['@type']}-{daddr[0]:04x}:{daddr[1]:02x}:{daddr[2]:02x}.{daddr[3]:x}"


def disk_address(domain, volume_id):
    domain_dict = xmltodict.parse(domain.XMLDesc())
    return _disk_address(domain_dict, volume_id)


@app.route("/domains/<uuid:domain_id>/volumes")
def volume_attachments(domain_id):
    domain = conn.lookupByUUIDString(str(domain_id))
    return jsonify(attachments=_get_attachments(domain))


@app.route("/domains/<uuid:domain_id>/volumes/<volume_id>", methods=["PUT"])
def attach_volume(domain_id, volume_id):
    domain = conn.lookupByUUIDString(str(domain_id))
    pool = conn.storagePoolLookupByName("volumes")
    vol = pool.storageVolLookupByName(volume_id)

    domain_dict = xmltodict.parse(domain.XMLDesc())
    disks = domain_dict["domain"]["devices"]["disk"]

    volumes_ids = [
        d["alias"]["@name"][3:]
        for d in disks
        if d["@device"] == "disk" and d["alias"]["@name"].startswith("ua-")
    ]

    if volume_id in volumes_ids:
        return jsonify()

    disk_shortnames = [d["target"]["@dev"][-1:] for d in disks]
    disk_letter = sorted(set(string.ascii_lowercase).difference(disk_shortnames))[0]
    domain.attachDeviceFlags(
        f"""<disk type='file' device='disk'>
   <driver name='qemu' type='qcow2'/>
   <source file='{vol.path()}'/>
   <target dev='vd{disk_letter}' bus='virtio'/>
   <alias name='ua-{volume_id}'/>
 </disk>
 """,
        libvirt.VIR_DOMAIN_AFFECT_LIVE | libvirt.VIR_DOMAIN_AFFECT_CONFIG,
    )

    return jsonify(disk_address=disk_address(domain, volume_id))


@app.route("/domains/<uuid:domain_id>/volumes/<volume_id>", methods=["DELETE"])
def detach_volume(domain_id, volume_id):
    domain = conn.lookupByUUIDString(str(domain_id))
    domain.detachDeviceAlias(
        f"ua-{volume_id}", libvirt.VIR_DOMAIN_AFFECT_LIVE | libvirt.VIR_DOMAIN_AFFECT_CONFIG
    )
    return jsonify()


@app.route("/domains/<uuid:domain_id>/volumes/<volume_id>")
def volume_attachment(domain_id, volume_id):
    domain = conn.lookupByUUIDString(str(domain_id))
    pool = conn.storagePoolLookupByName("volumes")
    vol = pool.storageVolLookupByName(volume_id)

    return jsonify(
        domain_id=domain.UUIDString(),
        volume_id=vol.name(),
        disk_address=disk_address(domain, volume_id),
    )


# @app.route('/forwardings/<int:source_port>', methods=['PUT'])
# def create_port_forwarding(source_port):
#     req_json = request.json
#     if req_json['source_port'] != source_port:
#         return jsonify(error=f'source port must be {source_port}'), 400
#
#     pwc.add(req_json)
#     return jsonify(), 201


@app.route("/forwardings", methods=["POST"])
def create_port_forwarding():
    pwc.add(request.json)
    return jsonify(), 201


@app.route("/forwardings")
def port_forwardings():
    return jsonify(port_forwardings=pwc.get_forwardings())


@app.route("/forwardings/<int:source_port>")
def port_forwarding(source_port):
    forwarding = pwc.get_forwarding(source_port)
    if forwarding is None:
        return jsonify(), 404
    return jsonify(**forwarding)


@app.route("/forwardings/<int:source_port>", methods=["DELETE"])
def delete_port_forwarding(source_port):
    pwc.remove(source_port)
    return jsonify(), 200


if __name__ == "__main__":
    pwc.sync()
    app.config["PROPAGATE_EXCEPTIONS"] = False
    app.run(host="0.0.0.0", port=8090, debug=True, use_debugger=False)
