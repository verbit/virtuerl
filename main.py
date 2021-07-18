import argparse
import logging
import os
import re
import string
from ipaddress import ip_address, ip_network

import libvirt
import xmltodict
from flask import Flask, jsonify, request, Response
from passlib.apache import HtpasswdFile
from werkzeug.exceptions import HTTPException

import image
from dns import DNSController, DNSRecord
from forwarding import PortForwardingController
from image import (
    create_cloud_config_image,
    IMAGES_ROOT,
    read_ip_from_cloud_config_image,
    read_user_data_from_cloud_config_image,
)
from route import AliasIPController
from version import __version__

app = Flask(__name__)
conn = libvirt.open("qemu:///session?socket=/var/run/libvirt/libvirt-sock")
libvirt.registerErrorHandler(lambda u, e: None, None)


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
        "network": domain_dict["domain"]["devices"]["interface"]["source"]["@network"],
        "bridge": domain_dict["domain"]["devices"]["interface"]["source"]["@bridge"],
        "state": libvirt_state_to_string(state),
        "private_ip": private_ip,
        "user_data": user_data,
    }


@app.errorhandler(libvirt.libvirtError)
def libvirt_error(e):
    status_code = 500
    if e.get_error_code() in [
        libvirt.VIR_ERR_NO_DOMAIN,
        libvirt.VIR_ERR_NO_STORAGE_VOL,
    ]:
        status_code = 404

    return jsonify(error=str(e), type=str(type(e)), error_code=e.get_error_code()), status_code


@app.errorhandler(HTTPException)
def http_error(e):
    if hasattr(e, "original_exception"):
        e = e.original_exception
    code = 500
    if hasattr(e, "code") and e.code is not None:
        code = e.code
    return jsonify(error=str(e)), code


@app.before_request
def basic_auth():
    if htpasswd is None:
        return

    if request.endpoint == "info":
        return

    auth = request.authorization
    authenticated = (
        auth is not None
        and auth.type == "basic"
        and htpasswd.check_password(auth.username, auth.password)
    )
    if authenticated:
        return

    return Response(status=401, headers={"WWW-Authenticate": 'Basic realm="Login required"'})


@app.route("/info")
def info():
    res = {"version": __version__}
    return jsonify(**res)


def materialize_stats(d):
    stats = {}
    for k, v in d.items():
        if callable(v):
            stats[k] = v(d)
        elif isinstance(v, dict):
            stats[k] = materialize_stats(v)
        else:
            stats[k] = v
    return stats


@app.route("/stats")
def server_stats():
    res = {"stats": "not supported"}
    if server is not None:
        stats = materialize_stats(server.stats)
        res["stats"] = stats
    return jsonify(**res)


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

    ip = ip_address(req_json["private_ip"])
    mac = "{:02x}:{:02x}:{:02x}:{:02x}:{:02x}:{:02x}".format(
        0x52, 0x54, 0x00, ip.packed[-3], ip.packed[-2], ip.packed[-1]
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
    <interface type='network'>
      <mac address='{mac}'/>
      <source network='default'/>
      <model type='virtio'/>
    </interface>
    <console type='pty'/>
  </devices>
</domain>"""
    )

    create_cloud_config_image(
        dom.UUIDString(), req_json["user_data"], mac, str(ip), req_json["name"]
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
        return jsonify(disk_address=disk_address(domain, volume_id))

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
    try:
        domain.detachDeviceAlias(
            f"ua-{volume_id}", libvirt.VIR_DOMAIN_AFFECT_LIVE | libvirt.VIR_DOMAIN_AFFECT_CONFIG
        )
    except:
        # TODO: check for string "no device found with alias"
        pass
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


@app.route("/forwardings/<int:source_port>-<protocol>")
def port_forwarding(source_port, protocol):
    forwarding = pwc.get_forwarding(source_port, protocol)
    if forwarding is None:
        return jsonify(), 404
    return jsonify(**forwarding)


@app.route("/forwardings/<int:source_port>-<protocol>", methods=["DELETE"])
def delete_port_forwarding(source_port, protocol):
    pwc.remove(source_port, protocol)
    return jsonify(), 200


@app.route("/dns")
def dns_mappings():
    return jsonify(dns.get_mappings())


@app.route("/dns/<name>-<type>")
def dns_mapping(name, type):
    mapping = dns.get_mapping(name, type)
    if mapping is None:
        return jsonify(), 404
    return jsonify(mapping)


@app.route("/dns/<name>-<type>", methods=["DELETE"])
def delete_dns_mapping(name, type):
    dns.remove(name, type)
    return jsonify(), 200


@app.route("/dns/<name>-<type>", methods=["PUT"])
def set_dns_mapping(name, type):
    # TODO: check that they are identical
    record_dict = request.json
    record_dict["name"] = name
    record_dict["type"] = type
    dns.set(DNSRecord(**record_dict))
    return jsonify(), 200


@app.route("/routes")
def get_routes():
    routes = route.get_routes().items()
    route.get_routes(namespace=request.args.get("namespace"))
    return jsonify(
        routes=[
            {
                "destination": str(dst),
                "gateways": [str(gw) for gw in r.gateways],
                "namespace": r.namespace,
            }
            for dst, r in routes
        ]
    )


@app.route("/routes/<ip>")
def get_route(ip):
    dst = ip_network(ip.replace("-", "/"))
    r = route.get_route(dst)
    return jsonify(
        {
            "destination": str(dst),
            "gateways": [str(gw) for gw in r.gateways],
            "namespace": r.namespace,
        }
    )


@app.route("/routes/<ip>", methods=["PUT"])
def add_route(ip):
    j = request.json
    dst = ip_network(ip.replace("-", "/"))
    gateways = [ip_address(gw) for gw in j["gateways"]]
    namespace = j.get("namespace", "default")
    route.set(dst, gateways, namespace)
    return jsonify()


@app.route("/routes/<ip>", methods=["DELETE"])
def delete_route(ip):
    dst = ip_network(ip.replace("-", "/"))
    route.remove(dst)
    return jsonify()


if __name__ == "__main__":
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
    parser.add_argument("--ssl-cert")
    parser.add_argument("--ssl-priv")
    parser.add_argument("--htpasswd")
    args = parser.parse_args()

    print(args)

    host, port = args.bind
    host = host or "0.0.0.0"

    pwc = PortForwardingController(args.config, state_file_name="forwardings.json")
    pwc.sync()

    dns = DNSController(args.config, state_file_name="dns.json")
    dns.start()

    route = AliasIPController(args.config, state_file_name="routes.json")
    route.sync()

    if args.htpasswd is None:
        htpasswd = None
    else:
        htpasswd = HtpasswdFile(args.htpasswd)

    if args.debug:
        app.config["PROPAGATE_EXCEPTIONS"] = False
        app.run(host=host, port=port, debug=True, use_debugger=False)
    else:
        from cheroot.wsgi import Server as WSGIServer
        from cheroot.ssl.builtin import BuiltinSSLAdapter

        # app.logger.addHandler(logging.StreamHandler())
        app.logger.setLevel(logging.INFO)
        server = WSGIServer((host, port), app)
        server.stats["Enabled"] = True
        if args.ssl_cert is not None or args.ssl_priv is not None:
            server.ssl_adapter = BuiltinSSLAdapter(args.ssl_cert, args.ssl_priv)
        server.safe_start()
