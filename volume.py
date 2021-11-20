import string

import libvirt
import xmltodict

import volume_pb2
import volume_pb2_grpc


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
