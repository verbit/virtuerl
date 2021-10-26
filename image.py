import os
import subprocess
import tempfile

import yaml

IMAGES_ROOT = "/data/restvirt/images"


# TODO: rewrite all of that with pycdlib


def _cloud_config_path(name):
    return os.path.join(IMAGES_ROOT, f"{name}-cloud-init.img")


def _read_from_cloud_config_file(data, section):

    with tempfile.NamedTemporaryFile(mode="wb") as f:
        f.write(data)
        f.flush()

        out = subprocess.run(
            ["isoinfo", "-R", "-x", section, "-i", f.name],
            capture_output=True,
            check=True,
            text=True,
        )
        return out.stdout


def read_user_data_from_cloud_config_image(data):
    return _read_from_cloud_config_file(data, "/user-data")


def read_ip_from_cloud_config_image(data):
    network_config = _read_from_cloud_config_file(data, "/network-config")
    network_config_dict = yaml.safe_load(network_config)
    cidr = network_config_dict["ethernets"]["primary"]["addresses"][0]
    ip, prefix = cidr.split("/")
    return ip


def create_cloud_config_image(domain_id, user_data, mac, ip, name):
    network_config = f"""version: 2
ethernets:
  primary:
     match:
       macaddress: "{mac}"
     dhcp4: false
     # default libvirt network
     addresses:
       - {ip}/24
     gateway4: 192.168.122.1
     nameservers:
       addresses:
         - 192.168.122.1
"""

    meta_config = f"""instance-id: {domain_id}
local-hostname: {name}
"""

    with tempfile.NamedTemporaryFile(mode="w+") as fn, tempfile.NamedTemporaryFile(
        mode="w+"
    ) as fud, tempfile.NamedTemporaryFile(mode="w+") as fmd, tempfile.NamedTemporaryFile(
        mode="rb"
    ) as out:
        fn.write(network_config)
        fn.flush()

        fud.write(user_data)
        fud.flush()

        fmd.write(meta_config)
        fmd.flush()

        subprocess.run(
            ["cloud-localds", "--network-config", fn.name, out.name, fud.name, fmd.name],
            check=True,
        )
        return out.read()
