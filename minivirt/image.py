from io import BytesIO

import pycdlib
import yaml


def _read_from_cloud_config_file(data, section):
    section_data = BytesIO()
    iso = pycdlib.PyCdlib()
    iso.open_fp(BytesIO(data))
    iso.get_file_from_iso_fp(section_data, joliet_path=section)
    iso.close()
    return section_data.getvalue().decode()


def read_user_data_from_cloud_config_image(data):
    return _read_from_cloud_config_file(data, "/user-data")


def read_ip_from_cloud_config_image(data):
    network_config = _read_from_cloud_config_file(data, "/network-config")
    network_config_dict = yaml.safe_load(network_config)
    cidr = network_config_dict["ethernets"]["primary"]["addresses"][0]
    ip, prefix = cidr.split("/")
    return ip


def create_cloud_config_image(domain_id, user_data, mac, network, address, gateway, name):
    assert gateway in network
    assert address in network

    network_config = f"""version: 2
ethernets:
  primary:
     match:
       macaddress: "{mac}"
     set-name: "ens2"
     dhcp4: false
     # default libvirt network
     addresses:
       - {address}/{network.prefixlen}
     gateway4: {gateway}
     nameservers:
       addresses:
         - {gateway}
"""

    meta_config = f"""instance-id: {domain_id}
local-hostname: {name}
"""

    iso = pycdlib.PyCdlib()
    iso.new(
        interchange_level=3,
        joliet=3,
        rock_ridge="1.09",
        vol_ident="cidata",
        sys_ident="LINUX",
    )

    iso_config = {
        "network-config": network_config,
        "meta-data": meta_config,
        "user-data": user_data,
    }

    for section_name, data_string in iso_config.items():
        data = data_string.encode()
        iso.add_fp(
            BytesIO(data),
            len(data),
            iso_path=f'/{section_name.replace("-", "").upper()}.;1',
            rr_name=section_name,
            joliet_path=f"/{section_name}",
        )

    result = BytesIO()
    iso.write_fp(result)
    iso.close()

    return result.getvalue()
