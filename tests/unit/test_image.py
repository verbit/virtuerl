from ipaddress import ip_address, ip_network

import pytest

from minivirt.image import (
    create_cloud_config_image,
    read_ip_from_cloud_config_image,
    read_user_data_from_cloud_config_image,
)


@pytest.fixture
def data():
    return create_cloud_config_image(
        domain_id="domain_id",
        user_data="hello world",
        mac="mac_address",
        network=ip_network("192.168.1.0/24"),
        address=ip_address("192.168.1.43"),
        gateway=ip_address("192.168.1.1"),
        name="name",
    )


def test_read_ip(data):
    ip = read_ip_from_cloud_config_image(data)
    assert ip == "192.168.1.43"


def test_read_user_data(data):
    user_data = read_user_data_from_cloud_config_image(data)
    assert user_data == "hello world"
