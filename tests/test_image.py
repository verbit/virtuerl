import pytest

from image import (
    create_cloud_config_image,
    read_ip_from_cloud_config_image,
    read_user_data_from_cloud_config_image,
)


@pytest.fixture
def name():
    name = "name"
    create_cloud_config_image("domain_id", "hello world", "mac_address", "192.168.1.43", name)
    return name


def test_read_ip_linux(name):
    ip = read_ip_from_cloud_config_image(name)
    assert ip == "192.168.1.43"


def test_read_user_data_linux(name):
    user_data = read_user_data_from_cloud_config_image(name)
    assert user_data == "hello world"
