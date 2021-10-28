import pytest

from image import (
    create_cloud_config_image,
    read_ip_from_cloud_config_image,
    read_user_data_from_cloud_config_image,
)


@pytest.fixture
def data():
    name = "name"
    return create_cloud_config_image(
        "domain_id", "hello world", "mac_address", "192.168.1.43", name
    )


def test_read_ip(data):
    ip = read_ip_from_cloud_config_image(data)
    assert ip == "192.168.1.43"


def test_read_user_data(data):
    user_data = read_user_data_from_cloud_config_image(data)
    assert user_data == "hello world"
