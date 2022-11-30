from setuptools import find_packages, setup

from minivirt.version import __version__

setup(
    name="restvirt",
    version="0.1",  # TODO: use __version__ instead
    packages=find_packages(),
    url="",
    license="Apache",
    author="Ilya Verbitskiy",
    author_email="ilya@verbit.io",
    description="",
    entry_points={"console_scripts": ["restvirt=minivirt.main:main"]},
    install_requires=[
        "libvirt-python==8.0.0",
        "grpcio==1.41.1",
        "grpcio-reflection==1.51.1",
        "protobuf==3.19.1",
        "pycdlib==1.12.0",
        "pyroute2==0.7.2",
        "nftables @ git+https://salsa.debian.org/pkg-netfilter-team/pkg-nftables.git@upstream/1.0.4#egg=nftables&subdirectory=py",
        "python-iptables==1.0.0",
        "SQLAlchemy==1.4.26",
        "xmltodict==0.12.0",
        "dnslib==0.9.16",
        "dnspython==2.2.1",
        "PyYAML==6.0",
    ],
)
