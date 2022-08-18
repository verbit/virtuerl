# RESTvirt

Manage libvirt through a REST API.

# Installation

```shell
apt install --no-install-recommends libvirt-daemon-system
snap install minivirt
sudo snap connect minivirt:network-control
sudo snap connect minivirt:firewall-control
sudo snap connect minivirt:libvirt
```

## Getting Started

```shell
git clone git@github.com:verbit/restvirt.git
cd restvirt

apt install python3-dev libvirt-dev
pip install -r requirements.txt

mkdir /etc/restvirt

# start controller
python main.py controller

#start daemon
apt install libvirt-daemon-system
python main.py daemon
```

## Code Generation
```shell
python -m grpc_tools.protoc --python_out=. --grpc_python_out=. -Iprotos/ protos/*.proto
```

## Known Issues

* AppArmor
  * https://ubuntu.com/server/docs/virtualization-libvirt
  * https://bugs.launchpad.net/ubuntu/+source/libvirt/+bug/1677398
