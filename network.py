import ipaddress

from sqlalchemy import delete, select

import controller_pb2_grpc
import domain_pb2
from models import Network


class SyncEventHandler:
    def handle_sync(self):
        pass


class GenericNetworkController:
    def __init__(self, session_factory, sync_handler=SyncEventHandler()):
        self.session_factory = session_factory
        self.sync_handler = sync_handler

    def networks(self):
        with self.session_factory() as session:
            return session.execute(select(Network)).scalars().all()

    def network(self, network_id):
        with self.session_factory() as session:
            return (
                session.execute(select(Network).filter(Network.id == network_id))
                .scalars()
                .one_or_none()
            )

    def put_network(self, name, cidr):
        network = Network(id=name, cidr=cidr)
        with self.session_factory() as session:
            merged = session.merge(network)
            if session.is_modified(merged):
                session.commit()
                self.sync_handler.handle_sync()
        return network

    def remove_network(self, network_id):
        with self.session_factory() as session:
            session.execute(
                delete(Network).where(
                    Network.id == network_id,
                )
            )
            session.commit()
            self.sync_handler.handle_sync()

    def sync(self):
        self.sync_handler.handle_sync()


class NetworkSynchronizer:
    import libvirt

    def __init__(
        self,
        controller: controller_pb2_grpc.ControllerServiceStub,
        libvirt_connection: libvirt.virConnect,
    ):
        self.controller = controller
        self.libvirt_connection = libvirt_connection

    def sync(self):
        # net = self.conn.networkLookupByUUIDString(request.uuid)
        # net_dict = xmltodict.parse(net.XMLDesc())["network"]
        # net_def = net_dict["ip"]
        # gateway = ipaddress.IPv4Address(net_def["@address"])
        # net = ipaddress.IPv4Network(f'{gateway}/{net_def["@netmask"]}', strict=False)
        #
        # return domain_pb2.Network(
        #     uuid=request.uuid,
        #     name=net_dict["name"],
        #     cidr=net.with_prefixlen,
        # )

        lv_networks = self.libvirt_connection.listAllNetworks()
        lv_network_names_map = {n.name(): n for n in lv_networks}
        networks = self.controller.ListNetworks(domain_pb2.ListNetworksRequest()).networks
        network_names_map = {n.name: n for n in networks}

        for lv_network_name, lv_network in lv_network_names_map.items():
            if lv_network_name not in network_names_map:
                lv_network.destroy()
                lv_network.undefine()

        for network_name, network in network_names_map.items():
            if network_name not in lv_network_names_map:
                net = ipaddress.IPv4Network(network.cidr)
                # TODO: forwarder domain addr should point to the controller
                lvnet = self.libvirt_connection.networkDefineXML(
                    f"""<network>
  <name>{network.name}</name>
  <forward mode='open'/>
  <bridge stp='on' delay='0'/>
  <dns>
    <forwarder domain='internal' addr='127.0.0.1'/>
  </dns>
  <dns enable='no'>
  </dns>
  <ip address='{net[1]}' netmask='{net.netmask}'>
  </ip>
</network>
"""
                )
                lvnet.create()
                lvnet.setAutostart(True)
                # network.uuid = lvnet.UUIDString()
