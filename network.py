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
