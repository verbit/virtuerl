import threading
import uuid
from datetime import datetime, timedelta

import grpc
from google.protobuf import empty_pb2
from grpc import StatusCode
from sqlalchemy import select

from minivirt import host_pb2, host_pb2_grpc
from minivirt.models import BootstrapToken, Host


class HostController:
    def __init__(self, session_factory):
        self.session_factory = session_factory
        self.lock = threading.Lock()
        self.cache = {}

        for host in self.hosts():
            self.cache[host.name] = grpc.insecure_channel(host.address)

    def hosts(self):
        with self.session_factory() as session:
            return session.execute(select(Host)).scalars().all()

    def host(self, name):
        with self.session_factory.begin() as session:
            return session.get(Host, name)

    def channel(self, hostname):
        with self.lock:
            return self.cache.get(hostname)

    def channels(self):
        with self.lock:
            return list(self.cache.values())

    def register(self, token, hostname, address):
        with self.session_factory.begin() as session:
            token = session.get(BootstrapToken, token)
            if token is None or token.expires_at <= datetime.utcnow():
                raise Exception("BootstrapToken invalid")
            session.add(Host(name=hostname, address=address))
            with self.lock:
                self.cache[hostname] = grpc.insecure_channel(address)
        return empty_pb2.Empty()

    def deregister(self, hostname):
        with self.session_factory.begin() as session:
            host = session.get(Host, hostname)
            if host is None:
                raise Exception("Host not found")
            session.delete(host)
            with self.lock:
                del self.cache[hostname]


class HostService(host_pb2_grpc.HostServiceServicer):
    def __init__(self, host_controller: HostController, session_factory):
        self.session_factory = session_factory
        self.host_controller = host_controller

    def CreateBootstrapToken(self, request, context):
        with self.session_factory.begin() as session:
            if not request.expires_at:
                expires_at = datetime.utcnow() + timedelta(minutes=10)
            else:
                expires_at = datetime.fromisoformat(request.expires_at)
            token = BootstrapToken(
                token=str(uuid.uuid4()),
                expires_at=expires_at,
            )
            session.add(token)
            return host_pb2.CreateBootstrapTokenResponse(token=token.token)

    def GetHost(self, request, context):
        host = self.host_controller.host(request.name)
        if host is None:
            context.set_code(StatusCode.NOT_FOUND)
            context.set_details("Host not found")
            return
        return host_pb2.Host(name=host.name, address=host.address)

    def ListHosts(self, request, context):
        hosts = self.host_controller.hosts()
        hosts = [host_pb2.Host(name=h.name, address=h.address) for h in hosts]
        return host_pb2.ListHostsResponse(hosts=hosts)

    def Register(self, request, context):
        try:
            self.host_controller.register(request.token, request.host.name, request.host.address)
            return empty_pb2.Empty()
        except Exception as e:
            if "BootstrapToken invalid" in str(e):
                context.set_code(StatusCode.INVALID_ARGUMENT)
                context.set_details("BootstrapToken invalid")
                return
            raise e

    def Deregister(self, request, context):
        try:
            self.host_controller.deregister(request.name)
            return empty_pb2.Empty()
        except Exception as e:
            if "Host not found" in str(e):
                context.set_code(StatusCode.NOT_FOUND)
                context.set_details("Host not found")
                return
            raise e

    def Heartbeat(self, request, context):
        with self.session_factory.begin() as session:
            host = session.get(Host, request.host.name)
            if host is None:
                context.set_code(StatusCode.NOT_FOUND)
                context.set_details("Host not found")
                return
            host.last_heartbeat = datetime.utcnow()
            return host_pb2.HeartbeatResponse()
