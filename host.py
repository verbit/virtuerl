import uuid
from datetime import datetime, timedelta

from google.protobuf import empty_pb2
from grpc import StatusCode
from sqlalchemy import select

import host_pb2
import host_pb2_grpc
from models import BootstrapToken, Host


class HostService(host_pb2_grpc.HostServiceServicer):
    def __init__(self, session_factory):
        self.session_factory = session_factory

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
        with self.session_factory.begin() as session:
            host = session.get(Host, request.name)
            if host is None:
                context.set_code(StatusCode.NOT_FOUND)
                context.set_details("Host not found")
                return
            return host_pb2.Host(name=host.name, address=host.address)

    def ListHosts(self, request, context):
        with self.session_factory.begin() as session:
            hosts = session.execute(select(Host)).scalars()
            hosts = [host_pb2.Host(name=h.name, address=h.address) for h in hosts]
            return host_pb2.ListHostsResponse(hosts=hosts)

    def Register(self, request, context):
        with self.session_factory.begin() as session:
            token = session.get(BootstrapToken, request.token)
            if token is None or token.expires_at <= datetime.utcnow():
                context.set_code(StatusCode.INVALID_ARGUMENT)
                context.set_details("BootstrapToken invalid")
                return
            session.add(Host(name=request.host.name, address=request.host.address))
        return empty_pb2.Empty()

    def Deregister(self, request, context):
        with self.session_factory.begin() as session:
            host = session.get(Host, request.name)
            if host is None:
                context.set_code(StatusCode.NOT_FOUND)
                context.set_details("Host not found")
                return
            session.delete(host)
        return empty_pb2.Empty()

    def Heartbeat(self, request, context):
        with self.session_factory.begin() as session:
            host = session.get(Host, request.host.name)
            if host is None:
                context.set_code(StatusCode.NOT_FOUND)
                context.set_details("Host not found")
                return
            host.last_heartbeat = datetime.utcnow()
            return host_pb2.HeartbeatResponse()
