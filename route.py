import ipaddress
from typing import Dict, Set

from google.protobuf import empty_pb2
from sqlalchemy import delete, select

import route_pb2
import route_pb2_grpc
from models import Route, RouteTable


class SyncEventHandler:
    def handle_sync(self, session):
        pass


class GenericRouteTableController:
    id_range_min, id_range_max = 30069, 30169

    def __init__(self, session_factory, sync_handler=SyncEventHandler()):
        self.session_factory = session_factory
        self.sync_handler = sync_handler

    def route_tables(self):
        with self.session_factory() as session:
            return session.execute(select(RouteTable)).scalars().all()

    def route_table(self, id):
        with self.session_factory() as session:
            return (
                session.execute(select(RouteTable).filter(RouteTable.id == id))
                .scalars()
                .one_or_none()
            )

    def _get_available_id(self, session):
        ids = session.execute(select(RouteTable.id).order_by(RouteTable.id)).scalars().all()
        if not ids or ids[0] > self.id_range_min:
            return self.id_range_min
        for i in range(len(ids) - 1):
            if ids[i + 1] - ids[i] > 1:
                return ids[i] + 1
        assert ids[-1] < self.id_range_max
        return ids[-1] + 1

    def create_route_table(self, r: route_pb2.RouteTable):
        with self.session_factory() as session:
            route = RouteTable(
                id=self._get_available_id(session),
                name=r.name,
            )
            session.add(route)
            session.commit()
            self.sync_handler.handle_sync(session)

            return route_pb2.RouteTable(
                id=route.id,
                name=route.name,
            )

    def remove_route_table(self, id):
        with self.session_factory() as session:
            res = session.execute(delete(RouteTable).where(RouteTable.id == id))
            if res.rowcount == 0:
                return False

            assert res.rowcount == 1
            session.commit()
            self.sync_handler.handle_sync(session)
            return True

    def sync(self):
        with self.session_factory() as session:
            self.sync_handler.handle_sync(session)


AliasIPConf = Dict[ipaddress.IPv4Network, Set[ipaddress.IPv4Address]]

TABLE_ID = 69


class GenericRouteController:
    def __init__(self, session_factory, sync_handler=SyncEventHandler()):
        self.session_factory = session_factory
        self.sync_handler = sync_handler

    def routes(self, route_table_id=None):
        with self.session_factory() as session:
            return (
                session.execute(select(Route).filter(Route.route_table_id == route_table_id))
                .scalars()
                .all()
            )

    def route(self, route_table_id, destination):
        with self.session_factory() as session:
            return (
                session.execute(
                    select(Route).filter(
                        Route.route_table_id == route_table_id,
                        Route.destination == destination,
                    )
                )
                .scalars()
                .one_or_none()
            )

    def put_route(self, r: route_pb2.Route):
        route = Route(
            destination=r.destination,
            gateways=set(r.gateways),
            route_table_id=r.route_table_id,
        )
        with self.session_factory() as session:
            merged = session.merge(route)
            if session.is_modified(merged):
                session.commit()
                self.sync_handler.handle_sync(session)

        return r

    def remove_route(self, route_table_id, destination):
        with self.session_factory() as session:
            session.execute(
                delete(Route).where(
                    Route.destination == destination,
                    Route.route_table_id == route_table_id,
                )
            )
            session.commit()
            self.sync_handler.handle_sync(session)

    def sync(self):
        with self.session_factory() as session:
            self.sync_handler.handle_sync(session)


class RouteService(route_pb2_grpc.RouteServiceServicer):
    def __init__(self, route_table_controller, route_controller):
        self.route_table_controller = route_table_controller
        self.route_controller = route_controller

    def GetRouteTable(self, request, context):
        table = self.route_table_controller.route_table(request.id)

        return route_pb2.RouteTable(
            id=table.id,
            name=table.name,
        )

    def ListRouteTables(self, request, context):
        tables = self.route_table_controller.route_tables()

        return route_pb2.ListRouteTablesResponse(
            route_tables=[
                route_pb2.RouteTable(
                    id=table.id,
                    name=table.name,
                )
                for table in tables
            ]
        )

    def CreateRouteTable(self, request, context):
        table = self.route_table_controller.create_route_table(request.route_table)
        return table

    def DeleteRouteTable(self, request, context):
        self.route_table_controller.remove_route_table(request.id)
        return empty_pb2.Empty()

    def GetRoute(self, request, context):
        route = self.route_controller.route(request.route_table_id, request.destination)
        return route_pb2.Route(
            route_table_id=route.route_table_id,
            destination=route.destination,
            gateways=route.gateways,
        )

    def ListRoutes(self, request, context):
        routes = self.route_controller.routes(request.route_table_id)

        return route_pb2.ListRoutesResponse(
            routes=[
                route_pb2.Route(
                    route_table_id=route.route_table_id,
                    destination=route.destination,
                    gateways=route.gateways,
                )
                for route in routes
            ]
        )

    def PutRoute(self, request, context):
        route = self.route_controller.put_route(request.route)
        return route

    def DeleteRoute(self, request, context):
        self.route_controller.remove_route(request.route_table_id, request.destination)
        return empty_pb2.Empty()

    def Sync(self, request, context):
        self.route_table_controller.sync()
        self.route_controller.sync()
        return empty_pb2.Empty()
