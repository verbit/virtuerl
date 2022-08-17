import pytest
from pyroute2 import IPRoute

import controller_pb2_grpc
import route_pb2


@pytest.fixture
def client(controller_client_dummy):
    return controller_client_dummy


def test_id_generation(client: controller_pb2_grpc.ControllerServiceStub):
    req = route_pb2.CreateRouteTableRequest(route_table=route_pb2.RouteTable(name="XAXAXA"))
    creates = [client.CreateRouteTable(req) for _ in range(3)]
    ids = [resp.id for resp in creates]
    client.DeleteRouteTable(route_pb2.RouteTableIdentifier(id=ids[1]))
    resp = client.CreateRouteTable(req)
    assert resp.id == ids[1]

    for _ in range(98):
        client.CreateRouteTable(req)

    with pytest.raises(Exception):
        client.CreateRouteTable(req)


def test_route_update(client: controller_pb2_grpc.ControllerServiceStub):
    req = route_pb2.CreateRouteTableRequest(route_table=route_pb2.RouteTable(name="XAXAXA"))
    resp = client.CreateRouteTable(req)
    route_table_id = resp.id
    client.PutRoute(
        route_pb2.PutRouteRequest(
            route=route_pb2.Route(
                route_table_id=route_table_id,
                destination="10.6.1.0/24",
                gateways=["192.168.0.1"],
            )
        )
    )
    client.PutRoute(
        route_pb2.PutRouteRequest(
            route=route_pb2.Route(
                route_table_id=route_table_id,
                destination="10.6.1.0/24",
                gateways=["192.168.0.2", "192.168.0.3"],
            )
        )
    )
    routes = client.ListRoutes(route_pb2.ListRoutesRequest(route_table_id=route_table_id)).routes
    assert len(routes) == 1
    assert sorted(routes[0].gateways) == ["192.168.0.2", "192.168.0.3"]


def test_route_delete(client: controller_pb2_grpc.ControllerServiceStub):
    req = route_pb2.CreateRouteTableRequest(route_table=route_pb2.RouteTable(name="XAXAXA"))
    resp = client.CreateRouteTable(req)
    route_table_id = resp.id
    route = client.PutRoute(
        route_pb2.PutRouteRequest(
            route=route_pb2.Route(
                route_table_id=route_table_id,
                destination="10.6.1.0/24",
                gateways=["192.168.0.1"],
            )
        )
    )
    routes = client.ListRoutes(route_pb2.ListRoutesRequest(route_table_id=route_table_id)).routes
    assert len(routes) == 1

    client.DeleteRoute(
        route_pb2.RouteIdentifier(
            route_table_id=route.route_table_id,
            destination=route.destination,
        )
    )
    routes = client.ListRoutes(route_pb2.ListRoutesRequest(route_table_id=route_table_id)).routes
    assert len(routes) == 0

    client.DeleteRoute(
        route_pb2.RouteIdentifier(
            route_table_id=route.route_table_id,
            destination=route.destination,
        )
    )


def test_route_table_delete(client: controller_pb2_grpc.ControllerServiceStub):
    req = route_pb2.CreateRouteTableRequest(route_table=route_pb2.RouteTable(name="XAXAXA"))
    resp = client.CreateRouteTable(req)
    route_table_id = resp.id
    client.PutRoute(
        route_pb2.PutRouteRequest(
            route=route_pb2.Route(
                route_table_id=route_table_id,
                destination="10.6.1.0/24",
                gateways=["192.168.0.1"],
            )
        )
    )

    client.DeleteRouteTable(route_pb2.RouteTableIdentifier(id=route_table_id))
    routes = client.ListRoutes(route_pb2.ListRoutesRequest(route_table_id=route_table_id)).routes
    assert len(routes) == 0

    client.DeleteRouteTable(route_pb2.RouteTableIdentifier(id=route_table_id))


@pytest.fixture
def rtnl_api():
    with IPRoute() as ip:
        yield ip


@pytest.fixture
def client_iproute(controller_client, rtnl_api):
    rtnl_api.link("add", ifname="restvirtbr0", kind="dummy")
    dev = rtnl_api.link_lookup(ifname="restvirtbr0")[0]
    rtnl_api.addr("add", index=dev, address="10.69.69.0", mask=24)
    rtnl_api.link("set", index=dev, state="up")

    yield controller_client

    rtnl_api.link("del", ifname="restvirtbr0")


def test_route_linux(client_iproute: controller_pb2_grpc.ControllerServiceStub, rtnl_api: IPRoute):
    client = client_iproute
    req = route_pb2.CreateRouteTableRequest(route_table=route_pb2.RouteTable(name="XAXAXA"))
    resp = client.CreateRouteTable(req)
    route_table_id = resp.id
    route = client.PutRoute(
        route_pb2.PutRouteRequest(
            route=route_pb2.Route(
                route_table_id=route_table_id,
                destination="10.69.42.0/24",
                gateways=["10.69.69.1"],
            )
        )
    )

    r = rtnl_api.route("get", dst="10.69.42.1")[0]
    assert r.get_attr("RTA_GATEWAY") == "10.69.69.1"

    client.DeleteRoute(
        route_pb2.RouteIdentifier(
            route_table_id=route.route_table_id,
            destination=route.destination,
        )
    )
    assert len(rtnl_api.get_routes(table=route_table_id)) == 0

    client.DeleteRouteTable(route_pb2.RouteTableIdentifier(id=route_table_id))
    assert len(rtnl_api.get_rules(priority=route_table_id)) == 0
