from sqlalchemy import Column, DateTime, ForeignKey, Integer, String, Text, TypeDecorator, Unicode
from sqlalchemy.orm import backref, registry, relationship


class StringList(TypeDecorator):
    impl = Unicode
    cache_ok = True

    def __init__(self, delimiter=","):
        self.delimiter = delimiter
        super().__init__()

    def process_bind_param(self, value, dialect):
        assert all([self.delimiter not in e for e in value])
        return self.delimiter.join(value)

    def process_result_value(self, value, dialect):
        return value.split(self.delimiter)


class StringSet(TypeDecorator):
    impl = Unicode
    cache_ok = True

    def __init__(self, delimiter=","):
        self.delimiter = delimiter
        super().__init__()

    def process_bind_param(self, value, dialect):
        assert isinstance(value, set)
        assert all([self.delimiter not in e for e in value])
        return self.delimiter.join(sorted(value))

    def process_result_value(self, value, dialect):
        return set(value.split(self.delimiter))


mapper_registry = registry()
Base = mapper_registry.generate_base()


class BootstrapToken(Base):
    __tablename__ = "bootstrap_tokens"

    token = Column(String, primary_key=True)
    expires_at = Column(DateTime)

    def __repr__(self):
        return f"BootstrapToken(token={self.token!r}, expires_at={self.expires_at!r})"


class Host(Base):
    __tablename__ = "hosts"

    name = Column(String, primary_key=True)
    address = Column(String)
    last_heartbeat = Column(DateTime)

    def __repr__(self):
        return f"Host(name={self.name!r}, address={self.address!r})"


class Network(Base):
    __tablename__ = "networks"

    id = Column(String, primary_key=True)
    cidr = Column(String)
    cidr6 = Column(String)

    def __repr__(self):
        return f"Network({self.id!r}: {self.cidr!r} {self.cidr6!r})"

class NetworkV2(Base):
    __tablename__ = "networks_v2"

    id = Column(String, primary_key=True)

    def __repr__(self):
        return f"NetworkV2({self.id!r})"


class IPPool(Base):
    __tablename__ = "ip_pools"

    id = Column(String, primary_key=True)
    network_id = Column(String, ForeignKey("networks_v2.id", ondelete="CASCADE"))
    cidr = Column(String)

    def __repr__(self):
        return f"IPPool({self.id!r}: {self.cidr!r})"


class FloatingIP(Base):
    __tablename__ = "floating_ips"

    ip_pool_id = Column(Integer, ForeignKey("ip_pool.id", ondelete="CASCADE"))
    ip_pool = relationship(
        "IPPool",
        backref=backref("floating_ips", cascade="all, delete-orphan", passive_deletes=True),
    )
    address = Column(String, primary_key=True)


class FloatingIPAssignment(Base):
    __tablename__ = ""

    floating_ip_address = Column(Integer, ForeignKey("floating_ip.address", ondelete="CASCADE"))
    floating_ip = relationship(
        "FloatingIP",
        backref=backref(
            "floating_ip_assignments", cascade="all, delete-orphan", passive_deletes=True
        ),
    )
    # domain_id = Column(Integer, ForeignKey("domain.id", ondelete="CASCADE"))
    # domain = relationship(
    #     "Domain", backref=backref("floating_ip_assignments", cascade="all, delete-orphan", passive_deletes=True)
    # )
    domain_id = Column(String)


class RouteTable(Base):
    __tablename__ = "route_tables"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    def __repr__(self):
        return f"RouteTable(id={self.id!r}, name={self.name!r})"


class Route(Base):
    __tablename__ = "routes"

    destination = Column(String, primary_key=True)
    gateways = Column(StringSet)
    route_table_id = Column(Integer, ForeignKey("route_tables.id", ondelete="CASCADE"))
    route_table = relationship(
        "RouteTable", backref=backref("routes", cascade="all, delete-orphan", passive_deletes=True)
    )

    def __repr__(self):
        return f"Route(dest={self.destination!r}, gateways={self.gateways!r}, table={self.route_table!r})"


class PortForwarding(Base):
    __tablename__ = "port_forwardings"

    protocol = Column(String, primary_key=True)
    source_port = Column(Integer, primary_key=True)
    target_ip = Column(String)
    target_port = Column(Integer)

    def __repr__(self):
        return f"{self.protocol} :{self.source_port} -> {self.target_ip}:{self.target_port}"


class DNSRecord(Base):
    __tablename__ = "dns_records"

    name = Column(String, primary_key=True)
    type = Column(String, primary_key=True)
    ttl = Column(Integer)
    records = Column(StringList)

    def __repr__(self):
        return f"DNSEntry({self.name} {self.type} {self.ttl} {' '.join(self.records)}"


class Domain(Base):
    __tablename__ = "domains"

    id = Column(String, primary_key=True)
    private_ip = Column(String, unique=True)
    ipv6_address = Column(String, unique=True)
    os_type = Column(String)
    user_data = Column(Text)

    def __repr__(self):
        return f"Domain(id={self.id} private_ip={self.private_ip} ipv6_address={self.ipv6_address})"
