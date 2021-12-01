from sqlalchemy import (
    Boolean,
    Column,
    DateTime,
    ForeignKey,
    Integer,
    String,
    TypeDecorator,
    Unicode,
    case,
    null,
)
from sqlalchemy.ext.hybrid import hybrid_property
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

    def __repr__(self):
        return f"Network({self.id!r}: {self.cidr!r})"


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
