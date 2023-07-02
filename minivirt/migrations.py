import itertools
import sqlite3

import sqlalchemy as sa


def _migration_0(engine):
    with engine.connect() as conn:
        conn.connection.executescript(
            """
BEGIN;

ALTER TABLE domains ADD ipv6_address VARCHAR;
ALTER TABLE networks ADD cidr6 VARCHAR;

DELETE FROM versions;
INSERT INTO versions VALUES('1');

COMMIT;
"""
        )


def _migration_1(engine):
    with engine.connect() as conn:
        conn.connection.executescript(
            """
BEGIN;

ALTER TABLE networks ADD name VARCHAR;

DELETE FROM versions;
INSERT INTO versions VALUES('2');

COMMIT;
"""
        )


migrations = {
    "0": _migration_0,
    "1": _migration_1,
}


def run_migrations(engine: sa.engine.Engine):
    create_initial(engine)

    with engine.connect() as conn:
        connection = conn.connection
        connection.row_factory = sqlite3.Row
        cur = connection.cursor()
        cur.row_factory = sqlite3.Row
        try:
            cur.execute("SELECT * FROM versions")
            res = cur.fetchall()
            assert len(res) == 1
            version = res[0]["version"]
        finally:
            cur.close()

    for key in itertools.dropwhile(lambda k: k != version, migrations):
        print(f'Executing migration from version "{key}"')
        migration_func = migrations[key]
        migration_func(engine)


def create_initial(engine):
    with engine.connect() as conn:
        conn.connection.executescript(
            """
CREATE TABLE IF NOT EXISTS bootstrap_tokens (
        token VARCHAR NOT NULL,
        expires_at DATETIME,
        PRIMARY KEY (token)
);

CREATE TABLE IF NOT EXISTS hosts (
        name VARCHAR NOT NULL,
        address VARCHAR,
        last_heartbeat DATETIME,
        PRIMARY KEY (name)
);

CREATE TABLE IF NOT EXISTS networks (
        id VARCHAR NOT NULL,
        cidr VARCHAR,
        PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS route_tables (
        id INTEGER NOT NULL,
        name VARCHAR,
        PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS port_forwardings (
        protocol VARCHAR NOT NULL,
        source_port INTEGER NOT NULL,
        target_ip VARCHAR,
        target_port INTEGER,
        PRIMARY KEY (protocol, source_port)
);

CREATE TABLE IF NOT EXISTS dns_records (
        name VARCHAR NOT NULL,
        type VARCHAR NOT NULL,
        ttl INTEGER,
        records VARCHAR,
        PRIMARY KEY (name, type)
);

CREATE TABLE IF NOT EXISTS domains (
        id VARCHAR NOT NULL,
        private_ip VARCHAR,
        os_type VARCHAR,
        user_data TEXT,
        PRIMARY KEY (id),
        UNIQUE (private_ip)
);

CREATE TABLE IF NOT EXISTS routes (
        destination VARCHAR NOT NULL,
        gateways VARCHAR,
        route_table_id INTEGER,
        PRIMARY KEY (destination),
        FOREIGN KEY(route_table_id) REFERENCES route_tables (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS versions (version VARCHAR);
INSERT INTO versions SELECT '0' WHERE NOT EXISTS (SELECT * FROM versions);
"""
        )
