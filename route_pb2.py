# -*- coding: utf-8 -*-
# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: route.proto
"""Generated protocol buffer code."""
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()


from google.protobuf import empty_pb2 as google_dot_protobuf_dot_empty__pb2


DESCRIPTOR = _descriptor.FileDescriptor(
  name='route.proto',
  package='',
  syntax='proto3',
  serialized_options=None,
  create_key=_descriptor._internal_create_key,
  serialized_pb=b'\n\x0broute.proto\x1a\x1bgoogle/protobuf/empty.proto\"<\n\nRouteTable\x12\x14\n\x0cnetwork_name\x18\x01 \x01(\t\x12\n\n\x02id\x18\x02 \x01(\r\x12\x0c\n\x04name\x18\x03 \x01(\t\"\"\n\x14RouteTableIdentifier\x12\n\n\x02id\x18\x01 \x01(\r\".\n\x16ListRouteTablesRequest\x12\x14\n\x0cnetwork_name\x18\x01 \x01(\t\"<\n\x17ListRouteTablesResponse\x12!\n\x0croute_tables\x18\x01 \x03(\x0b\x32\x0b.RouteTable\";\n\x17\x43reateRouteTableRequest\x12 \n\x0broute_table\x18\x01 \x01(\x0b\x32\x0b.RouteTable\">\n\x0fRouteIdentifier\x12\x16\n\x0eroute_table_id\x18\x01 \x01(\r\x12\x13\n\x0b\x64\x65stination\x18\x02 \x01(\t\"F\n\x05Route\x12\x16\n\x0eroute_table_id\x18\x01 \x01(\r\x12\x13\n\x0b\x64\x65stination\x18\x02 \x01(\t\x12\x10\n\x08gateways\x18\x03 \x03(\t\"+\n\x11ListRoutesRequest\x12\x16\n\x0eroute_table_id\x18\x01 \x01(\r\",\n\x12ListRoutesResponse\x12\x16\n\x06routes\x18\x01 \x03(\x0b\x32\x06.Route\"(\n\x0fPutRouteRequest\x12\x15\n\x05route\x18\x01 \x01(\x0b\x32\x06.Route2\xd3\x03\n\x0cRouteService\x12\x35\n\rGetRouteTable\x12\x15.RouteTableIdentifier\x1a\x0b.RouteTable\"\x00\x12\x46\n\x0fListRouteTables\x12\x17.ListRouteTablesRequest\x1a\x18.ListRouteTablesResponse\"\x00\x12;\n\x10\x43reateRouteTable\x12\x18.CreateRouteTableRequest\x1a\x0b.RouteTable\"\x00\x12\x43\n\x10\x44\x65leteRouteTable\x12\x15.RouteTableIdentifier\x1a\x16.google.protobuf.Empty\"\x00\x12&\n\x08GetRoute\x12\x10.RouteIdentifier\x1a\x06.Route\"\x00\x12\x37\n\nListRoutes\x12\x12.ListRoutesRequest\x1a\x13.ListRoutesResponse\"\x00\x12&\n\x08PutRoute\x12\x10.PutRouteRequest\x1a\x06.Route\"\x00\x12\x39\n\x0b\x44\x65leteRoute\x12\x10.RouteIdentifier\x1a\x16.google.protobuf.Empty\"\x00\x62\x06proto3'
  ,
  dependencies=[google_dot_protobuf_dot_empty__pb2.DESCRIPTOR,])




_ROUTETABLE = _descriptor.Descriptor(
  name='RouteTable',
  full_name='RouteTable',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='network_name', full_name='RouteTable.network_name', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='id', full_name='RouteTable.id', index=1,
      number=2, type=13, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='name', full_name='RouteTable.name', index=2,
      number=3, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=44,
  serialized_end=104,
)


_ROUTETABLEIDENTIFIER = _descriptor.Descriptor(
  name='RouteTableIdentifier',
  full_name='RouteTableIdentifier',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='id', full_name='RouteTableIdentifier.id', index=0,
      number=1, type=13, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=106,
  serialized_end=140,
)


_LISTROUTETABLESREQUEST = _descriptor.Descriptor(
  name='ListRouteTablesRequest',
  full_name='ListRouteTablesRequest',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='network_name', full_name='ListRouteTablesRequest.network_name', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=142,
  serialized_end=188,
)


_LISTROUTETABLESRESPONSE = _descriptor.Descriptor(
  name='ListRouteTablesResponse',
  full_name='ListRouteTablesResponse',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='route_tables', full_name='ListRouteTablesResponse.route_tables', index=0,
      number=1, type=11, cpp_type=10, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=190,
  serialized_end=250,
)


_CREATEROUTETABLEREQUEST = _descriptor.Descriptor(
  name='CreateRouteTableRequest',
  full_name='CreateRouteTableRequest',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='route_table', full_name='CreateRouteTableRequest.route_table', index=0,
      number=1, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=252,
  serialized_end=311,
)


_ROUTEIDENTIFIER = _descriptor.Descriptor(
  name='RouteIdentifier',
  full_name='RouteIdentifier',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='route_table_id', full_name='RouteIdentifier.route_table_id', index=0,
      number=1, type=13, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='destination', full_name='RouteIdentifier.destination', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=313,
  serialized_end=375,
)


_ROUTE = _descriptor.Descriptor(
  name='Route',
  full_name='Route',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='route_table_id', full_name='Route.route_table_id', index=0,
      number=1, type=13, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='destination', full_name='Route.destination', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='gateways', full_name='Route.gateways', index=2,
      number=3, type=9, cpp_type=9, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=377,
  serialized_end=447,
)


_LISTROUTESREQUEST = _descriptor.Descriptor(
  name='ListRoutesRequest',
  full_name='ListRoutesRequest',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='route_table_id', full_name='ListRoutesRequest.route_table_id', index=0,
      number=1, type=13, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=449,
  serialized_end=492,
)


_LISTROUTESRESPONSE = _descriptor.Descriptor(
  name='ListRoutesResponse',
  full_name='ListRoutesResponse',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='routes', full_name='ListRoutesResponse.routes', index=0,
      number=1, type=11, cpp_type=10, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=494,
  serialized_end=538,
)


_PUTROUTEREQUEST = _descriptor.Descriptor(
  name='PutRouteRequest',
  full_name='PutRouteRequest',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='route', full_name='PutRouteRequest.route', index=0,
      number=1, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=540,
  serialized_end=580,
)

_LISTROUTETABLESRESPONSE.fields_by_name['route_tables'].message_type = _ROUTETABLE
_CREATEROUTETABLEREQUEST.fields_by_name['route_table'].message_type = _ROUTETABLE
_LISTROUTESRESPONSE.fields_by_name['routes'].message_type = _ROUTE
_PUTROUTEREQUEST.fields_by_name['route'].message_type = _ROUTE
DESCRIPTOR.message_types_by_name['RouteTable'] = _ROUTETABLE
DESCRIPTOR.message_types_by_name['RouteTableIdentifier'] = _ROUTETABLEIDENTIFIER
DESCRIPTOR.message_types_by_name['ListRouteTablesRequest'] = _LISTROUTETABLESREQUEST
DESCRIPTOR.message_types_by_name['ListRouteTablesResponse'] = _LISTROUTETABLESRESPONSE
DESCRIPTOR.message_types_by_name['CreateRouteTableRequest'] = _CREATEROUTETABLEREQUEST
DESCRIPTOR.message_types_by_name['RouteIdentifier'] = _ROUTEIDENTIFIER
DESCRIPTOR.message_types_by_name['Route'] = _ROUTE
DESCRIPTOR.message_types_by_name['ListRoutesRequest'] = _LISTROUTESREQUEST
DESCRIPTOR.message_types_by_name['ListRoutesResponse'] = _LISTROUTESRESPONSE
DESCRIPTOR.message_types_by_name['PutRouteRequest'] = _PUTROUTEREQUEST
_sym_db.RegisterFileDescriptor(DESCRIPTOR)

RouteTable = _reflection.GeneratedProtocolMessageType('RouteTable', (_message.Message,), {
  'DESCRIPTOR' : _ROUTETABLE,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:RouteTable)
  })
_sym_db.RegisterMessage(RouteTable)

RouteTableIdentifier = _reflection.GeneratedProtocolMessageType('RouteTableIdentifier', (_message.Message,), {
  'DESCRIPTOR' : _ROUTETABLEIDENTIFIER,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:RouteTableIdentifier)
  })
_sym_db.RegisterMessage(RouteTableIdentifier)

ListRouteTablesRequest = _reflection.GeneratedProtocolMessageType('ListRouteTablesRequest', (_message.Message,), {
  'DESCRIPTOR' : _LISTROUTETABLESREQUEST,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:ListRouteTablesRequest)
  })
_sym_db.RegisterMessage(ListRouteTablesRequest)

ListRouteTablesResponse = _reflection.GeneratedProtocolMessageType('ListRouteTablesResponse', (_message.Message,), {
  'DESCRIPTOR' : _LISTROUTETABLESRESPONSE,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:ListRouteTablesResponse)
  })
_sym_db.RegisterMessage(ListRouteTablesResponse)

CreateRouteTableRequest = _reflection.GeneratedProtocolMessageType('CreateRouteTableRequest', (_message.Message,), {
  'DESCRIPTOR' : _CREATEROUTETABLEREQUEST,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:CreateRouteTableRequest)
  })
_sym_db.RegisterMessage(CreateRouteTableRequest)

RouteIdentifier = _reflection.GeneratedProtocolMessageType('RouteIdentifier', (_message.Message,), {
  'DESCRIPTOR' : _ROUTEIDENTIFIER,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:RouteIdentifier)
  })
_sym_db.RegisterMessage(RouteIdentifier)

Route = _reflection.GeneratedProtocolMessageType('Route', (_message.Message,), {
  'DESCRIPTOR' : _ROUTE,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:Route)
  })
_sym_db.RegisterMessage(Route)

ListRoutesRequest = _reflection.GeneratedProtocolMessageType('ListRoutesRequest', (_message.Message,), {
  'DESCRIPTOR' : _LISTROUTESREQUEST,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:ListRoutesRequest)
  })
_sym_db.RegisterMessage(ListRoutesRequest)

ListRoutesResponse = _reflection.GeneratedProtocolMessageType('ListRoutesResponse', (_message.Message,), {
  'DESCRIPTOR' : _LISTROUTESRESPONSE,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:ListRoutesResponse)
  })
_sym_db.RegisterMessage(ListRoutesResponse)

PutRouteRequest = _reflection.GeneratedProtocolMessageType('PutRouteRequest', (_message.Message,), {
  'DESCRIPTOR' : _PUTROUTEREQUEST,
  '__module__' : 'route_pb2'
  # @@protoc_insertion_point(class_scope:PutRouteRequest)
  })
_sym_db.RegisterMessage(PutRouteRequest)



_ROUTESERVICE = _descriptor.ServiceDescriptor(
  name='RouteService',
  full_name='RouteService',
  file=DESCRIPTOR,
  index=0,
  serialized_options=None,
  create_key=_descriptor._internal_create_key,
  serialized_start=583,
  serialized_end=1050,
  methods=[
  _descriptor.MethodDescriptor(
    name='GetRouteTable',
    full_name='RouteService.GetRouteTable',
    index=0,
    containing_service=None,
    input_type=_ROUTETABLEIDENTIFIER,
    output_type=_ROUTETABLE,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='ListRouteTables',
    full_name='RouteService.ListRouteTables',
    index=1,
    containing_service=None,
    input_type=_LISTROUTETABLESREQUEST,
    output_type=_LISTROUTETABLESRESPONSE,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='CreateRouteTable',
    full_name='RouteService.CreateRouteTable',
    index=2,
    containing_service=None,
    input_type=_CREATEROUTETABLEREQUEST,
    output_type=_ROUTETABLE,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='DeleteRouteTable',
    full_name='RouteService.DeleteRouteTable',
    index=3,
    containing_service=None,
    input_type=_ROUTETABLEIDENTIFIER,
    output_type=google_dot_protobuf_dot_empty__pb2._EMPTY,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='GetRoute',
    full_name='RouteService.GetRoute',
    index=4,
    containing_service=None,
    input_type=_ROUTEIDENTIFIER,
    output_type=_ROUTE,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='ListRoutes',
    full_name='RouteService.ListRoutes',
    index=5,
    containing_service=None,
    input_type=_LISTROUTESREQUEST,
    output_type=_LISTROUTESRESPONSE,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='PutRoute',
    full_name='RouteService.PutRoute',
    index=6,
    containing_service=None,
    input_type=_PUTROUTEREQUEST,
    output_type=_ROUTE,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='DeleteRoute',
    full_name='RouteService.DeleteRoute',
    index=7,
    containing_service=None,
    input_type=_ROUTEIDENTIFIER,
    output_type=google_dot_protobuf_dot_empty__pb2._EMPTY,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
])
_sym_db.RegisterServiceDescriptor(_ROUTESERVICE)

DESCRIPTOR.services_by_name['RouteService'] = _ROUTESERVICE

# @@protoc_insertion_point(module_scope)