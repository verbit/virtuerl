# -*- coding: utf-8 -*-
# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: dns.proto
"""Generated protocol buffer code."""
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()


from google.protobuf import empty_pb2 as google_dot_protobuf_dot_empty__pb2


DESCRIPTOR = _descriptor.FileDescriptor(
  name='dns.proto',
  package='',
  syntax='proto3',
  serialized_options=None,
  create_key=_descriptor._internal_create_key,
  serialized_pb=b'\n\tdns.proto\x1a\x1bgoogle/protobuf/empty.proto\"1\n\x13\x44NSRecordIdentifier\x12\x0c\n\x04name\x18\x01 \x01(\t\x12\x0c\n\x04type\x18\x02 \x01(\t\"E\n\tDNSRecord\x12\x0c\n\x04name\x18\x01 \x01(\t\x12\x0c\n\x04type\x18\x02 \x01(\t\x12\x0b\n\x03ttl\x18\x03 \x01(\x04\x12\x0f\n\x07records\x18\x04 \x03(\t\"\x17\n\x15ListDNSRecordsRequest\"9\n\x16ListDNSRecordsResponse\x12\x1f\n\x0b\x64ns_records\x18\x01 \x03(\x0b\x32\n.DNSRecord\"5\n\x13PutDNSRecordRequest\x12\x1e\n\ndns_record\x18\x01 \x01(\x0b\x32\n.DNSRecord2\xf5\x01\n\x03\x44NS\x12\x32\n\x0cGetDNSRecord\x12\x14.DNSRecordIdentifier\x1a\n.DNSRecord\"\x00\x12\x43\n\x0eListDNSRecords\x12\x16.ListDNSRecordsRequest\x1a\x17.ListDNSRecordsResponse\"\x00\x12\x32\n\x0cPutDNSRecord\x12\x14.PutDNSRecordRequest\x1a\n.DNSRecord\"\x00\x12\x41\n\x0f\x44\x65leteDNSRecord\x12\x14.DNSRecordIdentifier\x1a\x16.google.protobuf.Empty\"\x00\x62\x06proto3'
  ,
  dependencies=[google_dot_protobuf_dot_empty__pb2.DESCRIPTOR,])




_DNSRECORDIDENTIFIER = _descriptor.Descriptor(
  name='DNSRecordIdentifier',
  full_name='DNSRecordIdentifier',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='name', full_name='DNSRecordIdentifier.name', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='type', full_name='DNSRecordIdentifier.type', index=1,
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
  serialized_start=42,
  serialized_end=91,
)


_DNSRECORD = _descriptor.Descriptor(
  name='DNSRecord',
  full_name='DNSRecord',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='name', full_name='DNSRecord.name', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='type', full_name='DNSRecord.type', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='ttl', full_name='DNSRecord.ttl', index=2,
      number=3, type=4, cpp_type=4, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='records', full_name='DNSRecord.records', index=3,
      number=4, type=9, cpp_type=9, label=3,
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
  serialized_start=93,
  serialized_end=162,
)


_LISTDNSRECORDSREQUEST = _descriptor.Descriptor(
  name='ListDNSRecordsRequest',
  full_name='ListDNSRecordsRequest',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
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
  serialized_start=164,
  serialized_end=187,
)


_LISTDNSRECORDSRESPONSE = _descriptor.Descriptor(
  name='ListDNSRecordsResponse',
  full_name='ListDNSRecordsResponse',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='dns_records', full_name='ListDNSRecordsResponse.dns_records', index=0,
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
  serialized_start=189,
  serialized_end=246,
)


_PUTDNSRECORDREQUEST = _descriptor.Descriptor(
  name='PutDNSRecordRequest',
  full_name='PutDNSRecordRequest',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='dns_record', full_name='PutDNSRecordRequest.dns_record', index=0,
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
  serialized_start=248,
  serialized_end=301,
)

_LISTDNSRECORDSRESPONSE.fields_by_name['dns_records'].message_type = _DNSRECORD
_PUTDNSRECORDREQUEST.fields_by_name['dns_record'].message_type = _DNSRECORD
DESCRIPTOR.message_types_by_name['DNSRecordIdentifier'] = _DNSRECORDIDENTIFIER
DESCRIPTOR.message_types_by_name['DNSRecord'] = _DNSRECORD
DESCRIPTOR.message_types_by_name['ListDNSRecordsRequest'] = _LISTDNSRECORDSREQUEST
DESCRIPTOR.message_types_by_name['ListDNSRecordsResponse'] = _LISTDNSRECORDSRESPONSE
DESCRIPTOR.message_types_by_name['PutDNSRecordRequest'] = _PUTDNSRECORDREQUEST
_sym_db.RegisterFileDescriptor(DESCRIPTOR)

DNSRecordIdentifier = _reflection.GeneratedProtocolMessageType('DNSRecordIdentifier', (_message.Message,), {
  'DESCRIPTOR' : _DNSRECORDIDENTIFIER,
  '__module__' : 'dns_pb2'
  # @@protoc_insertion_point(class_scope:DNSRecordIdentifier)
  })
_sym_db.RegisterMessage(DNSRecordIdentifier)

DNSRecord = _reflection.GeneratedProtocolMessageType('DNSRecord', (_message.Message,), {
  'DESCRIPTOR' : _DNSRECORD,
  '__module__' : 'dns_pb2'
  # @@protoc_insertion_point(class_scope:DNSRecord)
  })
_sym_db.RegisterMessage(DNSRecord)

ListDNSRecordsRequest = _reflection.GeneratedProtocolMessageType('ListDNSRecordsRequest', (_message.Message,), {
  'DESCRIPTOR' : _LISTDNSRECORDSREQUEST,
  '__module__' : 'dns_pb2'
  # @@protoc_insertion_point(class_scope:ListDNSRecordsRequest)
  })
_sym_db.RegisterMessage(ListDNSRecordsRequest)

ListDNSRecordsResponse = _reflection.GeneratedProtocolMessageType('ListDNSRecordsResponse', (_message.Message,), {
  'DESCRIPTOR' : _LISTDNSRECORDSRESPONSE,
  '__module__' : 'dns_pb2'
  # @@protoc_insertion_point(class_scope:ListDNSRecordsResponse)
  })
_sym_db.RegisterMessage(ListDNSRecordsResponse)

PutDNSRecordRequest = _reflection.GeneratedProtocolMessageType('PutDNSRecordRequest', (_message.Message,), {
  'DESCRIPTOR' : _PUTDNSRECORDREQUEST,
  '__module__' : 'dns_pb2'
  # @@protoc_insertion_point(class_scope:PutDNSRecordRequest)
  })
_sym_db.RegisterMessage(PutDNSRecordRequest)



_DNS = _descriptor.ServiceDescriptor(
  name='DNS',
  full_name='DNS',
  file=DESCRIPTOR,
  index=0,
  serialized_options=None,
  create_key=_descriptor._internal_create_key,
  serialized_start=304,
  serialized_end=549,
  methods=[
  _descriptor.MethodDescriptor(
    name='GetDNSRecord',
    full_name='DNS.GetDNSRecord',
    index=0,
    containing_service=None,
    input_type=_DNSRECORDIDENTIFIER,
    output_type=_DNSRECORD,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='ListDNSRecords',
    full_name='DNS.ListDNSRecords',
    index=1,
    containing_service=None,
    input_type=_LISTDNSRECORDSREQUEST,
    output_type=_LISTDNSRECORDSRESPONSE,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='PutDNSRecord',
    full_name='DNS.PutDNSRecord',
    index=2,
    containing_service=None,
    input_type=_PUTDNSRECORDREQUEST,
    output_type=_DNSRECORD,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
  _descriptor.MethodDescriptor(
    name='DeleteDNSRecord',
    full_name='DNS.DeleteDNSRecord',
    index=3,
    containing_service=None,
    input_type=_DNSRECORDIDENTIFIER,
    output_type=google_dot_protobuf_dot_empty__pb2._EMPTY,
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
  ),
])
_sym_db.RegisterServiceDescriptor(_DNS)

DESCRIPTOR.services_by_name['DNS'] = _DNS

# @@protoc_insertion_point(module_scope)