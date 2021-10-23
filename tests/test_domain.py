# import grpc
#
# import domain_pb2
# import domain_pb2_grpc
#
# if __name__ == "__main__":
#     channel = grpc.insecure_channel("localhost:50051")
#     stub = domain_pb2_grpc.DomainServiceStub(channel)
#     doms = stub.ListDomains(domain_pb2.ListDomainsRequest())
#     print("DOMAINS")
#     for dom in doms.domains:
#         print(dom)
#     channel.close()
