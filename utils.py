import logging
from timeit import default_timer as timer

import grpc
import libvirt
from google.protobuf import empty_pb2


class UnaryUnaryInterceptor(grpc.ServerInterceptor):
    def intercept_service(self, continuation, handler_call_details):
        next = continuation(handler_call_details)
        if next is None:
            return None
        if next.unary_unary is None:
            return next

        def letsgo(request, context):
            start = timer()
            try:
                response = next.unary_unary(request, context)
            except libvirt.libvirtError as e:
                status_code = grpc.StatusCode.INTERNAL
                if e.get_error_code() in [
                    libvirt.VIR_ERR_NO_DOMAIN,
                    libvirt.VIR_ERR_NO_STORAGE_VOL,
                ]:
                    status_code = grpc.StatusCode.NOT_FOUND
                context.set_code(status_code)
                context.set_details(f"{e} ({e.get_error_code()})")
                response = empty_pb2.Empty()

            logging.debug(f"{handler_call_details.method} [{(timer() - start)*1000:.3f} ms]")
            return response

        return grpc.unary_unary_rpc_method_handler(
            letsgo,
            request_deserializer=next.request_deserializer,
            response_serializer=next.response_serializer,
        )
