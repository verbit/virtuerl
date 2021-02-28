FROM debian:bullseye-slim
LABEL org.opencontainers.image.source=https://github.com/verbit/restvirt

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        cloud-image-utils \
        python3-flask python3-cheroot python3-passlib \
        python3-iptables python3-xmltodict python3-yaml python3-libvirt \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "main.py"]
