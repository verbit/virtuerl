FROM python:3.9.7-bullseye
LABEL org.opencontainers.image.source=https://github.com/verbit/restvirt

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
        libvirt-dev \
    && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "main.py"]
