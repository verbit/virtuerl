FROM python:3.9.7-bullseye AS base
LABEL org.opencontainers.image.source=https://github.com/verbit/restvirt

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
        libvirt-dev \
    && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install -r requirements.txt


FROM base AS test

COPY requirements-dev.txt .
RUN pip install -r requirements-dev.txt

COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "-m", "pytest"]
CMD ["tests"]


FROM base AS default

COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "main.py"]
