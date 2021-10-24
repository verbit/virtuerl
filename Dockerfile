FROM python:3.9.7-slim-bullseye AS base
ENV PATH="/opt/venv/bin:$PATH"
WORKDIR /app


FROM base AS base_build
RUN apt-get update && apt-get install -y --no-install-recommends \
        gcc \
        libc-dev \
        pkg-config \
        libvirt-dev \
    && rm -rf /var/lib/apt/lists/*
RUN python -m venv /opt/venv
COPY requirements.txt .
RUN pip install -r requirements.txt


FROM base_build as test_build
COPY requirements-dev.txt .
RUN pip install -r requirements-dev.txt


FROM base as base_run
RUN apt-get update && apt-get install -y --no-install-recommends \
        libvirt0 \
        iptables \
        cloud-image-utils \
    && rm -rf /var/lib/apt/lists/*


FROM base_run as test
COPY --from=test_build /opt/venv /opt/venv
COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "-m", "pytest"]
CMD ["tests"]


FROM base_run as default
LABEL org.opencontainers.image.source=https://github.com/verbit/restvirt
COPY --from=base_build /opt/venv /opt/venv
COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "main.py"]
