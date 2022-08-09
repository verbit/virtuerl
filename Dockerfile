FROM python:3.9.7-slim-bullseye AS base
ENV PATH="/root/.local/bin:$PATH"
WORKDIR /app


FROM base AS base_build
RUN apt-get update && apt-get install -y --no-install-recommends \
        gcc \
        libc-dev \
        pkg-config \
        libvirt-dev \
        git \
    && rm -rf /var/lib/apt/lists/*
COPY requirements.txt .
RUN pip install --user -r requirements.txt


FROM base_build as test_build
COPY requirements-dev.txt .
RUN pip install --user -r requirements-dev.txt


FROM base as base_run
RUN apt-get update && apt-get install -y --no-install-recommends \
        libvirt0 \
        iptables \
        libnftables1 \
    && rm -rf /var/lib/apt/lists/*


FROM base_run as test
COPY --from=test_build /root/.local /root/.local
COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "-m", "pytest"]
CMD ["tests"]


FROM base_run as default
LABEL org.opencontainers.image.source=https://github.com/verbit/restvirt
COPY --from=base_build /root/.local /root/.local
COPY . .
ENTRYPOINT ["/usr/bin/env", "python3", "main.py"]
