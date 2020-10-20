FROM ubuntu:20.04 AS builder
RUN apt-get update && apt-get install -y --no-install-recommends \
    make \
    erlang-dev \
    && rm -rf /var/cache/apt/lists/*
COPY . /reverCSP
RUN make -C /reverCSP compile

FROM ubuntu:20.04
RUN apt-get update && apt-get install -y --no-install-recommends \
    erlang-base \
    graphviz \
    && rm -rf /var/cache/apt/lists/*
COPY --from=builder /reverCSP /reverCSP
WORKDIR /reverCSP
