FROM ubuntu:16.04

RUN apt-get update

RUN export PATH=$HOME/.local/bin:$PATH

RUN apt-get install -y build-essential erlang
RUN apt-get install -y git
RUN apt-get install -y graphviz 

# Uncomment to avoid caching repository, i.e. to force clone the newest version of the tool  
# RUN pwd
RUN git clone https://github.com/tamarit/reverCSP --recursive

RUN cd reverCSP \
    && make compile \
    && cd csp_tracker \
    && mv bin_linux/* . \
    && make \
    && cd .. \
    && make compile

WORKDIR /reverCSP