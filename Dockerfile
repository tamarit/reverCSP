FROM ubuntu:16.04

RUN apt-get update

RUN export PATH=$HOME/.local/bin:$PATH

RUN apt-get install -y build-essential erlang
RUN apt-get install -y git
RUN apt-get install -y graphviz 
RUN git clone https://github.com/tamarit/csp_reversible --recursive

RUN cd csp_reversible \
    && make compile \
    && cd csp_tracker \
    && mv bin_linux/* . \
    && make \
    && cd .. \
    && make compile