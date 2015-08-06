FROM ubuntu:15.10
RUN apt-get update && apt-get install -y \
     gcc g++ \
     libpapi-dev \
     make

ADD . ubiprof_src/

RUN pwd && cd ubiprof_src && pwd && \
    make -f Makefile_GCC lib
