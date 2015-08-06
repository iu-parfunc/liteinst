FROM ubuntu:15.10
RUN apt-get update && apt-get install -y \
     gcc g++ \
     libpapi-dev \
     make

ADD . ubiprof_src/

RUN cd ubiprof_src &&  \
    make lib
