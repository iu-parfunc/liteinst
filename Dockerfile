FROM ubuntu:15.10

ENV HOME /home/liteinst

RUN apt-get update && apt-get install -y \
           doxygen \
           gcc g++ \
           make\
           m4\
           vim\
           gdb\
           cmake\
           libboost-all-dev\
           wget\
           python\
           time

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
          echo 'deb http://download.fpcomplete.com/ubuntu trusty main' > /etc/apt/sources.list.d/fpco.list && \
                apt-get update && \
                apt-get install -y stack

COPY deps $HOME/deps
COPY apps $HOME/apps
RUN rm -rf $HOME/apps/benchmarks/bench-data
COPY include $HOME/include
COPY libliteinst $HOME/libliteinst
COPY libpointpatch $HOME/libpointpatch
COPY scripts $HOME/scripts
COPY libcallpatch $HOME/libcallpatch
COPY libliteprof $HOME/libliteprof
COPY Makefile $HOME/Makefile
COPY README.md $HOME/README.md
COPY utils $HOME/utils


RUN wget https://dl.dropboxusercontent.com/u/13531826/pldi17/bench-data.tar.gz -O /tmp/bench-data.tar.gz && \
      mv /tmp/bench-data.tar.gz $HOME/apps/benchmarks/ && cd $HOME/apps/benchmarks/ && \
      mkdir bench-data && tar -xzvf bench-data.tar.gz -C bench-data  && rm  -f bench-data.tar.gz

RUN wget https://dl.dropboxusercontent.com/u/13531826/pldi17/dyninst-9.3.0.tar.gz -O /tmp/dyninst-9.3.0.tar.gz && \
      mv /tmp/dyninst-9.3.0.tar.gz $HOME/../ && cd $HOME/../ && \
      mkdir dyninst-9.3.0 && tar -xzvf dyninst-9.3.0.tar.gz -C dyninst-9.3.0  && rm  -f dyninst-9.3.0.tar.gz
