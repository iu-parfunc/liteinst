FROM ubuntu:15.10

ENV HOME /home/liteinst

RUN apt-get update && apt-get install -y \
           doxygen \
           gcc g++ \
           make\
           m4\
           vim\
           gdb\
           time

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
          echo 'deb http://download.fpcomplete.com/ubuntu trusty main' > /etc/apt/sources.list.d/fpco.list && \
                apt-get update && \
                apt-get install -y stack

COPY deps $HOME/deps
COPY apps $HOME/apps
COPY include $HOME/include
COPY libliteinst $HOME/libliteinst
COPY libpointpatch $HOME/libpointpatch
COPY scripts $HOME/scripts
COPY libcallpatch $HOME/libcallpatch
COPY libliteprof $HOME/libliteprof
COPY Makefile $HOME/Makefile
COPY README.md $HOME/README.md
COPY utils $HOME/utils
