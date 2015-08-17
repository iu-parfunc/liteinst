# Run "make docker2" in the root of this repo to get this parent image:
FROM iu-parfunc/ubiprof_dyninst

RUN apt-get install -y linux-tools-common

ADD . ubiprof_src/dyninst/

RUN cd ubiprof_src/dyninst/ && make
RUN cd ubiprof_src/dyninst/ && \
    make run DYNINST_ROOT=/usr/local
