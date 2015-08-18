# Run "make docker2" in the root of this repo to get this parent image:
FROM iu-parfunc/ubiprof_dyninst:14.10

RUN apt-get update && \
    apt-get install -y linux-tools-common

# Stick this in a new/unique location to run a test:
ADD . ubiprof_src/instrumentors/dyninst_test/

RUN cd ubiprof_src/instrumentors/dyninst_test/ && ls -l && make
RUN cd ubiprof_src/instrumentors/dyninst_test/ && \
    make run DYNINST_ROOT=/usr/local
