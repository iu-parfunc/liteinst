
# Dummy, temporary makefile.


DOCKER_NAME = dyninst_test
# [2015.09.21] Not working on my mac laptop:
# DOCKER_OPTS = --cap-add SYS_PTRACE --security-opt apparmor:unconfined
# Another workaround for ptrace/docker issues is running this:
#    sudo aa-complain /etc/apparmor.d/docker

DYNINST_ROOT    = $(HOME)/opt/dyninst
DYNINST_LIB     = $(DYNINST_ROOT)/lib
DYNINST_INCLUDE = $(DYNINST_ROOT)/include

# Assume dyninst installed and available in appropriate paths.
LIBS= -ldyninstAPI  -lrt

CC  ?= gcc
CXX ?= g++

# -ldyninstAPI_RT

all: main.exe

# The mutator is also the mutatee in the current test:
main.exe: main.cpp
	$(CXX) -std=c++11 -I$(DYNINST_INCLUDE) main.cpp -c -o main.o
	$(CXX) main.o -L$(DYNINST_LIB) $(LIBS) -o main.exe

run:
	DYNINSTAPI_RT_LIB=$(DYNINST_LIB)/libdyninstAPI_RT.so time ./main.exe

ryan:
	 CPLUS_INCLUDE_PATH=$(HOME)/opt/dyninst/include LIBRARY_PATH=$(HOME)/opt/dyninst/lib $(MAKE)

docker:
	docker build $(DOCKER_OPTS) -t $(DOCKER_NAME) .

rundock:
	docker run $(DOCKER_OPTS) -it $(DOCKER_NAME)


test:
	docker run $(DOCKER_OPTS) -it $(DOCKER_NAME) bash -c 'cd ubiprof_src/instrumentors/dyninst/'

clean:
	rm -f *.o *.exe
