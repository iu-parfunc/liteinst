
# Dummy, temporary makefile.

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
	docker build -t dyninst_test .

rundock:
	docker run -it dyninst_test


test:
	docker run -it dyninst_test bash -c 'cd ubiprof_src/instrumentors/dyninst/'

clean:
	rm -f *.o *.exe
