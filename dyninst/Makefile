
# Dummy, temporary makefile.

DYNINST_ROOT    = $(HOME)/opt/dyninst
DYNINST_LIB     = $(DYNINST_ROOT)/lib
DYNINST_INCLUDE = $(DYNINST_ROOT)/include

# Assume dyninst installed and available in appropriate paths.
LIBS= -ldyninstAPI  -lrt

CC=gcc
CXX=g++

# -ldyninstAPI_RT

all: mutatee.exe mutator.exe

mutatee.exe: mutatee.c
	$(CC) -std=c99 -g -lrt mutatee.c -o mutatee.exe

mutator.exe: mutator.cpp
	$(CXX) -std=c++11 -I$(DYNINST_INCLUDE) mutator.cpp -c -o mutator.o
	$(CXX) mutator.o -L$(DYNINST_LIB) $(LIBS) -o mutator.exe

run:
	DYNINSTAPI_RT_LIB=$(DYNINST_LIB)/libdyninstAPI_RT.so time ./mutator.exe ./mutatee.exe

ryan:
	 CPLUS_INCLUDE_PATH=$(HOME)/opt/dyninst/include LIBRARY_PATH=$(HOME)/opt/dyninst/lib $(MAKE)


clean:
	rm -f *.o *.exe
