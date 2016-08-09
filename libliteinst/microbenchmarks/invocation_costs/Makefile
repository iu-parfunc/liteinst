.PHONY: clean all

NUM_INVOCATIONS=100000000

RUN_ARGS = $(NUM_INVOCATIONS)

ROOT=../../..
CFLAGS = #-finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L$(ROOT)/build/lib -lliteinst -lrt -pthread

CXX=g++

all: invocation_cost.exe

invocation_cost.exe:
	$(CXX) $(COMPILE_ARGS) --std=c++11 -g -c -I$(ROOT)/include -I$(ROOT)/utils/include -I. invocation_cost.cpp -o invocation_cost.o
	$(CXX) $(COMPILE_ARGS) --std=c++11 -g invocation_cost.o -o invocation_cost.exe  $(LIBS)

run: invocation_cost.exe
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./invocation_cost.exe $(RUN_ARGS)

clean:
	rm -f *.o *.exe *.out *.audit *.txt
