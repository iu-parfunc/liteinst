.PHONY: clean all

NUM_THREADS=16
FREQ=1000000
NUM_ITERATIONS=1000000
DURATION=10

RUN_ARGS = $(NUM_THREADS) $(DURATION) $(FREQ)

ROOT=../../..
CFLAGS = #-finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L$(ROOT)/build/lib -lliteinst -lrt -pthread

CXX=g++

all: toggle_throughput.exe

toggle_throughput.exe:
	$(CXX) $(COMPILE_ARGS) --std=c++11 -g -c -I$(ROOT)/include -I$(ROOT)/utils/include -I. liteprobes_toggle_throughput.cpp -o toggle_throughput.o
	$(CXX) $(COMPILE_ARGS) --std=c++11 -g toggle_throughput.o -o toggle_throughput.exe  $(LIBS)

run: toggle_throughput.exe
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./toggle_throughput.exe $(RUN_ARGS)

clean:
	rm -f *.o *.exe *.out *.audit *.txt
