.PHONY: clean all

NUM_THREADS=4
NUM_ITERATIONS=10000000
ROOT=../../..
CFLAGS = -finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L$(ROOT)/build/lib -lfastinst -lrt -ldl -lpthread

CXX=g++

all: toggle_throughput.exe 

toggle_throughput.exe: 
	$(CXX) --std=c++11 -g -c $(CFLAGS) func.cpp -o func.o 
	$(CXX) --std=c++11 -g -c -I$(ROOT)/include -I$(ROOT)/common/include -I. toggle_throughput.cpp -o toggle_throughput.o 
	$(CXX) --std=c++11 -g toggle_throughput.o func.o -o toggle_throughput.exe  $(LIBS)

run: toggle_throughput.exe
	(readelf -s toggle_throughput.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt)
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./toggle_throughput.exe $(NUM_THREADS) $(NUM_ITERATIONS)

clean:
	rm -f *.o *.exe *.out *.audit *.txt
