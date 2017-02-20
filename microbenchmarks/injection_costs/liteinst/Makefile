.PHONY: clean all

NUM_FUNCS=1024
NUM_RUNNERS ?= 8

ROOT=../../..
CODEGEN=../codegen
CFLAGS = -g 

LIBS = -L$(ROOT)/build/lib -lliteinst -lrt -ldl -pthread

CXX=g++

all: injection_costs.exe 

injection_costs.exe: 
	(cd $(CODEGEN);./make_source.sh $(NUM_FUNCS))
	$(CXX) --std=c++11 -g -c $(CODEGEN)/funcs.cpp -o funcs.o 
	$(CXX) --std=c++11 -g -c -I$(CODEGEN) -I$(ROOT)/include -I$(ROOT)/utils/include -I. injection_costs_threaded.cpp -o injection_costs_threaded.o 
	$(CXX) --std=c++11 -g injection_costs_threaded.o funcs.o -o injection_costs_threaded.exe  $(LIBS)

build: injection_costs.exe

run: 
	make run-threaded

run-threaded: 
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./injection_costs_threaded.exe $(NUM_RUNNERS) $(NUM_FUNCS) 

run-non-contended: injection_costs.exe
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./injection_costs_non_contended.exe $(NUM_FUNCS)

run-contended: injection_costs.exe
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./injection_costs_contended.exe $(NUM_FUNCS)


clean:
	(cd $(CODEGEN); ./make_source.sh --clean)
	rm -f *.o *.exe *.out *.audit *.txt 
