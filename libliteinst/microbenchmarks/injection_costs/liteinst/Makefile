.PHONY: clean all

NUM_FUNCS=120
ROOT=../../..
CFLAGS = -g

LIBS = -L$(ROOT)/build/lib -lliteinst -lrt -ldl -pthread

CXX=g++

all: injection_costs.exe 

injection_costs.exe: 
	./make_source.sh $(NUM_FUNCS)
	$(CXX) --std=c++11 -g -c funcs.cpp -o funcs.o 
	$(CXX) --std=c++11 -g -c -I$(ROOT)/include -I$(ROOT)/utils/include -I. injection_costs_non_contended.cpp -o injection_costs_non_contended.o 
	$(CXX) --std=c++11 -g -c -I$(ROOT)/include -I$(ROOT)/utils/include -I. injection_costs_contended.cpp -o injection_costs_contended.o 
	$(CXX) --std=c++11 -g injection_costs_contended.o funcs.o -o injection_costs_contended.exe  $(LIBS)
	$(CXX) --std=c++11 -g injection_costs_non_contended.o funcs.o -o injection_costs_non_contended.exe  $(LIBS)

run: injection_costs.exe
	make run-non-contended
	make run-contended

run-non-contended: injection_costs.exe
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./injection_costs_non_contended.exe $(NUM_FUNCS)

run-contended: injection_costs.exe
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./injection_costs_contended.exe $(NUM_FUNCS)


clean:
	./make_source.sh --clean
	rm -f *.o *.exe *.out *.audit *.txt
