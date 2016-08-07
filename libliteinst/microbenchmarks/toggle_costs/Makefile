.PHONY: clean all

NUM_FUNCS=1000
ROOT=../../..
CFLAGS = #-finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L$(ROOT)/build/lib -lliteinst -lrt -ldl 

CXX=g++

all: toggle_cost.exe 

toggle_cost.exe: 
	./make_source.sh $(NUM_FUNCS)
	$(CXX) --std=c++11 -g -c $(CFLAGS) funcs.cpp -o funcs.o 
	$(CXX) --std=c++11 -g -c -I$(ROOT)/include -I$(ROOT)/utils/include -I$(ROOT)/common/include -I. toggle_cost.cpp -o toggle_cost.o 
	$(CXX) --std=c++11 -g toggle_cost.o funcs.o -o toggle_cost.exe  $(LIBS)

run: toggle_cost.exe
	(readelf -s toggle_cost.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt)
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./toggle_cost.exe $(NUM_FUNCS)

clean:
	./make_source.sh --clean
	rm -f *.o *.exe *.out *.audit *.txt
