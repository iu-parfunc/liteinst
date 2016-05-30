.PHONY: clean all

NUM_FUNCS=10000
ROOT=../../..
CFLAGS = -finstrument-functions -fno-inline -fno-optimize-sibling-calls

LIBS = -L$(ROOT)/build/lib -lfastinst -lrt -ldl

CXX=g++

all: init_cost.exe 

init_cost.exe: 
	./make_source.sh $(NUM_FUNCS)
	$(CXX) --std=c++11 -g -c $(CFLAGS) funcs.cpp -o funcs.o 
	$(CXX) --std=c++11 -g -c -I$(ROOT)/include -I$(ROOT)/common/include -I. init_cost.cpp -o init_cost.o 
	$(CXX) --std=c++11 -g init_cost.o funcs.o -o init_cost.exe  $(LIBS)

run: init_cost.exe
	(readelf -s init_cost.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt)
	LD_LIBRARY_PATH=$(ROOT)/build/lib:$(LD_LIBRARY_PATH) ./init_cost.exe $(NUM_FUNCS)

clean:
	./make_source.sh --clean
	rm -f *.o *.exe *.audit *.txt
