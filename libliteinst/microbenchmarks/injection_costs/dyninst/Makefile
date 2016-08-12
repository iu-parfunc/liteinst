all: mutator mutatee

# This is the default install location based on DynInst's own build
# config:
DYNINST_ROOT=/u/budkahaw/Builds/Installations/dyninst-9.2.0
# DYNINST_ROOT=/u/budkahaw/installations/dyninst

NUM_FUNCS=1024
NUM_RUNNERS=64
ROOT=../../../..
CODEGEN=../codegen

LIBS = -lrt -ldl -lpthread
LDPATH = -L$(DYNINST_ROOT)/lib/ -ldyninstAPI -lcommon -linstructionAPI -lstackwalk -lpcontrol -lpatchAPI -lparseAPI -lsymtabAPI -ldynDwarf -ldynElf -ldwarf

codegen:
	$(CXX) -std=c++11 -g -c $(CODEGEN)/funcs.cpp -o funcs.o 

mutator:  
	g++ -std=c++11 -g mutator.cpp -I$(ROOT)/utils/include -I$(CODEGEN) -I$(DYNINST_ROOT)/include/ -o mutator $(LDPATH) $(LIBS)
mutatee: codegen
	g++ -std=c++11 -g -c -I$(CODEGEN) mutatee.c -o mutatee.o
	g++ -std=c++11 -g mutatee.o funcs.o -g -o mutatee -pthread 

build:
	(cd $(CODEGEN); ./make_source.sh $(NUM_FUNCS))
	make mutator 
	make mutatee

run:
	./mutatee $(NUM_RUNNERS) & 
	export LD_LIBRARY_PATH=$(DYNINST_ROOT)/lib/:${LD_LIBRARY_PATH}; \
	  	export DYNINSTAPI_RT_LIB="$(DYNINST_ROOT)/lib/libdyninstAPI_RT.so"; \
				pgrep mutatee > pid; \
	  		./mutator "pid" $(NUM_FUNCS)
	pgrep mutatee | xargs -I {} kill -9 {}

clean:
	pgrep mutatee | xargs -I {} kill -9 {}
	rm -f mutatee mutator mutator_time mutatee_time *.o
