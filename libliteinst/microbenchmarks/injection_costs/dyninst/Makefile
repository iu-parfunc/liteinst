all: mutator mutatee

# This is the default install location based on DynInst's own build
# config:
DYNINST_ROOT=/u/budkahaw/Builds/Installations/dyninst-9.2.0
# DYNINST_ROOT=/u/budkahaw/installations/dyninst

ROOT=../../../..

LIBS = -L$(ROOT)/build/lib -lliteinst -lrt -ldl -pthread
LDPATH = -L$(DYNINST_ROOT)/lib/ -ldyninstAPI -lcommon -linstructionAPI -lstackwalk -lpcontrol -lpatchAPI -lparseAPI -lsymtabAPI -ldynDwarf -ldynElf -ldwarf

mutator: mutator.o
	g++ mutator.o -I$(DYNINST_ROOT)/include/ $(LDPATH) -o mutator
mutatee: mutatee.o
	gcc mutatee.o -std=gnu99 -g -o mutatee -lrt
mutatee.o: mutatee.c
	gcc mutatee.c -std=gnu99 -g -c -o mutatee.o

mutator_time: mutator_time.o
	g++ mutator_time.o -I$(DYNINST_ROOT)/include/ $(LDPATH) -o mutator_time
mutatee_time: mutatee_time.o
	gcc mutatee_time.o -std=gnu99 -g -o mutatee_time -lrt
mutatee_time.o: mutatee_time.c
	gcc mutatee_time.c -std=gnu99 -g -c -o mutatee_time.o


run_time: mutatee_time mutator_time
	export LD_LIBRARY_PATH="$(DYNINST_ROOT)/lib/":${LD_LIBRARY_PATH}; \
		export DYNINSTAPI_RT_LIB="$(DYNINST_ROOT)/lib/libdyninstAPI_RT.so"; \
			./mutator_time "mutatee_time"

run : mutator mutatee
	export LD_LIBRARY_PATH="$(DYNINST_ROOT)/lib/":${LD_LIBRARY_PATH}; \
		export DYNINSTAPI_RT_LIB="$(DYNINST_ROOT)/lib/libdyninstAPI_RT.so"; \
			./mutator "mutatee"

.cpp.o:
	g++ -I$(DYNINST_ROOT)/include/ -I$(ROOT)/utils/include -g -c -Wall -o $@ $<

clean:
	rm -f mutatee mutator mutator_time mutatee_time *.o
