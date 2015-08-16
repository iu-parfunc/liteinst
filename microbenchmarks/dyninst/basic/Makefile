all: mutator mutatee

# This is the default install location based on DynInst's own build
# config:
DYNINST_ROOT=/usr/local
# DYNINST_ROOT=/u/budkahaw/installations/dyninst

mutator: mutator.o
	g++ mutator.o -B$(DYNINST_ROOT)/include/ -L$(DYNINST_ROOT)/lib/ -ldyninstAPI -o mutator
mutatee: mutatee.o
	gcc mutatee.o -std=gnu99 -g -o mutatee -lrt
mutatee.o: mutatee.c
	gcc mutatee.c -std=gnu99 -g -c -o mutatee.o

mutator_time: mutator_time.o
	g++ mutator_time.o -B$(DYNINST_ROOT)/include/ -L$(DYNINST_ROOT)/lib/ -ldyninstAPI -o mutator_time
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

clean:
	rm -f mutatee mutator
