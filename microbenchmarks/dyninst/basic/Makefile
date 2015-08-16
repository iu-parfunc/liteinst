all: mutator mutatee

mutator: mutator.o
	g++ mutator.o -B/u/budkahaw/installations/dyninst/include/ -L/u/budkahaw/installations/dyninst/lib/ -ldyninstAPI -o mutator
mutatee: mutatee.o
	gcc mutatee.o -std=gnu99 -g -o mutatee -lrt
mutatee.o: mutatee.c
	gcc mutatee.c -std=gnu99 -g -c -o mutatee.o

mutator_time: mutator_time.o
	g++ mutator_time.o -B/u/budkahaw/installations/dyninst/include/ -L/u/budkahaw/installations/dyninst/lib/ -ldyninstAPI -o mutator_time
mutatee_time: mutatee_time.o
	gcc mutatee_time.o -std=gnu99 -g -o mutatee_time -lrt
mutatee_time.o: mutatee_time.c
	gcc mutatee_time.c -std=gnu99 -g -c -o mutatee_time.o


run_time: mutatee_time mutator_time
	export LD_LIBRARY_PATH="/u/budkahaw/installations/dyninst/lib/":${LD_LIBRARY_PATH}; \
		export DYNINSTAPI_RT_LIB="/u/budkahaw/installations/dyninst/lib/libdyninstAPI_RT.so"; \
			./mutator_time "mutatee_time"

run : mutator mutatee
	export LD_LIBRARY_PATH="/u/budkahaw/installations/dyninst/lib/":${LD_LIBRARY_PATH}; \
		export DYNINSTAPI_RT_LIB="/u/budkahaw/installations/dyninst/lib/libdyninstAPI_RT.so"; \
			./mutator "mutatee"

clean:
	rm -f mutatee mutator
