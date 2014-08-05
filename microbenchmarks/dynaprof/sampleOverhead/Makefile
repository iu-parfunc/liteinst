CC = icc
CFLAGS = -lrt -Fa -O0 -g
CFLAGSZCA = -lelf libzca-toggle.a
CFLAGSDYN = libdynaprof.a

ifeq ($(COMPILE_ARGS),)
  COMPILE_ARGS= 10
endif

default: generate compile

generate: 31_StartProfilerOverhead.cp1 31_StartProfilerOverhead.cp2
	cp ../../../zcatoggle/src/include/zca-toggle.hpp .
	cp ../../../zcatoggle/src/include/zca-types.hpp .
	cp ../../../zcatoggle/build/libzca-toggle.a .
	cp ../../../dynaprof/src/dynaprof.h .
	cp ../../../dynaprof/build/libdynaprof.a  .
	./make_source.sh ${COMPILE_ARGS}

compile:
	$(CC) $(CFLAGS)  31_StartProfilerOverhead.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 31_StartProfilerOverhead.exe

run:
	DYN_DISABLE_SELFTIMED=1 DYN_STRATEGY=NO_BACKOFF time ./31_StartProfilerOverhead.exe ${RUN_ARGS}

test:
	$(MAKE) run RUN_ARGS="1 1 1000"

lots:
	for ((i=0; i<100; i++)); do make run RUN_ARGS="1 1 0" 2>1 | grep SELFTIMED: ; done \
          | sed 's/SELFTIMED://' | tee samples.dat

clean:
	rm -f *.exe *.o *.s *.csv 31_StartProfilerOverhead.cpp libzca-toggle.a libdynaprof.a prof.out
