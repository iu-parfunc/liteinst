CC = icc
CFLAGS = -lrt -Fa -O0 -g
CFLAGSZCA = -lelf libzca-toggle.a
CFLAGSDYN = libdynaprof.a

ifeq ($(COMPILE_ARGS),)
  COMPILE_ARGS= 10
endif

ifeq ($(CPU),)
  CPU=3
endif

ifeq ($(LOTS),)
  LOTS=500
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
	DYN_DISABLE_SELFTIMED=1 DYN_STRATEGY=NO_BACKOFF time taskset -c $(CPU) ./31_StartProfilerOverhead.exe ${RUN_ARGS}

test:
	$(MAKE) run RUN_ARGS="1 1 1000"

lots:
	for ((i=0; i < $(LOTS); i++)); do make run RUN_ARGS="1 1 0" 2>1 | grep SELFTIMED: ; done \
          | sed 's/SELFTIMED:/$i/' | tee samples.dat

sumthem:
	rm -f samples.dat
	set -xe; \
        for ((i=0; i < $(LOTS); i++)); do \
	for ((j=1; j <= 100; j++)); do \
          make run RUN_ARGS='1 '$$i' 0' 2>1 | grep NUMCALLS:  \
            | sed 's/NUMCALLS://' | tee -a samples_averaged.dat \
        ; done; done;

clean:
	rm -f *.exe *.o *.s *.csv 31_StartProfilerOverhead.cpp libzca-toggle.a libdynaprof.a prof.out
