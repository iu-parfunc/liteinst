CC = icc
CFLAGS = -lrt -Fa -O0 -g -finstrument-functions -fno-optimize-sibling-calls
LDFLAGS = -L. -lubiprof

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
	./make_source.sh ${COMPILE_ARGS}

compile:
	cp ../../../build/libubiprof.so .
	$(CC) $(CFLAGS)  31_StartProfilerOverhead.cpp $(LDFLAGS) -o 31_StartProfilerOverhead.exe

run:
	# PROFILER_TYPE=NO_BACKOFF time taskset -c $(CPU) ./31_StartProfilerOverhead.exe ${RUN_ARGS}
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) PROFILER_TYPE=FIXED_BACKOFF SAMPLE_SIZE=1000000000 time taskset -c $(CPU) ./31_StartProfilerOverhead.exe ${RUN_ARGS}

test:
	$(MAKE) run RUN_ARGS="1 1000 1000"

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
	rm -f *.exe *.o *.s *.csv 31_StartProfilerOverhead.cpp libubiprof.so prof.out samples.dat 1
