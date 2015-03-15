CC = icc
CFLAGS =-lrt -Fa -O0 -g -finstrument-functions -fno-optimize-sibling-calls -std=c++0x 
LDFLAGS = -L. -lubiprof

ifeq ($(COMPILE_ARGS),)
	  COMPILE_ARGS= 10
endif

default:
	# ./make_source.sh ${COMPILE_ARGS}
	cp ../../../build/libubiprof.so .
	$(CC) $(CFLAGS)  40_deactivate.cpp $(LDFLAGS) -o 40_deactivate.exe

run:
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) PROFILER_TYPE=FIXED_BACKOFF ./40_deactivate.exe

clean:
	rm -f *.exe *.o *.s *.csv 40_deactivate.cpp libubiprof.so prof.out

