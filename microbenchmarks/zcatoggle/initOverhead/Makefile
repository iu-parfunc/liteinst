CC = icc
CFLAGS =-lrt -Fa -O0
CFLAGSZCA = -lelf libzca-toggle.a
CFLAGSDYN = libdynaprof.a

default:
	./make_source.sh ${COMPILE_ARGS}
	$(CC) $(CFLAGS) 32_StartProfilerOverhead.cpp $(CFLAGSZCA) -o 32_StartProfilerOverhead.exe

run:
	./32_StartProfilerOverhead.exe

clean:
	rm -f *.exe *.o *.s *.csv 32_StartProfilerOverhead.cpp

