CC = icc
CFLAGS =-lrt -Fa -O0
CFLAGSZCA = -lelf libzca-toggle.a
CFLAGSDYN = libdynaprof.a

default:
	cp ../../../zcatoggle/src/include/zca-toggle.hpp .
	cp ../../../zcatoggle/src/include/zca-types.hpp .
	cp ../../../zcatoggle/build/libzca-toggle.a .
	./make_source.sh ${COMPILE_ARGS}
	$(CC) $(CFLAGS) 32_StartProfilerOverhead.cpp $(CFLAGSZCA) -o 32_StartProfilerOverhead.exe

run:
	./32_StartProfilerOverhead.exe

clean:
	rm -f *.exe *.o *.s *.csv 32_StartProfilerOverhead.cpp libzca-toggle.a

