CC = icc
CFLAGS =-lrt -Fa -O0
CFLAGSZCA = -lelf libzca-toggle.a
CFLAGSDYN = libdynaprof.a

default:
	cp ../../../zcatoggle/src/include/zca-toggle.hpp .
	cp ../../../zcatoggle/src/include/zca-types.hpp .
	cp ../../../zcatoggle/build/libzca-toggle.a .
	cp ../../../dynaprof/src/dynaprof.h .
	cp ../../../dynaprof/build/libdynaprof.a  .
	./make_source.sh ${COMPILE_ARGS}
	$(CC) $(CFLAGS)  40_deactivate.cpp $(CFLAGSDYN) $(CFLAGSZCA) -o 40_deactivate.exe

run:
	DYN_DISABLE_SELFTIMED=1 ./40_deactivate.exe

clean:
	rm -f *.exe *.o *.s *.csv 40_deactivate.cpp libzca-toggle.a libdynaprof.a prof.out

