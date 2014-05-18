
ARGS= -g -Wall
DEFS= -DTARGET_IA32E -DTARGET_LINUX -DFUND_TC_HOSTCPU=FUND_CPU_INTEL64
INCLUDES= 
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
# CC  = gcc $(ARGS)
# CPP = g++ -std=c++11 $(ARGS)
# CC  = clang $(ARGS)
# CPP = clang $(ARGS)
CC  = icc $(ARGS) 
CPP = icpc -std=c++11 $(ARGS) 
AR = ar
RM = rm
OPTS=-DLOG_LEVEL=2 -DPROFILE=1 # -DON

INSTALLD=../build
BENCHMARKS=../../benchmarks/gzip-1.6/instrumented ../../benchmarks/grep-2.18/instrumented/ ../../benchmarks/grep-2.18/instrumented/src

HDRS = zca-toggle.hpp profiler_types.hpp dynaprof.h logger.h 
CODE = profiler.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)
SRC = $(HDRS) $(CODE)
LIBS = -lelf -lpthread ../../zcatoggle/build/libzca-toggle.a # Make this libzca-toggle.so
# CFLAGS=-O0

LIBSOUT = libdynaprof.so libdynaprof.a 
HDRSOUT = dynaprof.h

.PHONY: all build install 

all: build install

build:	$(LIBSOUT)
install: build
	if ! [ -d $(INSTALLD) ]; then mkdir -p $(INSTALLD); fi	
	cp $(LIBSOUT) $(INSTALLD)
	echo $(BENCHMARKS) | xargs -n 1 cp $(LIBSOUT)
	echo $(BENCHMARKS) | xargs -n 1 cp $(HDRSOUT)

.cpp.o:
	$(CPP) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<
.c.o:
	$(CC) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

libdynaprof.so: $(OBJ) $(DEPS) 
	$(CC) $(LIBS) -shared -Wl,-soname,libdynaprof.so -Wl,-init,initZCAService -o libdynaprof.so $(OBJ) $(DEPS) -lc

libdynaprof.a : $(OBJ) $(DEPS)
	$(AR) rc libdynaprof.a profiler.o

prof : build profiler.o
	$(AR) rc libdynprof.a profiler.o
	$(CPP) $(OPTS) test_profiler.cpp libdynprof.a libzca-toggle.a $(LIBS) -o prof.exe
	./prof.exe

clean:
	$(RM) -f $(OBJ) $(DEPS) $(LIBSOUT) $(INSTALLD)/libdynaprof.so $(INSTALLD)/libdynaprof.a *.exe *.a *.so
