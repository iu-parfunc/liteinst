
ARGS= -g -I./include/ -I../3rdparty -Wall
DEFS= -DTARGET_IA32E -DTARGET_LINUX -DFUND_TC_HOSTCPU=FUND_CPU_INTEL64
INCLUDES= -I ../../niknatar_prototype/libzca-src-195/zca/include/ -I ../../niknatar_prototype/3rdparty/pintool/source/include/pin/ -I ../../niknatar_prototype/3rdparty/pintool/source/include/pin/gen/ -I ../../niknatar_prototype/3rdparty/pintool/extras/components/include/util/ -I ../../niknatar_prototype/3rdparty/pintool/extras/components/include/ -I ../../niknatar_prototype/3rdparty/pintool/extras/xed2-intel64/include/
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

THIRD_PARTY = ../3rdparty
ASM_JIT = $(THIRD_PARTY)/AsmJit

HDRS = zca-types.h include/zca-toggle.h include/zca-utils.h
CODE = zca-toggle.cpp elf-provider.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)
DEPS0 :=  $(wildcard $(ASM_JIT)/*.cpp)
DEPS = $(DEPS0:.cpp=.o)
SRC = $(HDRS) $(CODE)
LIBS = -L../../deps/ -lelf -lpthread -lrt
CFLAGS = -O0 -I../../deps/

LIBSOUT = libzca-toggle.so libzca-toggle.a  

.PHONY: all build install 

all: build install

docs:

build:	$(LIBSOUT)
install: build
	if ! [ -d $(INSTALLD) ]; then mkdir -p $(INSTALLD); fi	
	cp $(LIBSOUT) $(INSTALLD)
#	for dir in $(BENCHMARKS); do if ! [ -d ${dir} ]; then mkdir -p ${dir}; fi cp $(LIBSOUT) ${dir} done

# echo $(BENCHMARKS) | xargs -n 1 cp $(LIBSOUT)

test: build test.cpp 
	$(CPP) $(OPTS) test.cpp libzca-toggle.a $(LIBS) -o test.exe
	./test.exe
        
tests : build tests.cpp
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=1 tests.cpp libzca-toggle.a $(LIBS) -o tests.exe
	./tests.exe

# Benchmark binary generation targets
test_prof_off: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=0 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_off.exe

test_prof_on_single_probe: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=1 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_on_single_probe.exe

test_prof_on_probe_loop: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=1 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_on_probe_loop.exe

test_prof_off_probe_loop: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=0 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_off_probe_loop.exe

test_prof_on_large_probe_count : build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=1 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_on_large_probe_count.exe

test_prof_on_activations: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=1 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_on_activations.exe


hello_notify.exe: hello_notify.c
	$(CC) hello_notify.c -o hello_notify.exe

.cpp.o:
	$(CPP) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<
../3rdparty/AsmJit/.cpp.o:
	$(CC) $(OPTS) -fPIC -c -Wall -o $@ $<
.c.o:
	$(CC) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

# TEMP: trying ONE big call to ICC:
# libzca-toggle.so: $(CODE) $(DEPS0) 
# 	$(CC) $(LIBS) -fPIC -std=c++11 -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so $(CODE) $(DEPS0) -lc

# libutils.so: $(OBJ) $(DEPS) 
# $(CC) $(LIBS) -shared -Wl,-soname,libutils.so -Wl,-o libutils.so utils.o -lc

libutils.a: utils.o 
	$(AR) rc libutils.a utils.o

libzca-toggle.so: $(OBJ) $(DEPS)
	$(CC) $(LIBS) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so $(OBJ) $(DEPS) -lc

libzca-toggle.a: $(OBJ) $(DEPS) libutils.a 
	$(AR) rc libzca-toggle.a  $(OBJ) $(DEPS)

prof : build profiler.o
	$(AR) rc libdynprof.a profiler.o
	$(CPP) $(OPTS) -O0 test_profiler.cpp libdynprof.a libzca-toggle.a libutils.a $(LIBS) -o prof.exe
	./prof.exe

notify : build read_elf.c
	$(CPP) read_elf.c $(LIBS) -o notifier 

prof1 : build profiler.o
	$(AR) rc libdynprof.a profiler.o

pdt : prof1 libzca-toggle.a
	$(CPP) $(ARGS) -O3 -DSTREAM_ARRAY_SIZE=4000000 stream.tau.c libdynprof.a libzca-toggle.a $(LIBS) -o stream
	./stream

tmp : build temp.cpp
	$(CPP) $(OPTS) temp.cpp libzca-toggle.a $(LIBS) -o t

### Start of Testing stuff ###
test6: test6.cpp
	icc -g -I../3rdparty/ -I./include ../3rdparty/AsmJit/*.cpp -O0 -std=c++11 test6.cpp -lelf -o test6.exe

test8: test8.cpp 
	icc -g -I../3rdparty/ -I./include ../3rdparty/AsmJit/*.cpp -O0 -std=c++11 test8.cpp $(INCLUDES) $(DEFS) -lelf -o test8.exe 
	./test8.exe

asm: asmjit.cpp
	icc -g -I../3rdparty ../3rdparty/AsmJit/*.cpp  -std=c++11 asmjit.cpp -lelf -o asm.exe
	./asm.exe

### End of Testing stuff ###

clean:
	$(RM) -f $(OBJ) $(DEPS) $(LIBSOUT) $(INSTALLD)/libzca-toggle.so $(INSTALLD)/libzca-toggle.a $(INSTALLD)/libdynprof.a hello_notify.exe *.exe *.a
