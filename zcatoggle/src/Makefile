
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

THIRD_PARTY = ../3rdparty
ASM_JIT = $(THIRD_PARTY)/AsmJit

HDRS = zca-types.h include/zca-toggle.h include/zca-utils.h
CODE = zca-toggle.cpp elf-provider.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)
DEPS0 :=  $(wildcard $(ASM_JIT)/*.cpp)
DEPS = $(DEPS0:.cpp=.o)
SRC = $(HDRS) $(CODE)
LIBS = -lelf

LIBSOUT = libzca-toggle.so libzca-toggle.a 

.PHONY: all build install 

all: build install

build:	$(LIBSOUT)
install: build
	if ! [ -d $(INSTALLD) ]; then mkdir -p $(INSTALLD); fi	
	cp $(LIBSOUT) $(INSTALLD)

test: build test.cpp hello_notify.exe elf-provider.o
	$(CPP) $(OPTS) -DPROBE_LOOP test.cpp libzca-toggle.a $(LIBS) -o test.exe
	./test.exe

# Benchmark binary generation targets
test_prof_off: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=0 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_off.exe

test_prof_on_single_probe: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=1 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_on_single_probe.exe

test_prof_on_probe_loop: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=1 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_on_probe_loop.exe

test_prof_off_probe_loop: build bench.cpp elf-provider.o
	$(CPP) $(CFLAGS) -DLOG_LEVEL=2 -DPROFILE=0 bench.cpp libzca-toggle.a $(LIBS) -o app_prof_off_probe_loop.exe

hello_notify.exe: hello_notify.c
	$(CC) hello_notify.c -o hello_notify.exe

.cpp.o:
	$(CPP) $(OPTS) -fPIC -c -Wall -o $@ $<
../3rdparty/AsmJit/.cpp.o:
	$(CC) $(OPTS) -fPIC -c -Wall -o $@ $<
.c.o:
	$(CC) $(OPTS) -fPIC -c -Wall -o $@ $<

# TEMP: trying ONE big call to ICC:
# libzca-toggle.so: $(CODE) $(DEPS0) 
# 	$(CC) $(LIBS) -fPIC -std=c++11 -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so $(CODE) $(DEPS0) -lc

libzca-toggle.so: $(OBJ) $(DEPS) 
	$(CC) $(LIBS) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so $(OBJ) $(DEPS) -lc

libzca-toggle.a: $(OBJ) $(DEPS) 
	$(AR) rc libzca-toggle.a $(OBJ) $(DEPS)

test6: test6.cpp
	icc -g -I../3rdparty/ -I./include ../3rdparty/AsmJit/*.cpp -O0 -std=c++11 test6.cpp -lelf -o test6.exe

test8: test8.cpp 
	icc -g -I../3rdparty/ -I./include ../3rdparty/AsmJit/*.cpp -O0 -std=c++11 test8.cpp $(INCLUDES) $(DEFS) -lelf -o test8.exe 
	./test8.exe

asm: asmjit.cpp
	icc -g -I../3rdparty ../3rdparty/AsmJit/*.cpp  -std=c++11 asmjit.cpp -lelf -o asm.exe
	./asm.exe

clean:
	$(RM) -f $(OBJ) $(DEPS) $(LIBSOUT) $(INSTALLD)/libzca-toggle.so $(INSTALLD)/libzca-toggle.a hello_notify.exe
