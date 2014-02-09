
ARGS= -g -I./include/ -I../3rdparty -Wall
DEFS= -DTARGET_IA32E -DTARGET_LINUX -DFUND_TC_HOSTCPU=FUND_CPU_INTEL64
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
# CC  = gcc $(ARGS)
# CPP = g++ -std=c++11 $(ARGS)
# CC  = clang $(ARGS)
# CPP = clang $(ARGS)
CC  = icc -O0 -std=c++11 $(ARGS) $(DEFS) $(LDFLAGS)
CPP = icpc -std=c++11 $(ARGS) $(DEFS) $(LDFLAGS)
AR = ar
RM = rm

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
	$(CPP) test.cpp libzca-toggle.a $(LIBS) -o test.exe
	./test.exe

hello_notify.exe: hello_notify.c
	$(CC) hello_notify.c -o hello_notify.exe

.cpp.o:
	$(CC) -fPIC -c -Wall -o $@ $<
../3rdparty/AsmJit/.cpp.o:
	$(CC) -fPIC -c -Wall -o $@ $<
.c.o:
	$(CC) -fPIC -c -Wall -o $@ $<

libzca-toggle.so: $(OBJ) $(DEPS)
	$(CC) $(LIBS) -shared -Wl,-soname,libzca-toggle.so -Wl,-init,initZCAService -o libzca-toggle.so $(OBJ) $(DEPS) -lc

libzca-toggle.a: $(OBJ) $(DEPS) 
	$(AR) rc libzca-toggle.a $(OBJ) $(DEPS)

clean:
	$(RM) -f $(OBJ) $(DEPS) $(LIBSOUT) $(INSTALLD)/libzca-toggle.so $(INSTALLD)/libzca-toggle.a hello_notify.exe
