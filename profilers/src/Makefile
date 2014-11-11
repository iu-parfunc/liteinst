
ARGS= -g -Wall
LIBS = -lpthread
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared

CCC = icc
CC = icc $(ARGS)
CPP = icpc -std=c++11 $(ARGS)
# COMP = gcc
# CC = gcc $(ARGS)
# CPP = g++ -std=c++0x $(ARGS)

AR = ar
RM = rm
OPTS = 

DEPS = ../../deps/distorm/distorm3.a

CODE = fprofiler.cpp sprofiler.cpp profiler.cpp initializer.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

INST_SRC_C := $(wildcard ../../instrumentors/finstrument/src/*.c)
INST_SRC_CPP:= $(wildcard ../../instrumentors/finstrument/src/*.cpp)
INST_SRC := $(INST_SRC_C)$(INST_SRC_CPP)

INST_OBJ0 = $(INST_SRC:.cpp=.o)
INST_OBJ = $(INST_OBJ0:.c=.o)

INSTALL = ../../build

.PHONY: all build install

libubiprof.so: libubiprof.a 
	$(CC) $(LIBS) -shared -Wl,-soname,libubiprof.so -o libubiprof.so $(OBJ) $(INST_OBJ) $(DEPS) -lc

libubiprof.a: build distorm
	mkdir temp;cd temp; $(AR) -x ../$(DEPS)
	$(AR) rc libubiprof.a  $(OBJ) $(INST_OBJ) temp/*.o

all: build

build: $(OBJ) 

install:
	-rm -r $(INSTALL)
	mkdir $(INSTALL)
	make libubiprof.so
	cp libubiprof.so $(INSTALL) 
	cp libubiprof.a $(INSTALL) 

distorm: 
	cd ../../deps/distorm/make/linux;make clean;make CC=$(CCC)

.cpp.o:
	  $(CPP) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<
.c.o:
	  $(CC) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.a *.so
	$(RM) -rf $(INSTALL)




