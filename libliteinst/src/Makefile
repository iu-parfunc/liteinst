
ROOT = ../..
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

EXTRA_WARNS= -pedantic 

CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) 
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

# Setup build macros from env variables

ARGS= -I.. -I$(ROOT)/include -I$(ROOT)/utils/include -I$(ROOT)/deps/distorm/include # -I$(ROOT)/deps/elph/include

LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
POINTPATCH = $(INSTALL)/libpointpatch.a 
UTILS = $(INSTALL)/libutils.a 
DEPS = $(POINTPATCH) $(UTILS) 
LIBS = -lelf -lrt -lstdc++ -pthread

ELPH = $(DEPS_ROOT)/elph

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) $(ARGS)

AR = ar
RM = rm
OPTS =
FIND = find

CODE = $(wildcard liteprobes/*.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean

all: build

build: $(OBJ) deps

deps: 
	if [ ! -d $(UTILS) ]; then (cd $(ROOT)/utils/src; make install); fi
	if [ ! -d $(POINTPATCH) ]; then (cd $(ROOT)/libpointpatch/src; make install); fi

libliteinst.a: build 
	if [ -d libliteinst.a ]; then $(RM) -f libliteinst.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libliteinst.a  $(OBJ) temp/*.o
	$(RM) -rf temp

libliteinst.so: build 
	$(CXX+) $(LIBS) -shared -Wl,-soname,libliteinst.so -Wl,-init,boostrap -Wl,-init,init_point_patcher -o libliteinst.so $(OBJ) $(DEPS) 

install: libliteinst.a libliteinst.so
	(cp libliteinst.so $(INSTALL))
	(cp libliteinst.a $(INSTALL))

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(FIND) . \( -name '*.o' -o -name '*.a' -o -name '*.so' \) -type f -delete
	$(RM) $(INSTALL)/libliteinst.*
