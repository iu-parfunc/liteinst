
ROOT= ../..
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

EXTRA_WARNS= -pedantic 

override CFLAGS += --std=c++11 -g -Wall $(EXTRA_WARNS) -DAUDIT 
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

# Setup build macros from env variables

ARGS= -I. -I.. -I./liteprobes -I$(ROOT)/include -I$(ROOT)/utils/include -I$(ROOT)/deps/distorm/include -I$(ROOT)/deps/elph/include

LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
POINTPATCH = $(INSTALL)/libpointpatch.a 
UTILS = $(INSTALL)/libutils.a 
DEPS = $(POINTPATCH) $(UTILS) 
LIBS = -lstdc++ -pthread

ELPH = $(DEPS_ROOT)/elph

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) $(ARGS) $(CFLAGS) 

AR = ar
RM = rm
OPTS =
FIND = find

CODE = $(wildcard liteprobes/*.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean init

init:
	$(CXX) --version || echo OK
	# if [ $(CXX) --version != *"5.3.0"* ]]; then exit 1; fi

all: build

build: init $(OBJ) deps

deps: 
	if [ ! -d $(UTILS) ]; then (cd $(ROOT)/utils/src; make install); fi
	if [ ! -d $(POINTPATCH) ]; then (cd $(ROOT)/libpointpatch/src; make install); fi

libliteinst.a: build 
	if [ -d libliteinst.a ]; then $(RM) -f libliteinst.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libliteinst.a  $(OBJ) temp/*.o
	$(RM) -rf temp

INIT = liteprobes/init.o

libliteinst.test.a: build
	if [ -d libliteinst.test.a ]; then $(RM) -f libliteinst.test.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libliteinst.test.a  $(filter-out $(INIT),$(OBJ)) temp/*.o
	$(RM) -rf temp

libliteinst.so: build 
	$(CXX+) $(LIBS) -shared -Wl,-soname,libliteinst.so  -Wl,-init,init_point_patcher -o libliteinst.so $(OBJ) $(DEPS) # -Wl,-init,boostrap

install: libliteinst.a libliteinst.test.a libliteinst.so
	(cp libliteinst.so $(INSTALL))
	(cp libliteinst.a $(INSTALL))
	(cp libliteinst.test.a $(INSTALL))

.cpp.o:
	$(CXX+) -fPIC -c -Wall -o $@ $<

clean:
	$(FIND) . \( -name '*.o' -o -name '*.a' -o -name '*.so' \) -type f -delete
	$(RM) -f $(INSTALL)/libliteinst.*
