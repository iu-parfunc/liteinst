
ROOT = ../..
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

# Setup build macros from env variables

EXTRA_WARNS= -pedantic 

ARGS= -g -Wall $(EXTRA_WARNS) -I$(ROOT)/include -I$(ROOT)/common/include -I$(ROOT)/deps/distorm/include

LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
DEPS = $(INSTALL)/libpointpatch.a 
LIBS = -lelf -lrt -lstdc++ -pthread

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) -std=c++11 $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = $(wildcard *.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean

all: build

build: $(OBJ) $(COMMON_OBJ)

libdynprobes.a: build 
	if [ -d libdynprobes.a ]; then $(RM) -f libdynprobes.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libdynprobes.a  $(OBJ) $(COMMON_OBJ) temp/*.o
	$(RM) -rf temp

libdynprobes.so: build
	$(CXX+) $(LIBS) -shared -Wl,-soname,libdynprobes.so -Wl,-init,messit -Wl,-init,init_point_patcher -o libdynprobes.so $(OBJ) $(COMMON_OBJ) $(DEPS) 

install: libmessit.a libmessit.so
	(cp libdynprobes.so $(INSTALL))
	(cp libdynprobes.a $(INSTALL))

.cpp.o:
	$(CXX+) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.a *.so $(INSTALL)/libdynprobes.*
