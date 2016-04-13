
ROOT = ../../..
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

libaudit.a: build 
	if [ -d libaudit.a ]; then $(RM) -f libaudit.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libaudit.a  $(OBJ) $(COMMON_OBJ) temp/*.o
	$(RM) -rf temp

libaudit.so: build
	$(CXX+) $(LIBS) -shared -Wl,-soname,libaudit.so -Wl,-init,audit -Wl,-init,init_point_patcher -o libaudit.so $(OBJ) $(COMMON_OBJ) $(DEPS) 

install: libaudit.a libaudit.so
	(cp libaudit.so $(INSTALL))
	(cp libaudit.a $(INSTALL))

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.a *.so $(INSTALL)/libaudit.*
