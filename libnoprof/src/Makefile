
ROOT = ../../
INSTALL = $(ROOT)/build/lib

EXTRA_WARNS= -pedantic
ARGS= -g -Wall $(EXTRA_WARNS) -I$(ROOT)/include -I$(ROOT)/deps/distorm/include -I$(ROOT)/common/include -I.

DEPS = $(INSTALL)/libpointpatch.a
LIBS =

CC  ?= gcc
CXX ?= g++ 

RM = rm
OPTS = 

CODE = empty_cyg_functions.cpp pointpatched_cyg_functions.cpp 
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean

all: build

build: $(OBJ)

libpointpatched.so: build
	  $(CC) $(LIBS) -shared -Wl,-soname,libpointpatched.so -o libpointpatched.so pointpatched_cyg_functions.o $(DEPS)

libempty.so: build
	  $(CC) $(LIBS) -shared -Wl,-soname,libempty.so -o libempty.so empty_cyg_functions.o 

install: libempty.so libpointpatched.so
	(cp libempty.so $(INSTALL))
	(cp libpointpatched.so $(INSTALL))

.cpp.o:
	  $(CXX) -std=c++11 $(ARGS) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.so $(INSTALL)/libempty.so $(INSTALL)/libpointpatched.so
