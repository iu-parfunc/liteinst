
ROOT = ../../..
INSTALL= $(ROOT)/build/lib
DEPS_ROOT= $(ROOT)/deps

EXTRA_WARNS= -pedantic 

CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) 
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

# Setup build macros from env variables

ARGS= -I$(ROOT)/include -I$(ROOT)/common/include -I$(ROOT)/deps/distorm/include -I$(ROOT)/deps/elph/include

LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
DEPS = $(INSTALL)/libpointpatch.a 
LIBS = -lelf -lrt -lstdc++ -pthread

ELPH = $(DEPS_ROOT)/elph

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = $(wildcard *.cpp)
override CODE += $(wildcard $(ROOT)/common/src/lock/*.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

# DEP_OBJ = $(wildcard $(ELPH)/obj/*.o)
DEP_OBJ = $(ELPH)/obj/elf64_hdr.o $(ELPH)/obj/elf64.o $(ELPH)/obj/elf64_patch.o $(ELPH)/obj/elf64_payload.o $(ELPH)/obj/elf64_phr.o \
					$(ELPH)/obj/elf64_read.o $(ELPH)/obj/elf64_shr.o $(ELPH)/obj/elf64_write.o  $(ELPH)/obj/fdetect.o $(ELPH)/obj/sym_tab.o \
					$(ELPH)/obj/verbose.o 

.PHONY: all build install clean

all: build

build: $(OBJ) elph 

elph:
	(cd $(ELPH); make);

libaudit.a: build 
	if [ -d libaudit.a ]; then $(RM) -f libaudit.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libaudit.a  $(OBJ) $(DEP_OBJ) temp/*.o
	$(RM) -rf temp

libaudit.so: build elph
	$(CXX+) $(LIBS) -shared -Wl,-soname,libaudit.so -Wl,-init,audit -Wl,-init,init_point_patcher -o libaudit.so $(OBJ) $(DEP_OBJ) $(DEPS) 

install: libaudit.a libaudit.so
	(cp libaudit.so $(INSTALL))
	(cp libaudit.a $(INSTALL))

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.a *.so $(INSTALL)/libaudit.* $(DEP_OBJ)
