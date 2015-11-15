
ROOT = ../..
INSTALL= $(ROOT)/build/lib
# CFLAGS=-DPROBE_TRUE_EMPTY_ON -DPROBE_CPU_TIME
# CFLAGS=-DPROBE_HIST_ON -DPROBE_CPU_TIME

# Setup build macros from env variables
ifeq ($(ARG_PATCH),ARG_PATCH_SYNC)
override CFLAGS += -DARG_PATCH_SYNC
endif

ifeq ($(ARG_PATCH),ARG_PATCH_ASYNC)
override CFLAGS += -DARG_PATCH_ASYNC
endif

ifeq ($(INVOKE_PATCH),INVOKE_PATCH_SYNC)
override CFLAGS += -DINVOKE_PATCH_SYNC
endif

ifeq ($(INVOKE_PATCH),INVOKE_PATCH_CALL)
override CFLAGS += -DINVOKE_PATCH_CALL
endif

ifeq ($(AUDIT_PROBES),AUDIT_PROBES)
override CFLAGS += -DAUDIT_PROBES 
endif

ifeq ($(AUDIT_INIT_COST),AUDIT_INIT_COST)
override CFLAGS += -DAUDIT_INIT_COST
endif

ifeq ($(PROBE_TOGGLING),DISABLE_STRADDLERS)
override CFLAGS += -DDISABLE_STRADDLERS
else ifeq ($(PROBE_TOGGLING),DISABLE_ALL_PROBES)
override CFLAGS += -DDISABLE_ALL_PROBES
endif

EXTRA_WARNS= -pedantic 
#-Wextra
## -Wformat-nonliteral -Wcast-align -Wpointer-arith -Wbad-function-cast -Wmissing-prototypes -Wstrict-prototypes -Wmissing-declarations -Winline -Wundef -Wnested-externs -Wcast-qual -Wshadow -Wwrite-strings -Wno-unused-parameter -Wfloat-equal

ARGS= -g -Wall $(EXTRA_WARNS) -I$(ROOT)/include -I$(ROOT)/deps/distorm/include -I$(ROOT)/common/include -I.
LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
DEPS = $(INSTALL)/libpointpatch.a $(INSTALL)/libcallpatch.a
LIBS =

CC  ?= icc
CXX ?= icpc

CC+  = $(CC) $(ARGS)
CXX+ = $(CC) -std=c++11 $(ARGS)

AR = ar
RM = rm
OPTS =

COMMON = $(wildcard $(ROOT)/common/src/lock/*.cpp)
override COMMON +=  $(wildcard $(ROOT)/common/src/utils/*.cpp)
COMMON_OBJ0 = $(COMMON:.cpp=.o)
COMMON_OBJ  = $(COMMON_OBJ0:.c=.o)

CODE = finstrument_probe_provider.cpp probe_provider.cpp cyg_functions.cpp globals.cpp calibrate.cpp
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

.PHONY: all build install clean

all: build

build: $(OBJ) $(COMMON_OBJ)

libfastinst.a: build
	if [ -d libfastinst.a ]; then $(RM) -f libfastinst.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) # for DEP in $(DEPS); do $(AR) -x ../${DEP}; done
	$(AR) rc libfastinst.a  $(OBJ) $(COMMON_OBJ) temp/*.o
	$(RM) -rf temp

libfastinst.so: build
	$(CC+) $(LIBS) -shared -Wl,-soname,libfastinst.so -o libfastinst.so $(OBJ) $(COMMON_OBJ) $(DEPS)

install: libfastinst.a libfastinst.so
	(cp libfastinst.so $(INSTALL))
	(cp libfastinst.a $(INSTALL))

.cpp.o:
	$(CXX+) -std=c++11 $(ARGS) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.so $(INSTALL)/libfastinst.*
