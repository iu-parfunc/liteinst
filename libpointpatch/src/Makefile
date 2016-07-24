
ROOT = ../..
BUILD = $(ROOT)/build
INSTALL= $(ROOT)/build/lib

# using icc requires some extra flags.
# these flags are probably different from when using gcc
# since a lot of what we do is not standard-c (asm, REG_RIP, etc not standard!)
#  gcc --std=gnu99
#  icc -- ?????
# it seems that icc also accepts the flag std=gnu99, good.
CC= gcc
CXX= g++ 

CODE = patcher.c
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

COMMON = $(wildcard $(ROOT)/common/src/patch/*.c)
COMMON_OBJ = $(COMMON:.c=.o)

INCLUDES = -I$(ROOT)/include -I$(ROOT)/utils/include -I$(ROOT)/deps/distorm/include

LIBS =
DEPS = $(ROOT)/deps/distorm/distorm3.a $(BUILD)/lib/libutils.a

AR = ar
RM = rm

.PHONY: test build distorm install

build: $(OBJ) $(COMMON_OBJ)

libpointpatch.a: build distorm
	if [ -d libpointpatch.a ]; then $(RM) -f libpointpatch.a; fi
	if [ -d temp ]; then $(RM) -rf temp; fi
	mkdir temp;cd temp; $(foreach dep,$(DEPS),$(AR) -x ../$(dep);) 
	$(AR) rc libpointpatch.a  $(OBJ) $(COMMON_OBJ) temp/*.o
	$(RM) -rf temp

libpointpatch.so: build distorm 
	$(CXX) $(LIBS) -shared -Wl,-soname,libpointpatch.so -Wl,-init,init_point_patcher -o libpointpatch.so $(OBJ) $(COMMON_OBJ) $(DEPS)

install: libpointpatch.so libpointpatch.a
	if [ ! -d $(BUILD) ]; then mkdir $(BUILD); fi
	if [ ! -d $(INSTALL) ]; then mkdir $(INSTALL); fi
	(cp libpointpatch.so $(INSTALL))
	(cp libpointpatch.a $(INSTALL))

.c.o:
	  $(CC) -std=gnu99 $(INCLUDES) $(CFLAGS) $(OPTS) -fPIC -c -Wall  -o $@ $<

utils:
	if [ ! -d $(BUILD)/lib/libutils.a ]; then (cd utils/src; make install); fi

distorm:
	(cd ../../deps/distorm/make/linux/ && make clean && make CC=$(CC));

test:
	(cd ../tests; make test)

clean:
	$(RM) -f *.o *.exe *.so *.a $(INSTALL)/libpointpatch.*
