
# using icc requires some extra flags.
# these flags are probably different from when using gcc
# since a lot of what we do is not standard-c (asm, REG_RIP, etc not standard!)
#  gcc --std=gnu99
#  icc -- ?????
# it seems that icc also accepts the flag std=gnu99, good.
CC ?= icc
CXX ?= icpc

CODE = wait_free.c
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

INCLUDES = -I../../include -I../../deps/distorm/include

LIBS =
DEPS = ../../deps/distorm/distorm3.a

BUILD = ../../build
INSTALL = $(BUILD)/lib

AR = ar
RM = rm

.PHONY: test build distorm install

build: $(OBJ)

libcallpatch.a: build distorm
	if [ -d libcallpatch.a ]; then $(RM) -f libcallpatch.a; fi
	mkdir temp;cd temp; $(AR) -x ../$(DEPS)
	$(AR) rc libcallpatch.a  $(OBJ) temp/*.o
	$(RM) -rf temp

libcallpatch.so: build distorm
	$(CXX) $(LIBS) -shared -Wl,-soname,libcallpatch.so -o libcallpatch.so $(OBJ) $(DEPS)

install: libcallpatch.so libcallpatch.a
	if [ ! -d $(BUILD) ]; then mkdir $(BUILD); fi
	if [ ! -d $(INSTALL) ]; then mkdir $(INSTALL); fi
	(cp libcallpatch.so $(INSTALL))
	(cp libcallpatch.a $(INSTALL))

.c.o:
	  $(CC) -g -std=gnu99 $(INCLUDES) $(CFLAGS) $(OPTS) -fPIC -c -Wall  -o $@ $<

distorm:
	(cd ../../deps/distorm/make/linux/ && make clean && make CC=$(CC));

test:
	(cd ../tests; make test)

clean:
	$(RM) -f *.o *.exe *.so *.a $(INSTALL)/libcallpatch.*
