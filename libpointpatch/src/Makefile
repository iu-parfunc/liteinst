
# using icc requires some extra flags. 
# these flags are probably different from when using gcc
# since a lot of what we do is not standard-c (asm, REG_RIP, etc not standard!)
#  gcc --std=gnu99 
#  icc -- ?????
# it seems that icc also accepts the flag std=gnu99, good. 
CC=icc
CXX=icpc

CODE = patcher.c
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)

INCLUDES = -I../../include -I../../deps/distorm/include

LIBS =
DEPS = ../../deps/distorm/distorm3.a

BUILD = ../../build
INSTALL = $(BUILD)/lib

.PHONY: test build distorm libpointpatch install

build: $(OBJ)

libpointpatch: build distorm
	$(CXX) $(LIBS) -shared -Wl,-soname,libpointpatch.so -o libpointpatch.so $(OBJ) $(DEPS)

install: libpointpatch
	if [ ! -d $(BUILD) ]; then mkdir $(BUILD); fi
	if [ ! -d $(INSTALL) ]; then mkdir $(INSTALL); fi
	(cp libpointpatch.so $(INSTALL))

.c.o:
	  $(CC) -std=gnu99 $(INCLUDES) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

distorm:
	(cd ../../deps/distorm/make/linux/;make clean;make CC=$(CC));

test: 
	(cd ../tests; make test)


