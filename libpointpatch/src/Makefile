
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
	if [ ! -d $(BUILD) ]; then mkdir $(BUILD) fi
	if [ ! -d $(INSTALL) ]; then mkdir $(INSTALL) fi
	(cp libpointpatch.so $(INSTALL));

.cpp.o:
	  $(CXX) -std=c++11 $(INCLUDES) $(CFLAGS) -fPIC -c -Wall -o $@ $<
.c.o:
	  $(CC) $(INCLUDES) $(CFLAGS) $(OPTS) -fPIC -c -Wall -o $@ $<

distorm:
	(cd ../../deps/distorm/make/linux/;make clean;make CC=$(CC));

test: 
	(cd ../tests; make test)


