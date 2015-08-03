
.phony: all lib bench
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all: lib
# Nothing here currently.

# INST_OBJS := $(patsubst %.cpp,%.o,$(wildcard ./instrumentors/finstrument/src/*.cpp))

lib:
	(cd instrumentors/finstrument/src/; \
	make CFLAGS='-DNDEBUG -O3')
	(cd profilers/src/; \
	make install CFLAGS='-DNDEBUG -O3')

libdebug: 
	(cd instrumentors/finstrument/src/; make)
	(cd profilers/src/; make install)

clean: 
	(cd instrumentors/finstrument/src/; \
	make clean) 
	(cd profilers/src/; \
	make clean)
	rm -rf build 
