
.phony: all lib bench FORCE
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all: lib
# Nothing here currently.

# INST_OBJS := $(patsubst %.cpp,%.o,$(wildcard ./instrumentors/finstrument/src/*.cpp))

bench :
	(cd microbenchmarks; make bench)

lib:
	(cd instrumentors/finstrument/src/; \
	make CFLAGS='-DNDEBUG -O3')
	(cd profilers/src/; \
	make install CFLAGS='-DNDEBUG -O3')

libdebug: 
	(cd instrumentors/finstrument/src/; make)
	(cd profilers/src/; make install)

# Extracts all documentation from source files.
devdoc: FORCE
	doxygen scripts/Doxyfile_dev 

# Extracts only the exported interface. 
doc:    FORCE
	doxygen scripts/Doxyfile


FORCE: 

clean: 
	(cd instrumentors/finstrument/src/; \
	make clean) 
	(cd profilers/src/; \
	make clean)
	rm -rf build 
