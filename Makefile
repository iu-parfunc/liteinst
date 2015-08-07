.PHONY: all lib microbench bench doc devdoc docker clean
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all: lib
# Nothing here currently.

# INST_OBJS := $(patsubst %.cpp,%.o,$(wildcard ./instrumentors/finstrument/src/*.cpp))

microbench : lib
	(cd microbenchmarks; make run)

bench: lib
	(cd benchmarks; make run)

tests: lib
	(cd instrumentors/tests/unit;make check)
	(cd instrumentors/tests/integration;make check)
	(cd profilers/tests/unit;make check)
	(cd profilers/tests/integration;make check)

lib:
	(cd instrumentors/finstrument/src/; \
	make CFLAGS='-DNDEBUG -O3')
	(cd profilers/src/; \
	make install CFLAGS='-DNDEBUG -O3')

libdebug:
	(cd instrumentors/finstrument/src/; make)
	(cd profilers/src/; make install)

# Extracts all documentation from source files.
devdoc:
	doxygen scripts/Doxyfile_dev

# Extracts only the exported interface.
doc:
	doxygen scripts/Doxyfile

# .phony is not good enough for this:
# FORCE:

docker:
# Check what's there and build our new one:
	docker images
	docker build -t iu-parfunc/ubiprof .
# Remove any dangling ones as a general cleanup measure:
	docker rmi $(docker images -q --filter "dangling=true") || echo ok

# Finally, you can hop in the image with:
# docker run -it iu-parfunc/ubiprof

clean:
	(cd instrumentors/finstrument/src/; \
	make clean)
	(cd profilers/src/; \
	make clean)
	rm -rf ./build run-benchmarks.hi run-benchmarks.exe run-benchmarks.o
	rm -rf ./doc ./devdoc
