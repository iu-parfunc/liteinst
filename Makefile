.PHONY: all lib microbench bench doc devdoc docker clean deps test quicktest
.PHONY: docker docker2 rundock rundock2 testdocker
.PHONY: fastinst_tests pointpatch_tests dyninst_inst_test
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all: lib
# Nothing here currently.

# INST_OBJS := $(patsubst %.cpp,%.o,$(wildcard ./instrumentors/finstrument/src/*.cpp))

# --------------------------------------------------------------------------------
# Building
# --------------------------------------------------------------------------------

# Build the 3rd-party dependencies for the core libs:
# This is currently implied-by/redundant with the "lib" target.
deps:
# Distorm is an in-place build:
	(cd deps/distorm/make/linux && make)

lib:
	$(CXX) --version || echo ok
	$(CC) --version || echo ok
	(cd utils/src   && make CC=$(CC) CXX=$(CXX) && make install )
	(cd libpointpatch/src && make CC=$(CC) CXX=$(CXX) CFLAGS="$(CFLAGS) -DNDEBUG -DWAIT_SPIN_RDTSC" && make install )
	(cd libliteinst/src   && make CC=$(CC) CXX=$(CXX) && make install )
	(cd libliteprof/src   && make CC=$(CC) CXX=$(CXX) && make install )

#       Ugh, something above wipes the build/ directory... FIXME
# (cd libcallpatch/src && make CC=$(CC) CXX=$(CXX) CFLAGS="$(CFLAGS) -DNDEBUG" && make install )
# (cd libubiprof/src   && make CC=$(CC) CXX=$(CXX) CFLAGS='$(CFLAGS) -DNDEBUG' && make install )

# Extracts all documentation from source files.
devdoc:
	doxygen scripts/Doxyfile_dev

# Extracts only the exported interface.
doc:
	doxygen scripts/Doxyfile

# --------------------------------------------------------------------------------
# Test and benchmark
# --------------------------------------------------------------------------------

microbench : lib
# 	(cd microbenchmarks && make run)
# 	(cd libfastinst/microbenchmarks && make run)

# Run all available tests, this is our regression testing / continuous
# integration target:
test: quicktest

# Only the fast-running tests:
quicktest: lib utils_tests liteinst_tests # pointpatch_tests 
# pointpatch_tests ... need to make these fast!
# prof_tests -- RRN: What's the deal with this one?

# inst_tests:
#	(cd instrumentors/tests/unit && make check)
#	(cd instrumentors/tests/integration && make check)

# prof_tests:
# 	(cd profilers/tests/unit && make check)
# 	(cd profilers/tests/integration && make check)

utils_tests:
	(cd utils/tests && make test)

pointpatch_tests:
	(cd libpointpatch/tests && make test )

liteinst_tests:
	(cd libliteinst/tests && make test)

callpatch_tests:
	(cd libcallpatch/tests && make test)

# Not running this from the default `make test` above due to Dyninst dependency.
dyninst_inst_test:
#       This isn't done yet but make sure it builds:
	(cd instrumentors/dyninst && make )

# --------------------------------------------------------------------------------
# Docker concerns:
# --------------------------------------------------------------------------------

DOCKERTAG=parfunc/liteprof:0.1

docker: clean
# Check what's there and build our new one:
	docker images
# Dockerfile's "build -f" seems basically broken in 1.8.  Hence this hackery:
	cp -f dockerfiles/Dockerfile_default ./Dockerfile
	docker build -t $(DOCKERTAG) .
	rm -f Dockerfile
# Finally, you can hop in the image with:
# docker run -it iu-parfunc/ubiprof

docker_cleanup:
# Remove any dangling ones as a general cleanup measure:
	docker rmi $(docker images -q --filter "dangling=true") || echo ok

# This verison builds on top of an image that includes DynInst.  It's
# MUCH bigger.
WDYNINST_TAG=iu-parfunc/ubiprof_dyninst:14.10
#
# TODO: This should eventually turn into the benchmarking image.

docker2: docker_dyninst
docker_dyninst: clean
# I know of no principled way to parameterize Dockerfiles.  Hence *this* hackery:
	cp -f dockerfiles/Dockerfile_wdyninst_14.10 ./Dockerfile
	docker build -t $(WDYNINST_TAG) .
	rm -f ./Dockerfile

# Sheer laziness:
rundock:
	docker run -it iu-parfunc/ubiprof bash

rundock2:
	docker run -it $(WDYNINST_TAG) bash

# The same as `make test` except inside the docker container resulting from `make docker`:
testdocker:
	docker run iu-parfunc/ubiprof bash -c 'cd ubiprof_src && make test'

# --------------------------------------------------------------------------------

clean:
# 	(cd instrumentors/finstrument/src/ && make clean)
# 	(cd profilers/src/ && make clean)
	# (cd libpointpatch/src/ && make clean)
	# (cd libpointpatch/tests/ && make clean)
	# (cd libfastinst/tests/ && make clean)
	# (cd libfastinst/src/ && make clean)
# 	(cd libubiprof/tests/ && make clean)
	(cd utils/src/ && make clean);
	(cd libliteinst/src/ && make clean)
	(cd libliteprof/src/ && make clean)

	rm -rf ./build run-benchmarks.hi run-benchmarks.exe run-benchmarks.o
	rm -rf ./doc ./devdoc

	(cd deps/distorm/make/linux && make clean)

distclean: clean
# Here is a harsher option for the deps.  We can't trust them to have
# good Makefiles:
#	git clean -fx deps/
