.PHONY: all lib clean deps test quicktest
.PHONY: utils_tests pointpatch_tests 
#summary ----------------------------------------

all: lib

# --------------------------------------------------------------------------------
# Building
# --------------------------------------------------------------------------------

# Build the 3rd-party dependencies for the core libs:
# This is currently implied-by/redundant with the "lib" target.
deps:
# Distorm is an in-place build:
	(cd deps/distorm/make/linux && make)

build:
	$(info --------------------------------------------)
	$(info  Building Liteinst                          )
	$(info --------------------------------------------)
	(make lib)
	$(info )
	(cd apps/benchmarks; make bench)

run:
	(cd microbenchmarks/injection_costs; ./run.sh)
#	(cd apps/benchmarks; make run)

plot:
	(cd scripts; ./summarize.py)
	(cp scripts/layouts/layouts results/Layout_Distribution-Table2/; cd results/Layout_Distribution-Table2; \
	 	./layouts raw/*; rm layouts)

lib:
	$(CXX) --version || echo ok
	$(CC) --version || echo ok
	(cd utils/src   && make CC=$(CC) CXX=$(CXX) && make install )
	(cd libpointpatch/src && make CC=$(CC) CXX=$(CXX) CFLAGS="$(CFLAGS) -DNDEBUG -DWAIT_SPIN_RDTSC" && make install )
	(cd libliteinst/src   && make CC=$(CC) CXX=$(CXX) && make install )
	(cd libliteprof/src   && make CC=$(CC) CXX=$(CXX) && make install )
	(cd libcallpatch/src  && make CC=$(CC) CXX=$(CXX) CFLAGS="$(CFLAGS) -DNDEBUG" && make install )

# Run all available tests, this is our regression testing / continuous
# integration target:
test: quicktest

# Only the fast-running tests:
quicktest: lib utils_tests 

utils_tests:
	(cd utils/tests && make test)

pointpatch_tests:
	(cd libpointpatch/tests && make test )

# --------------------------------------------------------------------------------
# Docker concerns:
# --------------------------------------------------------------------------------

docker_cleanup:
# Remove any dangling ones as a general cleanup measure:
	docker rmi $(docker images -q --filter "dangling=true") || echo ok

clean:
	rm -rf build results
	(cd utils/src/ && make clean)
	(cd libliteinst/src/ && make clean)
	(cd libliteprof/src/ && make clean)
	(cd deps/distorm/make/linux && make clean)
	(cd apps/benchmarks && make clean)
