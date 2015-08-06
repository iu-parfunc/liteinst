
.phony: all lib bench doc devdoc docker clean FORCE
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
devdoc:
	doxygen scripts/Doxyfile_dev

# Extracts only the exported interface.
doc: FORCE
	doxygen scripts/Doxyfile

# .phony is not good enough for this:
FORCE:

docker:
# Remove a previous image, if it exists:
	docker rmi iu-parfunc/ubiprof || echo ok
# Remove any dangling as a general cleanup measure:
	docker rmi $(docker images -q --filter "dangling=true") || echo ok
# Then build our new one:
	docker build -t iu-parfunc/ubiprof .

# Finally, you can hop in the image with:
# docker run -it iu-parfunc/ubiprof

clean:
	(cd instrumentors/finstrument/src/; \
	make clean)
	(cd profilers/src/; \
	make clean)
	rm -rf build
