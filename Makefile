

ifeq ($(CABAL),)
  CABAL=cabal
endif

.phony: all lib bench

# ----------------------------------------

# TODO: build everything before running/benchmarking:
all:
# Nothing here currently.

lib:
	(cd ./zcatoggle/; make install; make docs)

# Run the benchmarks
bench: run-benchmarks.exe
	./run-benchmarks.exe

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init
	$(CABAL) install --bindir=. -j --disable-documentation

