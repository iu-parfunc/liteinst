

ifeq ($(CABAL),)
  CABAL=cabal
endif

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init
	$(CABAL) install --bindir=. -j --disable-documentation

