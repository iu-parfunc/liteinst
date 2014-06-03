
# Configuration stuff
# ----------------------------------------

ifeq ($(CABAL),)
  CABAL=cabal
endif

ifeq ($(JENKINS_GHC),)
  JENKINS_GHC=7.6.3
endif

# TRIALS=3
TRIALS=1

# Google API authentication
# Parfunc account / FusionTable_DynaprofUploader project:
CID=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com
SEC=MQ72ZWDde_1e1ihI5YE9YlEi

.phony: all lib bench
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all:
# Nothing here currently.

lib:
	(cd ./zcatoggle/; make install; make docs)
	(cd ./dynaprof/; make install)

# Run the benchmarks
bench: run-benchmarks.exe
	./run-benchmarks.exe --keepgoing --trials=$(TRIALS) --name="Dynaprof_Benchmarks" --fusion-upload --clientid=$CID --clientsecret=$SEC 

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init
	$(CABAL) install ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion --disable-documentation --with-ghc=ghc-$(JENKINS_GHC)
	$(CABAL) install --only-dep -j --disable-documentation --with-ghc=ghc-$(JENKINS_GHC)
	$(CABAL) install --bindir=. --disable-documentation --with-ghc=ghc-$(JENKINS_GHC)
