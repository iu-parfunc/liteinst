
# Configuration stuff
# ----------------------------------------
# Responds to Env vars:
#   WHICHBENCH -- gets passed to hsbencher harness upon "make bench"
#   BENCHARGS  -- same as WHICHBENCH, gets passed to hsbencher harness
#   CABAL -- set executable to use for cabal
#   JENKINS_GHC -- X.Y.Z version number for GHC
# ----------------------------------------

ifeq ($(CABAL),)
  CABAL=cabal
endif

ifeq ($(JENKINS_GHC),)
#  JENKINS_GHC=7.6.3
# Changing default [2014.07.28]:
  JENKINS_GHC=7.8.2
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
all: lib run-benchmarks.exe
# Nothing here currently.

lib:
	(cd ./zcatoggle/src; make install; make docs)
	(cd ./dynaprof/src; make install)

# Run the benchmarks
bench: run-benchmarks.exe
	./run-benchmarks.exe --keepgoing --trials=$(TRIALS) --name="Dynaprof_Benchmarks" --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init
##	git submodule update --init --recursive --depth=1
#	git submodule init 
#	git submodule update 
#	cd HSBencher; git submodule init
#	cd HSBencher; git submodule update 
	$(CABAL) install ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) -j
	$(CABAL) install --only-dep -j --disable-documentation --with-ghc=ghc-$(JENKINS_GHC)
	$(CABAL) install --bindir=. --disable-documentation --with-ghc=ghc-$(JENKINS_GHC)
	./run-benchmarks.exe -l

clean:
	(cd ./zcatoggle/src; make clean)
	(cd ./dynaprof/src; make clean)
