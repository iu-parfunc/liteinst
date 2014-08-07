
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

ifeq ($(MACHINECLASS),)
  MACHINECLASS=$(HOSTNAME)
endif

RUNID=$(shell hostname -s)_$(shell date "+%s")

ifeq ($(JENKINS_GHC),)
#  JENKINS_GHC=7.6.3
# Changing default [2014.07.28]:
  JENKINS_GHC=7.8.2
endif

TRIALS=3
# TRIALS=1

# Google API authentication

ifeq ($(MACHINECLASS),delta)
  # Parfunc account / FusionTable_DynaprofUploader project:
  CID=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com
  SEC=MQ72ZWDde_1e1ihI5YE9YlEi
else
# ifeq ($(MACHINECLASS),mine)
ifneq (,$(filter $(MACHINECLASS),mine xmen))
  # Hit quota, using this one temporarily (the 1st FT project):
  CID=905767673358.apps.googleusercontent.com
  SEC=2a2H57dBggubW1_rqglC7jtK
else
  # Second general FT "project"
  CID=546809307027-8tm2lp5gtqg5o3pn3s016gd6467cf7j3.apps.googleusercontent.com
  SEC=148aQ08EPpgkb0DiYVLoT9X2
endif
endif

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
	./run-benchmarks.exe --retry=3 --hostname=$(MACHINECLASS) --runid=$(RUNID) --keepgoing --trials=$(TRIALS) --name="Dynaprof_Benchmarks" --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init
	$(CABAL) sandbox hc-pkg list
	$(CABAL) install ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) -j --force-reinstalls
	$(CABAL) install --only-dep -j --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls
	$(CABAL) install --bindir=. --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls
	./run-benchmarks.exe --help
	./run-benchmarks.exe -l

clean:
	(cd ./zcatoggle/src; make clean)
	(cd ./dynaprof/src; make clean)
	rm -rf ./run-benchmarks.exe ./dist
