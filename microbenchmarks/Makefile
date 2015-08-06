
# Configuration stuff
# ----------------------------------------
# Note, obeys environment variables:
# 
# ----------------------------------------

ifeq ($(CABAL),)
  CABAL=cabal
endif

ifeq ($(MACHINECLASS),)
  MACHINECLASS=$(HOSTNAME)
endif

RUNID=$(shell hostname -s)_$(shell date "+%s")

ifeq ($(JENKINS_GHC),)
  JENKINS_GHC=7.8.2
endif

TRIALS=5

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
	(cd ..; make lib)

# Run the benchmarks. Used in top level run-tests.sh to run tests without rebuilding ubiprof
run: run-benchmarks.exe
	./run-benchmarks.exe --hostname=$(MACHINECLASS) --runid=$(RUNID) --keepgoing --trials=$(TRIALS) --name="Dynaprof_Benchmarks" --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

# Run the benchmarks
bench: lib run-benchmarks.exe
	./run-benchmarks.exe --hostname=$(MACHINECLASS) --runid=$(RUNID) --keepgoing --trials=$(TRIALS) --name="Dynaprof_Benchmarks" --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init --sandbox=../.cabal-sandbox/
#       Build the parent to make sure all the libs are installed:
	(cd ..; make run-benchmarks.exe)
#       Then we know that ours can build:
	$(CABAL) install --bindir=. --disable-documentation --with-ghc=ghc-$(JENKINS_GHC)
	./run-benchmarks.exe --help
	./run-benchmarks.exe -l

clean:
	rm -rf run-benchmarks.exe ./dist
