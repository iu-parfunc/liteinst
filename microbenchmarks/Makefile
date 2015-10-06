
# Configuration stuff
# ----------------------------------------
# Note, obeys environment variables:
#
# ----------------------------------------

ifeq ($(STACK),)
  STACK=stack
endif

ifeq ($(MACHINECLASS),)
  MACHINECLASS=$(HOSTNAME)
endif

RUNID=$(shell hostname -s)_$(shell date "+%s")

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

run-benchmarks.exe: run-micro-benchmarks.cabal run-benchmarks.hs
	$(STACK) setup
	$(STACK) --local-bin-path=. install
	./run-benchmarks.exe --help
	./run-benchmarks.exe -l

clean:
	rm -rf run-benchmarks.exe ./dist
