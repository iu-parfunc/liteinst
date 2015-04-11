
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

TRIALS=6
# TRIALS=1
TOP=$(shell pwd)

# Google API authentication

ifeq ($(MACHINECLASS),delta)
  # Parfunc account / FusionTable_DynaprofUploader project:
  $(info Using DynaProf-specific FusionTable uploader.)
  CID=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com
  SEC=MQ72ZWDde_1e1ihI5YE9YlEi
else
# ifeq ($(MACHINECLASS),mine)
ifneq (,$(filter $(MACHINECLASS),mine xmen))
  # Hit quota, using this one temporarily (the 1st FT project):
#  CID=905767673358.apps.googleusercontent.com
#  SEC=2a2H57dBggubW1_rqglC7jtK
# ^ Got loaded on [2014.08.07]

  $(info Using general FusionTable uploader number 3)
  CID=759282369766-qbqb7sccab86m8j0jma6r3t21ng6uaoa.apps.googleusercontent.com
  SEC=TtfkUzUSR3LWfJ_udJi6wdO3
else
  $(info Using general FusionTable uploader number 2)
  CID=546809307027-8tm2lp5gtqg5o3pn3s016gd6467cf7j3.apps.googleusercontent.com
  SEC=148aQ08EPpgkb0DiYVLoT9X2
endif
endif

# TABLE=Dynaprof_Benchmarks
# [2014.08.07] Debugging problems, trying to start fresh:
TABLE=Dynaprof_Benchmarks2

.phony: all lib bench
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all: lib run-benchmarks.exe
# Nothing here currently.

# INST_OBJS := $(patsubst %.cpp,%.o,$(wildcard ./instrumentors/finstrument/src/*.cpp))

lib:
	(cd instrumentors/finstrument/src/; make)
	(cd profilers/src/; make install)
	# (cd ./zcatoggle/src; make install; make docs)
	# (cd ./dynaprof/src; make install)

#OVERRIDING ANY DECISION ABOUT CID AND SEC (WE HAD A LOT OF OUT OF QUOTA TROUBLE!)
echo "Overriding decision about CID and SEC" 
#CID=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com
#SEC=MQ72ZWDde_1e1ihI5YE9YlEi
CID=42301043663-7dnt2gj4svc3uhsiads8n2j6rqm5695n.apps.googleusercontent.com 
SEC=Drg2w-jGSMJGe9wdthz0sVx6


# Run the benchmarks
bench: run-benchmarks.exe
	./run-benchmarks.exe --retry=10 --hostname=$(MACHINECLASS) --runid=$(RUNID) --keepgoing --trials=$(TRIALS) --name=$(TABLE) --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init
	$(CABAL) sandbox hc-pkg list
	$(CABAL) sandbox hc-pkg unregister hsbencher-analytics || echo ok
	$(CABAL) install ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) -j --force-reinstalls
	$(CABAL) install --only-dep -j --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls
	$(CABAL) install --bindir=. --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls
	./run-benchmarks.exe --help
	./run-benchmarks.exe -l

plotter: run-benchmarks.exe
	(cd ./Plotting && $(CABAL) sandbox init --sandbox=$(TOP)/.cabal-sandbox/)
	(cd ./Plotting && $(CABAL) install --bindir=. . ../HSBencher/hsbencher-analytics --force-reinstalls --disable-documentation)

clean:
	(cd ./instrumentors/finstrument/src; make clean)
	(cd ./profilers/src; make clean)
	rm -rf ./run-benchmarks.exe ./dist
