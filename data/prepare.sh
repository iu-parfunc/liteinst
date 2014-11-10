#!/bin/sh

cabal sandbox init

cabal install ../HSBencher/hsbencher ../HSBencher/hsbencher-fusion/ ../HSBencher/hsbencher-tool/ ../HSBencher/hgdata/ --bindir=$(pwd) -j 
