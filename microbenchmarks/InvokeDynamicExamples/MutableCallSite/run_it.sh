#!/bin/bash

set -xe

javac IDDL.java IDDG.java TimingHarness.java

java IDDG

time java TimingHarness
