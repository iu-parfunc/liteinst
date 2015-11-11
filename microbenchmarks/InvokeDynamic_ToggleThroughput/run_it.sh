#!/bin/bash

set -xe

module add java/1.8

javac IDDL.java IDDG.java
# This first run generates IDD.class without having a source file:
java IDDG

javac TimingHarness.java
time java TimingHarness 1 3 10000
