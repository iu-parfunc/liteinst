#!/bin/bash

set -xe

javac IDDL.java IDDG.java
# This first run generates IDD.class without having a source file:
java IDDG

javac TimingHarness.java
time java TimingHarness
