#!/bin/bash

set -xe

javac IDDL.java IDDG.java
java IDDG

javac TimingHarness.java
time java TimingHarness
