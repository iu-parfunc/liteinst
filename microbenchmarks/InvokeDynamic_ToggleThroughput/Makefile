
.PHONY: build run clean

RUN_ARGS ?= 1 3 10000

# This first run generates IDD.class without having a source file:
build: IDD.class TimingHarness.class IDDL.class

IDD.class: IDDG.java
	javac IDDG.java
	java IDDG

%.class: %.java
	javac $^

# javac TimingHarness.java

run: build
	time java TimingHarness $(RUN_ARGS)

clean:
	rm -f *.class
