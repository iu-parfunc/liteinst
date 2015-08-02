
CC = icpc -std=c++11

CFLAGS=-g -finstrument-functions -fno-optimize-sibling-calls

UBIPATH = ../build
LIBS = -L$(UBIPATH) -lubiprof

test: build run

build: finstrumentor_test.exe

finstrumentor_test.exe: finstrumentor_test.cpp
	$(CC) $(OPTS) $(LIBS) $(CFLAGS) finstrumentor_test.cpp -o finstrumentor_test.exe
	readelf -s finstrumentor_test.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt

run:
	LD_LIBRARY_PATH=$(UBIPATH):$(LD_LIBRARY_PATH) ./finstrumentor_test.exe

clean:
	rm -f *.o *.exe functions.txt
