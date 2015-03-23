# CC = icc
CFLAGS=-DNO_INIT -O2
CC = gcc
CPU = 2

ifeq ($(N),)
    N=3
endif

all: cache.exe
	(cd ../../../;make clean;make CFLAGS='$(CFLAGS)' lib)
	make run

run: cache.exe
	LD_LIBRARY_PATH=../../../build/:$(LD_LIBRARY_PATH) taskset -c $(CPU) ./cache.exe ${N}

cache.exe: cache.c
	$(CC) $(CFLAGS) cache.c -L../../../build -lubiprof -lrt -lpapi -o cache.exe

noprof.exe: noprof.c
	$(CC) $(CFLAGS) -finstrument-functions -fno-optimize-sibling-calls -L../../../build/noprof -lubiprof noprof.c -o nprof.exe
	(readelf -s nprof.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt)

clean:
	rm -f *.exe *.o *.z *.otf *.thumb *.uctl *.def *.events
