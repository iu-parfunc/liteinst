# CC = icc
CFLAGS=-DINDIVIDUAL
CC = gcc
CFLAGS = -O2
CPU = 2

all: noprof.exe 
	make run
# main_vt.exe

noprof.exe: noprof.c
	$(CC) $(CFLAGS) -Wl,--no-as-needed -finstrument-functions -fno-optimize-sibling-calls -L/u/crest-team/working_copies/onlineProfiling/build/noprof -lubiprof -lrt noprof.c -o nprof.exe
	(readelf -s nprof.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt)

run: noprof.exe
	LD_LIBRARY_PATH=../../../build/noprof:$(LD_LIBRARY_PATH) taskset -c $(CPU) ./nprof.exe

clean:
	rm -f *.exe *.o *.z *.otf *.thumb *.uctl *.def *.events
