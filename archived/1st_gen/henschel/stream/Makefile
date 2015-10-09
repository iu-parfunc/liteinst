CC = icc
CFLAGS =-DSTREAM_ARRAY_SIZE=4000000 -O3 -mcmodel=medium
#CFLAGS =-DSTREAM_ARRAY_SIZE=400000000 -O3 -mcmodel=medium
PARSE=/N/dcwan/scratch/henschel/ForRyan/pdtoolkit-3.17/x86_64/bin/cparse
INSTRUMENT=/N/dcwan/scratch/henschel/ForRyan/pdtoolkit-3.17/x86_64/bin/tau_instrumentor

all: stream_c.exe


stream.tau.c: stream.c
	$(PARSE) stream.c
	$(INSTRUMENT) stream.pdb stream.c -l -o stream.tau.c -c -spec ./TAUINST.SPEC

stream_c.exe: stream.tau.c
	$(CC) $(CFLAGS) stream.tau.c -o stream

clean:
	rm -f stream *.o stream.pdb stream.tau.c
