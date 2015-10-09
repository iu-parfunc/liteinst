CC = vtcc -vt:cc icc
CFLAGS =-DSTREAM_ARRAY_SIZE=40000000 -O3 -mcmodel=medium
#CFLAGS =-DSTREAM_ARRAY_SIZE=400000000 -O3 -mcmodel=medium

all: stream_c.exe

stream_c.exe: stream.c
	$(CC) $(CFLAGS) stream.c -o stream_c.exe

clean:
	rm -f stream_c.exe *.o
