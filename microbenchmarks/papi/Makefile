
# CC = icc
CC = gcc
CFLAGS = -O3

all: main.exe 
# main_vt.exe

main.exe: papi_timer.c
	$(CC) $(CFLAGS) -lpapi papi_timer.c -o pt.exe

run: main.exe
	./pt.exe

clean:
	rm -f *.exe *.o *.z *.otf *.thumb *.uctl *.def *.events
