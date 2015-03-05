
# CC = icc
CC = gcc
CFLAGS = -O3

all: main.exe 
# main_vt.exe

main.exe: main.c
	$(CC) $(CFLAGS) main.c -o main.exe

main_vt.exe: main.c
	$(VTCC) $(CFLAGS) main.c -o main_vt.exe

run:
	./main.exe

clean:
	rm -f main.exe main_vt.exe *.o *.z *.otf *.thumb *.uctl *.def *.events
