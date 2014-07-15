CC = icc
CFLAGS =-lrt -Fa -lelf -g -O0
all: main.exe

main.exe: main.cpp
	$(CC) $(CFLAGS) main.cpp libzca-toggle.a -o main.exe

clean:
	rm -f main.exe main_vt.exe *.o *.z *.otf *.thumb *.uctl *.def *.events *.marker main.s
