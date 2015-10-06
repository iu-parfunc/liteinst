
CC = cc
# CC = icc
# VTCC = vtcc -vt:cc icc
CFLAGS =

default: main.exe

LIBS = -lgcc
# -ldynamic

OPTS = -O0 -g
# This will do tail call optimization:
# OPTS = -fast

main.exe: main.c
	$(CC) $(OPTS) $(LIBS) $(CFLAGS) main.c -o main.exe


all:
	gcc -g -O0 $(LIBS) $(CFLAGS) main.c -o main_gcc_O0.exe
	gcc -O3 $(LIBS) $(CFLAGS) main.c -o main_gcc_O3.exe

	icc -g -O0 $(LIBS) $(CFLAGS) main.c -o main_icc_O0.exe
	icc -g -O3 -fast $(LIBS) $(CFLAGS) main.c -o main_icc_O3.exe

	clang -g -O0 $(LIBS) $(CFLAGS) main.c -o main_clang_O0.exe
	clang -g -O3  $(LIBS) $(CFLAGS) main.c -o main_clang_O3.exe

# TODO: this dumps a time series.  It could do a linear regression and
# print the slope which HSbencher could capture.
run: main.exe
	./main.exe

clean:
	rm -f main.exe main_vt.exe *.o *.z *.otf *.thumb *.uctl *.def *.events
