
CC = gcc
CFLAGS = -O2

all: main.exe 

# Just build
sigtime:
	$(CC) $(CFLAGS) main.c -o main.exe

sig0:
	$(CC) $(CFLAGS) pt_main_0.c -lpthread -o pt_main_0.exe

sig1:
	$(CC) $(CFLAGS) pt_main_1.c -lpthread -o pt_main_1.exe

sig2:
	$(CC) $(CFLAGS) pt_main_2.c -lpthread -o pt_main_2.exe

sig3:
	$(CC) $(CFLAGS) pt_main_3.c -lpthread -o pt_main_3.exe

# Just run
runtime:
	./main.exe

run0:
	./pt_main_0.exe

run1:
	./pt_main_1.exe

run2:
	./pt_main_2.exe

run3:
	./pt_main_3.exe

# Build and run
btime: sigtime
	make runtime

brun0: sig0
	make run0

brun1: sig1
	make run1

brun2: sig2
	make run2

brun3: sig3
	make run3


clean:
	rm -f *.exe *.o *.z *.otf *.thumb *.uctl *.def *.events
