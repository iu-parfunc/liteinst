
CC = g++
CFLAGS = -O2 $(NUMNOPS)

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

sig4:
	$(CC) $(CFLAGS) pt_main_4.c -lpthread -o pt_main_4.exe

sig5:
	$(CC) $(CFLAGS) pt_main_5.c -lpthread -o pt_main_5.exe

sig6:
	$(CC) $(CFLAGS) pt_main_6.c -lpthread -o pt_main_6.exe


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

run4:
	./pt_main_4.exe

run5:
	./pt_main_5.exe

run6:
	./pt_main_6.exe


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

brun4: sig4
	make run4

brun5: sig5
	make run5

brun6: sig6
	make run6


clean:
	rm -f *.exe *.o *.z *.otf *.thumb *.uctl *.def *.events
