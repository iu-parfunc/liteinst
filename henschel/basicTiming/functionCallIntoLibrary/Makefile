CC = icc
#CFLAGS =-lrt -Fa  //this will keep the assembly file around
CFLAGS =-lrt 
AR = ar
RM = rm

all: myLibrary.o libMyLibrary.so libMyLibrary.a mainDynamic.exe mainStatic.exe

myLibrary.o: myLibrary.c
	$(CC) -fPIC -g -c -Wall myLibrary.c

libMyLibrary.so: myLibrary.o
	$(CC) -shared -Wl,-soname,libMyLibrary.so -Wl,-init,myInit -o libMyLibrary.so myLibrary.o -lc

libMyLibrary.a: myLibrary.o
	$(AR) rc libMyLibrary.a myLibrary.o

mainDynamic.exe: libMyLibrary.so main.c
	$(CC) $(CFLAGS) main.c libMyLibrary.so -o mainDynamic.exe

mainStatic.exe: libMyLibrary.a main.c
	$(CC) $(CFLAGS) main.c libMyLibrary.a -o mainStatic.exe

clean:
	rm -f main.exe main_vt.exe *.o *.z *.otf *.thumb *.uctl *.def *.events *.marker main.s myLibrary.o libMyLibrary.so libMyLibrary.a mainStatic.exe mainDynamic.exe
