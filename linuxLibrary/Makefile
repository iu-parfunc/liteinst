CC = icc
AR = ar
RM = rm

all: myLibrary.o libMyLibrary.so libMyLibrary.a mainDynamic mainStatic

myLibrary.o: myLibrary.c
	$(CC) -fPIC -g -c -Wall myLibrary.c

libMyLibrary.so: myLibrary.o
	$(CC) -shared -Wl,-soname,libMyLibrary.so -Wl,-init,myInit -o libMyLibrary.so myLibrary.o -lc

libMyLibrary.a: myLibrary.o
	$(AR) rc libMyLibrary.a myLibrary.o

mainDynamic: libMyLibrary.so main.c
	$(CC) main.c libMyLibrary.so -o mainDynamic

mainStatic: libMyLibrary.a main.c
	$(CC) main.c libMyLibrary.a -o mainStatic

clean:
	$(RM) -f myLibrary.o libMyLibrary.so libMyLibrary.a mainStatic mainDynamic
