
all:
	gcc -O0 -o bench timer.c

audit:
	g++ -std=c++11 -c -o audit.o -I ../deps/distorm/include audit.cpp

prof:
	g++ -std=c++11 -o app app.cpp -L../build/lib -lliteprof
	g++ -std=c++11 -o timer timer.c -L../build/lib -lliteprof

clean:
	rm -rf *.o
