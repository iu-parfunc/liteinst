
inst.so:
	g++ -std=c++11 -fPIC -I. -I../../deps/asmjit/src/asmjit/ -g -c -Wall -o instrument.o instrument.cpp 
	g++ -fPIC -shared -Wl,-soname,libinst.so -o libinst.so instrument.o ../../deps/asmjit/build/libasmjit.a

asmjit: inst.so
	g++ -std=c++11 -g -I. -o main main.cpp -L. -linst 
	LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ./main
			
all:
	$(CC) -g -c -o zca_app.o zca_app.cpp
	$(CC) -o zca_app zca_app.o -L../../build/lib -lubiprof -lrt -lm -lelf

clean:
	rm -f main zca_app *.out *.so *.o


