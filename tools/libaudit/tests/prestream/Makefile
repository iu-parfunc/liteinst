.PHONY: clean all

APP_CODE = prestream_app.cpp 
LIB_CODE = prestream_lib.cpp

APP_OBJ = $(APP_CODE:.cpp=.o)
LIB_OBJ = $(LIB_CODE:.cpp=.o)

APP = $(APP_OBJ:.o=.exe)
	
CFLAGS = -O3

ARGS = -I../src/

LIBS = -L. -lprestream 

libprestream.so: 
	$(CXX) $(CFLAGS) -fPIC -c -Wall -o $(LIB_OBJ) $(LIB_CODE)
	$(CXX) -shared -Wl,-soname,libprestream.so -Wl,-init,prestream -o libprestream.so $(LIB_OBJ) 

test: libprestream.so
	$(CXX) --std=c++11 -g $(CFLAGS) $(APP_CODE) -o $(APP) 
	LD_PRELOAD=/u/budkahaw/Builds/ubiprof/tools/libaudit/tests/prestream/libprestream.so ./$(APP)

clean:
	rm -f *.o libprestream.so *.exe *.txt 
