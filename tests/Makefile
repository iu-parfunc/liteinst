
# COMPILE INSTRUCTIONS 
# -------------------- 
# CXX=icpc make
# or 
# CXX=g++ make 
# or 
# make 
#  which uses default setting of CXX (most likely g++) 

EXTRA_CFLAGS=-std=c++11

CFLAGS=$(EXTRA_CFLAGS) -g -finstrument-functions -fno-optimize-sibling-calls

UBIPATH = ../build
LIBS_g++  = -L$(UBIPATH) -lubiprof -lpthread -ldl 
LIBS_icpc = -L$(UBIPATH) -lubiprof -lpthread

LIBS=${LIBS_${CXX}}
test: build run

build: finstrumentor_test.exe

finstrumentor_test.exe: finstrumentor_test.cpp
	$(CXX) $(OPTS) $(CFLAGS) finstrumentor_test.cpp -o finstrumentor_test.exe $(LIBS)
	readelf -s finstrumentor_test.exe | grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt

run:
	LD_LIBRARY_PATH=$(UBIPATH):$(LD_LIBRARY_PATH) ./finstrumentor_test.exe

clean:
	rm -f *.o *.exe functions.txt core prof.out functions.txt

