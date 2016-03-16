.PHONY: clean all

SRCS = $(wildcard *.cpp)

EXES = $(patsubst %.cpp,%.exe,$(SRCS))

# CFLAGS = -finstrument-functions -fno-inline -fno-optimize-sibling-calls

ARGS = -I../src/

LIBS = -L../../build/lib -lmessit -lpthread # -lelf -lrt
# LIBS = ../../build/lib/libfastinst.a
# LIBS = -L. -ltest 

all: $(EXES)

%.exe: %.cpp

	$(CXX) --std=c++11 -g $(CFLAGS) $(ARGS) -I../../include -I../../common/include -I../../deps/distorm/include $< -o $@ $(LIBS)


test: $(EXES)
	readelf -s perturb_single_thread.exe |grep FUNC | awk -F '[[:space:]]+' '{print $$3 "," $$9}' > functions.txt
	LD_PRELOAD=../../build/lib/libmessit.so PERTURBED_FUNCTIONS=_Z3foov ./perturb_single_thread.exe
	# LD_LIBRARY_PATH=../../build/lib:$(LD_LIBRARY_PATH) PERTURBED_FUNCTIONS=_Z3foov ./perturb_single_thread.exe
	
clean:
	rm -f *.o *.exe *.txt
