
.PHONY: clean resources

ROOT=../..
UTILS=$(ROOT)/utils
BUILD=$(ROOT)/build/lib
LITEINST=../src
LITEPROBES=$(LITEINST)/liteprobes
RESOURCES=resources
CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) 
DEPS = $(BUILD)/libliteinst.a

LIBS = -pthread

ARGS= -I. -I.. -I../src/liteprobes -I$(UTILS)/include  -I$(ROOT)/include -I../src/liteprobes

CC+  = $(CC) $(ARGS)
CXX+ = $(CXX) $(ARGS)

AR = ar
RM = rm
OPTS =

CODE = $(wildcard *.cpp)

OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)
# override OBJ  += $(filter-out $LITEPROBES/init.o, $(wildcard $(LITEPROBES)/*.o))
# override OBJ  += $(wildcard $(UTILS)/src/*.o)

.PHONY: all build install clean

all: build

liteinst:
	(cd $(LITEINST); make install);

resources:
	(cd resources; $(CXX+) $(CFLAGS) -O0 test_app_0.cpp -o test_app_0_O0.exe -L.. -ltests) #-L.. -ltests)
	(cd resources; $(CXX+) $(CFLAGS) -O3 test_app_1.cpp -o test_app_1_O0.exe -L.. -ltests) #-L.. -ltests)
	# $(CXX+) $(CFLAGS) -O3 $(RESOURCES)/test_app_0.cpp -o $(RESOURCES)/test_app_0_O3.exe # -L$(BUILD)/ -lliteinst

build: $(OBJ) liteinst 
	$(CXX+) $(LIBS) -shared -Wl,-soname,libtests.so -o libtests.so $(OBJ) -Wl,--whole-archive $(DEPS) -Wl,--no-whole-archive # -Wl,-init,boostrap

test: build resources
	# LD_PRELOAD=`pwd`/libtests.so ./$(RESOURCES)/test_app_0_O0.exe
	# LD_LIBRARY_PATH=`pwd`/ ./$(RESOURCES)/test_app_0_O0.exe
	LD_LIBRARY_PATH=`pwd`/ ./$(RESOURCES)/test_app_1_O0.exe

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) -fPIC -c -Wall -o $@ $<

clean:
	rm -rf *.o *.exe *.so *.a core* resources/*.exe resources/core* resources/vgcore.*
