
.PHONY: clean resources

ROOT=../..
UTILS=$(ROOT)/utils
BUILD=$(ROOT)/build/lib
LITEPROF=../src
APPS=apps
CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) 
CCFLAGS = -g -Wall $(EXTRA_WARNS) 
DEPS = $(BUILD)/libliteinst.test.a

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

.PHONY: all build install clean apps liteprof

all: build

liteprof:
	(cd $(LITEPROF); make install);

apps:
	(cd $(APPS); $(CC+) $(CCFLAGS) -O0 test_app_0.cpp -o test_app_0_O0.exe) # -L../$(BUILD) -lliteprof)
	(cd $(APPS); $(CC+) $(CCFLAGS) -O3 test_app_1.cpp -o test_app_1_O3.exe) # -L../$(BUILD) -lliteprof)

build: $(OBJ) liteprof apps 

test: build
	python tests.py

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) -fPIC -c -Wall -o $@ $<

clean:
	rm -rf *.o $(APPS)/*.exe *.exe *.so *.a core* $(APPS)/core* $(APPS)/vgcore.*
