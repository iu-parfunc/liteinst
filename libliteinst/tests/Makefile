
.PHONY: clean resources

ROOT=../..
UTILS=$(ROOT)/utils
BUILD=$(ROOT)/build/lib
LITEINST=../src
LITEPROBES=$(LITEINST)/liteprobes
APPS=apps
CFLAGS = --std=c++11 -g -Wall $(EXTRA_WARNS) 
CCFLAGS = -g -Wall $(EXTRA_WARNS) 
TEST_DEPS = $(BUILD)/libliteinst.test.a
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

REG_OBJ = init.o liteprobes_reg_test.o
ACTIVATION_OBJ = init.o liteprobes_activation_test.o
DEACTIVATION_OBJ = init.o liteprobes_deactivation_test.o
STANDALNE_OBJ = liteprobes_toggling.o

.PHONY: all build install clean apps

all: build

liteinst:
	(cd $(LITEINST); make install);

standalone: build 
	$(CXX+) $(LIBS) $(CFLAGS) liteprobes_toggling.o $(DEPS) -o liteprobes_toggling.exe

build: $(OBJ) liteinst apps 
	$(CXX+) $(LIBS) -shared -Wl,-soname,libtests_reg.so -o libtests_reg.so \
		$(REG_OBJ) -Wl,--whole-archive $(TEST_DEPS) -Wl,--no-whole-archive # -Wl,-init,boostrap
	$(CXX+) $(LIBS) -shared -Wl,-soname,libtests_act.so -o libtests_act.so \
		$(ACTIVATION_OBJ) -Wl,--whole-archive $(TEST_DEPS) -Wl,--no-whole-archive # -Wl,-init,boostrap
	$(CXX+) $(LIBS) -shared -Wl,-soname,libtests_deact.so -o libtests_deact.so \
	 	$(DEACTIVATION_OBJ) -Wl,--whole-archive $(TEST_DEPS) -Wl,--no-whole-archive # -Wl,-init,boostrap

apps: build
	(cd $(APPS); $(CXX+) $(CCFLAGS) -O0 test_app_0.cpp -o test_app_0_O0.exe) # -L.. -ltests_reg)
	(cd $(APPS); $(CC+) $(CCFLAGS) -O3 test_app_1.cpp -o test_app_1_O3.exe) #-L.. -ltests)

test: apps
	python tests.py

.cpp.o:
	$(CXX+) $(CFLAGS) $(ARGS) -fPIC -c -Wall -o $@ $<

clean:
	rm -rf *.o $(APPS)/*.exe *.exe *.so *.a core* $(APPS)/core* $(APPS)/vgcore.*
