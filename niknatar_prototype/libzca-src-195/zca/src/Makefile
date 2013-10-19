# Copyright (C) 2012 Intel Corporation
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#   * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#   * Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
#   * Neither the name of Intel Corporation nor the names of its
#     contributors may be used to endorse or promote products derived
#     from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
# WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.


TOP = ../..

CC = gcc
CXX = g++

OUT ?= linux64

PIN_TARGET_ARCH = intel64
PREPROC = -DTARGET_IA32E -DHOST_IA32E -fPIC -DTARGET_LINUX -DUSING_XED

BUILDDIR = $(OUT)
PINDIR = $(TOP)/../3rdparty/pintool

INCLUDE  = -I ../include
INCLUDE += -I $(PINDIR)/source/include -I $(PINDIR)/source/include/gen
INCLUDE += -I $(PINDIR)/extras/xed2-$(PIN_TARGET_ARCH)/include
INCLUDE += -I $(PINDIR)/extras/components/include
INCLUDE += -I $(PINDIR)/source/include/pin -I $(PINDIR)/source/include/pin/gen

# Pin binary to copy
PINBIN = pinbin

LDFLAGS  = -Wl,--hash-style=sysv -shared -Wl,-Bsymbolic -fPIC
#LDFLAGS += -Wl,--version-script=./pintool.ver 
LDFLAGS += -L $(PINDIR)/$(PIN_TARGET_ARCH)/lib
LDFLAGS += -L $(PINDIR)/$(PIN_TARGET_ARCH)/lib-ext
LDFLAGS += -L $(PINDIR)/extras/xed2-$(PIN_TARGET_ARCH)/lib
LDLIBS = -lpin -lsapin -lxed -ldwarf -lelf -ldl -lpthread

CXXFLAGS = -ggdb -fomit-frame-pointer -fno-stack-protector -fno-strict-aliasing 
CXXFLAGS += -fPIC -MMD $(INCLUDE) $(PREPROC)

ZCASRC         = api.cpp metadata.cpp get-image-offset.cpp zca_table.cpp \
		 dwarf-to-pin-reg.cpp
EXAMPLE1SRC    = spawn-depth.cpp

ZCAOBJ         = $(addprefix $(OUT)/,$(ZCASRC:.cpp=.o))
EXAMPLE1OBJ    = $(addprefix $(OUT)/,$(EXAMPLE1SRC:.cpp=.o))

LIBZCA	= libzca.a
EXAMPLE1TOOLS   = $(EXAMPLE1SRC:.cpp=.so) 

all: $(ZCAOBJ) $(BUILDDIR)/$(LIBZCA) $(BUILDDIR)/$(EXAMPLE1TOOLS)

$(BUILDDIR):
	mkdir -p $@

$(BUILDDIR)/%.o: %.cpp $(BUILDDIR)
	$(CXX) $(CXXFLAGS) -o $@ -c $< 

$(BUILDDIR)/$(LIBZCA): $(ZCAOBJ)
	ar rcs $@ $^

$(BUILDDIR)/$(EXAMPLE1TOOLS): $(ZCAOBJ) $(EXAMPLE1OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $(LDFLAGS) $^ $(LDLIBS) 

clean:
	rm -rf $(BUILDDIR)
