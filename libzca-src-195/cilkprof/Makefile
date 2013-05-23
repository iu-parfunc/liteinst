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

OUT ?= linux64
TOP = ..

PINDIR = $(TOP)/../3rdparty/pintool
PIN_TARGET_ARCH = intel64

BUILDDIR = $(OUT)

CXX = icpc
#CXX = g++

INCLUDE  = -I $(PINDIR)/source/include -I $(PINDIR)/source/include/gen
INCLUDE += -I $(PINDIR)/extras/xed2-$(PIN_TARGET_ARCH)/include
INCLUDE += -I $(PINDIR)/extras/components/include
INCLUDE += -I $(TOP)/zca/include

PREPROC = -DTARGET_IA32E -DHOST_IA32E -DTARGET_LINUX -DUSING_XED

CXXFLAGS = $(PREPROC) $(INCLUDE) -fPIC
CXXFLAGS += -ggdb

LDFLAGS  = -Bsymbolic -fPIC -Wl,--hash-style=sysv -shared
LDFLAGS += -L $(PINDIR)/$(PIN_TARGET_ARCH)/lib
LDFLAGS += -L $(PINDIR)/$(PIN_TARGET_ARCH)/lib-ext
LDFLAGS += -L $(PINDIR)/extras/xed2-$(PIN_TARGET_ARCH)/lib
LDFLAGS += -L $(TOP)/zca/src/$(BUILDDIR)
LDLIBS = -lpin -lxed -ldwarf -lelf -ldl -lpthread -lzca

SRC = cilkprof.cpp util.cpp
OBJ = $(addprefix $(BUILDDIR)/,$(SRC:.cpp=.o))
TARGET = $(BUILDDIR)/cilkprof.so

all:	$(TARGET)

$(TARGET): $(OBJ)
	$(CXX) -o $@ $(LDFLAGS) $^ $(LDLIBS)

$(BUILDDIR)/%.o: %.cpp $(BUILDDIR)
	$(CXX) -c -o $@ $(CXXFLAGS) $<

$(BUILDDIR):
	mkdir -p $@

clean:
	rm -rf $(BUILDDIR)
