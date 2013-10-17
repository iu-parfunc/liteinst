/*
 * Copyright (C) 2012 Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *   * Neither the name of Intel Corporation nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 * WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <pin.H>
#include "zca_table.h"
#include <iostream>

INT32 Usage()
{
    std::cerr << "This tool produces a trace of calls to RtlAllocateHeap.";
    std::cerr << endl << endl;
    std::cerr << KNOB_BASE::StringKnobSummary();
    std::cerr << endl;
    return -1;
}

void print_text (char* text)
{
    std::cout<<text<<std::endl;
}

static int cur_depth = 0;
static int max_depth = 0;

void increase_spawn_depth ()
{
    if (++cur_depth > max_depth)
        max_depth = cur_depth;
}

void decrease_spawn_depth ()
{
    --cur_depth;
}

VOID Fini(INT32 code, VOID *v)
{
    std::cout<<"Maximum spawn depth: "<<max_depth<<std::endl;
}

ZCA::zca_table table;

int main (int argv, char**argc)
{
    PIN_InitSymbols();
    if( PIN_Init(argv,argc) )
    {
        return Usage();
    }
    
    /* Write to a file since cout and cerr maybe closed by the application */
    //TraceFile.open(KnobOutputFile.Value().c_str());
    //TraceFile << hex;
    //TraceFile.setf(ios::showbase);

    table.initialize();
    table.insert_annotation_calls ("cilk_spawn_prepare",
                                   (LEVEL_PINCLIENT::AFUNPTR)increase_spawn_depth,
                                   IARG_END);

    table.insert_annotation_calls ("cilk_sync_begin",
                                   (LEVEL_PINCLIENT::AFUNPTR)decrease_spawn_depth,
                                   IARG_END);

    PIN_AddFiniFunction (Fini, 0);
    // Never returns
    PIN_StartProgram();
    
    return 0;
}
