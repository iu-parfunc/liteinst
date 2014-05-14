/*  zca_table.cpp            -*- C++ -*-
 *
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

#include <assert.h>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include "metadata.h"
#include "zca_table.h"
#include "api.h"

#ifdef _WIN32
#define SNPRINTF _snprintf
#else
#define SNPRINTF std::snprintf
#endif

/*
 * Get an INS (instruction) from the corresponding ADDRINT (address in
 * integer form).  There is no builtin for this in Pin.
 */
static INS search_rtn_for_address (RTN rtn, ADDRINT addr)
{
    for (INS ins = LEVEL_PINCLIENT::RTN_InsHead(rtn);
         LEVEL_PINCLIENT::INS_Valid(ins);
         ins = LEVEL_PINCLIENT::INS_Next(ins))
    {
        if (addr == LEVEL_PINCLIENT::INS_Address(ins))
            return ins;
    }

    return INS_Invalid();
}

/*
 * Default constructor.  Creates an empty instance of the Metadata class.
 */
ZCA::zca_table::zca_table ()
{
}

/*
 * Initialize the zca_table by enrolling callbacks on load/unload of
 * modules.  This should be called after PIN_Init(), and before any
 * other methods are invoked.
 */
void ZCA::zca_table::initialize ()
{
    IMG_AddInstrumentFunction(load, this);
    IMG_AddUnloadFunction(unload, this);
}

/*
 * calculate_data_address
 *
 * Helper function to take the value of a register and an offset, add them
 * together, and return them to a PIN scratch register
 */

static ADDRINT PIN_FAST_ANALYSIS_CALL calculate_data_address(ADDRINT base_address, ADDRINT offset)
{
    return base_address + offset;
}

static REG call_calculate_data_address_if_necessary(INS ins,
                                                    REG reg, INT32 offset)
{
    // Per-thread "scratch" register used to combine a register and offset
    static REG computed_address_reg = REG_INVALID();

    // If there's no offset from the register, just use the register directly
    if (0 == offset)
        return reg;

    // The DWARF expression includes a register and offset
    //
    // If we haven't done it already, allocate the scratch register we'll use
    // to compute a data address
    if (! REG_valid(computed_address_reg))
        computed_address_reg = PIN_ClaimToolRegister();

    // If we couldn't get a scratch register, we're doomed
    if (! REG_valid(computed_address_reg))
    {
        fprintf(stderr, "Fatal Error - Cannot allocate a scratch register.");
        abort();
    }

    // Add a callback for our helper routine to calculate the data address
    // from the register and offset, and store it in the PIN "scratch"
    // register we reserved
    INS_InsertCall(ins, IPOINT_BEFORE, AFUNPTR(calculate_data_address),
                   IARG_FAST_ANALYSIS_CALL,
                   IARG_REG_VALUE, reg,
                   IARG_UINT32, (UINT32)offset,
                   IARG_RETURN_REGS, computed_address_reg,
                   IARG_CALL_ORDER, CALL_ORDER_FIRST,
                   IARG_END);

    // The caller should use the scratch register which will hold the computed
    // address
    return computed_address_reg;
}

/*
 * insert_call
 *
 * Add a callback.
 *
 * Note that Tevi said it would be more efficient for us to use an
 * instruction callback instead of requesting callbacks in advance.
 * Consider fixing this in the future
 */
void ZCA::zca_table::insert_call (ZCA::ANN ann,
                                  ZCA::zca_table::callback &cb)
{
    INS ins = INS_Invalid();

    // Find the nearest routine and scan though it's instructions for the
    // one we're looking for
    ADDRINT addr = ANN_Address(ann);

    fprintf(stderr, "insert_call for %s at %p\n", ANN_Name(ann), addr);

    RTN rtn = RTN_FindByAddress(addr);
    if (RTN_Valid(rtn))
    {
        RTN_Open(rtn);
        ins = search_rtn_for_address(rtn, addr);
        if (! INS_Valid(ins))
        {
#define LABEL_BUF_SIZE 256
            char buf[LABEL_BUF_SIZE];
            static unsigned int label_counter = 0;

            // PIN didn't find the instruction in the routine.  This can
            // happen if PIN decides that the routine really wasn't as long
            // as it thought it was afterall.
            //
            // Create a "routine" at the address and open that one.  We'll
            // trust that the compiler didn't give us an address in the middle
            // of nowhere.  First close the old routine since it's no longer
            // interesting.
            RTN_Close(rtn);

            // Generate a unique label for the address and create a
            // "routine" at it
            SNPRINTF(buf, LABEL_BUF_SIZE,
                          "__cilk_metadata_annotation_%u",
                          ++label_counter);
            rtn = RTN_CreateAt(addr, buf);
            RTN_Open(rtn);
            ins = RTN_InsHead(rtn);
        }
    }

    // We'd better have a valid routine and instruction now
    assert(RTN_Valid(rtn));
    assert(INS_Valid(ins));

    // If the annotation has data associated with it, the first argument
    // to that function is the raw data expression.
    const unsigned char *data = ANN_DataExpression(ann);
    LEVEL_PINCLIENT::REG reg;
    LEVEL_PINCLIENT::INT32 offset;
    if (ANN_RegisterAndOffset(ann, &reg, &offset))
    {
        // If the DWARF location expression gave us a register and offset,
        // register a callback to calculate the data address and store the
        // address in a scratch register
        reg = call_calculate_data_address_if_necessary(ins, reg, offset);

        // The metadata has a parameter. The first argument to be provided
        // to the callback is the data
        INS_InsertCall(ins, IPOINT_BEFORE, cb.afun,
                       IARG_REG_VALUE, reg,
                       IARG_IARGLIST, cb.args, IARG_END);
    }
    else
    {
        // No special parameters to this metadata.
        INS_InsertCall(ins, IPOINT_BEFORE, cb.afun,
                       IARG_IARGLIST, cb.args, IARG_END);
    }

    // Close the open routine now that we've finished with the instruction
    RTN_Close(rtn);
}

/*
 * Callback for when Pin loads an image.  If there is metadata in the
 * image, and if the user has requested callbacks on any of it, register
 * those callbacks and track the image for when it gets unloaded.
 */
void ZCA::zca_table::load (LEVEL_PINCLIENT::IMG img, void *arg)
{
    ZCA::zca_table* table = (ZCA::zca_table*)arg;

    ZCA::METADATA m = GetMetadata(img);
    if (ZCA::METADATA_Invalid() == m)
    {
        return;
    }
    fprintf(stderr, "\n\n\nFound metadata in %s:\n", IMG_Name(img).c_str());
    PrintMetadata(m);

    // Track this image for when the time comes to unload all of the
    // metadata.
    ADDRINT low = LEVEL_PINCLIENT::IMG_LowAddress(img);
    ADDRINT high = LEVEL_PINCLIENT::IMG_HighAddress(img);
    ZCA::zca_table::img_info entry(m, low, high);
    table->md_handles.push_back(entry);

    fprintf(stderr, "Found metadata in %s, [%p - %p]\n",
            IMG_Name(img).c_str(), low, high);

    // For each annotation in this metadata section, check to see whether
    // the user wants to register a callback.  If so, perform the
    // registration.
    for (ZCA::ANN ann = ZCA::METADATA_HeadAnnotation(m);
         ZCA::ANN_Invalid() != ann;
         ann = ZCA::ANN_Next(ann)) {
        for (std::vector<ZCA::zca_table::callback>::iterator iter
                 = table->annotations.begin();
             iter != table->annotations.end();
             iter++) {
            if (ANN_Name(ann) == iter->name) {
                table->insert_call(ann, *iter);
                break;
            }
        }
    }
}

/*
 * Callback for when Pin unloads an image.  If the image had any metadata,
 * eliminate our pointers into it.
 */
void ZCA::zca_table::unload (LEVEL_PINCLIENT::IMG img, void *arg)
{
    ZCA::zca_table* table = (ZCA::zca_table*)arg;
    ADDRINT low_addr = LEVEL_PINCLIENT::IMG_LowAddress(img);
    ADDRINT high_addr = LEVEL_PINCLIENT::IMG_HighAddress(img);
    for (std::vector<ZCA::zca_table::img_info>::iterator iter =
             table->md_handles.begin();
         iter != table->md_handles.end();
         ++iter) {
        if ((iter)->low_address == low_addr
             && (iter)->high_address == high_addr)
        {
            table->md_handles.erase(iter);
            return;
        }
    }
}

int ZCA::find_followup_type (IARG_TYPE type)
{
    switch (type)
    {
    case IARG_ADDRINT:
        return FOLLOW_ADDRINT;
    case IARG_PTR:
        return FOLLOW_PTR;
    case IARG_BOOL:
        return FOLLOW_BOOL;
    case IARG_UINT32:
        return FOLLOW_UINT32;
    case IARG_REG_VALUE:
    case IARG_RETURN_REGS:
        return FOLLOW_REG;
    case IARG_FILE_NAME:
        return FOLLOW_CHAR_PTR;
    case IARG_LINE_NO:
        return FOLLOW_LINE_NO;
    case IARG_LAST:
        return FOLLOW_LAST;
    default:
        return FOLLOW_NULL;
    }
}

/*
 * Request that analysis calls be added for the addresses for annotations
 * with a matching name.  You can specify additional parameters for your
 * analysis routine as described for INS_InsertCall. The parameters must
 * end with LEVEL_PINCLIENT::IARG_END .  The call will be inserted before
 * the instruction at the annotation address. If the data value for the
 * annotation is not a constant 0, the analysis function afun will be
 * called with the data value as the first parameter, and any specified
 * parameters as the remaining parameters. Note that analysis calls will
 * only be added for any future JITed code.  No attempt is made to insert
 * analysis calls for code that PIN has already considered for instrumentation.
 *
 * Parameters:
 * name – The name of the annotation that callbacks are to be inserted.
 * afun – The analysis function to be called when the address associated
 * with the annotation is reached.
 */
void ZCA::zca_table::insert_annotation_calls (const char *name,
                                              LEVEL_PINCLIENT::AFUNPTR afun,
                                              ...)
{
    IARGLIST arglist = IARGLIST_Alloc();
    va_list list;
    va_start(list, afun);

    int followup_type;
    do {
        IARG_TYPE arg_type = (IARG_TYPE)va_arg(list, int);

        followup_type = find_followup_type(arg_type);
        if ((followup_type == FOLLOW_NULL) ||
            (followup_type == FOLLOW_LAST))
        {
            // Either this is a simple (stand-alone) argument, or it's
            // the IARG_LAST, which is invalid...
            IARGLIST_AddArguments(arglist,
                                  arg_type,
                                  IARG_END);
        }
        else
        {
            switch (followup_type)
            {
                case FOLLOW_ADDRINT:
                {
                    ADDRINT addr = (ADDRINT)va_arg(list, void*);
                    IARGLIST_AddArguments(arglist,
                                          arg_type, addr,
                                          IARG_END);
                }
                break;

                case FOLLOW_PTR:
                {
                    void *ptr = reinterpret_cast<void*>(va_arg(list, char*));
                    IARGLIST_AddArguments(arglist,
                                          arg_type, ptr,
                                          IARG_END);
                }
                break;

                case FOLLOW_BOOL:
                {
                    bool boolval = (bool)va_arg(list, int);
                    IARGLIST_AddArguments(arglist,
                                          arg_type, boolval,
                                          IARG_END);
                }
                break;

                case FOLLOW_UINT32:
                {
                    UINT32 unsigned_32_int = va_arg(list, int);
                    IARGLIST_AddArguments(arglist,
                                          arg_type, unsigned_32_int,
                                          IARG_END);
                }
                break;

                case FOLLOW_REG:
                {
                    int reg = va_arg(list, int);
                    IARGLIST_AddArguments(arglist,
                                          arg_type, reg,
                                          IARG_END);
                }
                break;

                case FOLLOW_CHAR_PTR:
                {
                    char *char_ptr = va_arg(list, char*);
                }
                break;

                case FOLLOW_LINE_NO:
                {
                    int line_no = va_arg(list, int);
                }
                break;

                default:
                    assert(!"Bad value for followup_type.");
                    break;
            }

        }
    } while (followup_type != FOLLOW_LAST);

    va_end(list);

    annotations.push_back(callback(name, afun, arglist));
}
