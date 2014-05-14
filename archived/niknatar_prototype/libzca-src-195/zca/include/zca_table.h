/*  zca_table.h              -*- C++ -*-
 *
 *  Copyright (C) 2012 Intel Corporation
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef _ZCA_UTIL_H_
#define _ZCA_UTIL_H_

#include "zca.h"
#include <cstdio>
#include <iterator>
#include <vector>

/* The High-Level API for ZCA Metadata provides a convenient set of classes
   to collect the ZCA metadata annotations and request calls to analysis
   functions.  All classes and values will be in the zca:: namespace.

   Issues:
   - We’ll include the dumper code as an example at the end of the document.

   - While I’ve gotten the code from Greg to combine the offset and register
   using PIN magic, I haven’t tried it. */
namespace ZCA
{
    /* Check whether the IARG_TYPE requires a follow up argument when
       passed to a function and the type of the arg */
    #define FOLLOW_NULL     0
    #define FOLLOW_ADDRINT  1
    #define FOLLOW_PTR      2
    #define FOLLOW_BOOL     3
    #define FOLLOW_UINT32   4
    #define FOLLOW_REG      5
    #define FOLLOW_CHAR_PTR 6
    #define FOLLOW_LINE_NO  7
    #define FOLLOW_LAST     8
    int find_followup_type (IARG_TYPE type);

    /* The zca_table class collects ZCA Metadata annotations from loaded
       modules. */
    class zca_table
    {
    private:
        /* Will use this later, now iterate through METADATA and insert
           callbacks */
        //typedef std::multiset<zca_annotation,
        //                      zca_annotation::less_by_ip> zca_map_t;
        //zca_map_t d_map;

        struct img_info
        {
            METADATA  md;
            ADDRINT   low_address;
            ADDRINT   high_address;
            img_info (METADATA md_, ADDRINT low_address_, ADDRINT high_address_)
                : md(md_), low_address(low_address_),
                  high_address(high_address_)
            {}
        };

        struct callback
        {
            string                   name;   // Name of annotation
            LEVEL_PINCLIENT::AFUNPTR afun;   // Callback function
            IARGLIST                 args;   // arg list ending with IARG_END
            callback (string name_, LEVEL_PINCLIENT::AFUNPTR afun_,
                      IARGLIST args_)
                : name(name_), afun(afun_), args(args_)
                {
                }
        };

        /* Valid Metadata handles */
        std::vector<img_info> md_handles;
        std::vector<callback> annotations;

        void insert_call (ANN ann, callback &cb);
        static void load (LEVEL_PINCLIENT::IMG img, void *arg);
        static void unload (LEVEL_PINCLIENT::IMG img, void *arg);

    public:

        /*  Default constructor.  Creates an empty instance of the Metadata
            class. */
        zca_table();

        /*
         * Initialize the zca_table by enrolling callbacks on load/unload of
         * modules.  This should be called after PIN_Init(), and before any
         * other methods are invoked.
         */
        void initialize ();

        /* Request that analysis calls be added for the addresses for
           annotations with a matching name.  You can specify additional
           parameters for your analysis routine as described for INS_InsertCall.
           The parameters must end with LEVEL_PINCLIENT::IARG_END .  The call
           will be inserted before the instruction at the annotation address.
           If the data value for the annotation is not a constant 0, the
           analysis function afun will be called with the data value as the
           first parameter, and any specified parameters as the remaining
           parameters. Note that analysis calls will only be added for any
           future JITed code.  No attempt is made to insert analysis calls for
           code that PIN has already considered for instrumentation.

           Parameters:
           name – The name of the annotation that callbacks are to be inserted.
           afun – The analysis function to be called when the address associated
           with the annotation is reached.*/
        void insert_annotation_calls(const char *name,
                                     LEVEL_PINCLIENT::AFUNPTR afun, ...);
    };

}

#endif /* !defined _ZCA_UTIL_H_ */
