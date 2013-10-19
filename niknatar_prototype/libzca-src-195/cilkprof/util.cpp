/*
  Copyright (C) 2012 Intel Corporation
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.
  * Neither the name of Intel Corporation nor the names of its
    contributors may be used to endorse or promote products derived
    from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
 */

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <strings.h>
#include "util.h"

static std::string executable_file_name;

/*
 * Print out an error message and exit.
 */
void die (const char *fmt, ...)
{
    std::va_list l;
    std::fprintf(stderr, "cilkprof: fatal error\n");
    va_start(l, fmt);
    std::vfprintf(stderr, fmt, l);
    fflush(stderr);
    va_end(l);
    std::exit(1);
}

void save_executable_file_name (const char *name)
{
    executable_file_name = name;
}

/*
 * Return a file pointer to the specified output file.
 */
static
FILE *get_outfile (const char *envstr_name, const char *default_suffix)
{
    std::string outfile_name;

    // See if the user defined the environment variable. If so, it specifies
    // the file to be created
    const char *filename = std::getenv(envstr_name);

    // If the user didn't specify the environment variable, build a name
    // using the main executable
    if (NULL == filename)
    {
        outfile_name = executable_file_name;
        outfile_name += default_suffix;
        filename = outfile_name.c_str();
    }

    // Open the output file
    FILE *fp = std::fopen(filename, "w");
    if (0 == fp)
    {
        std::string msg;
        msg =  "cilkprof error: unable to open '";
        msg += filename;
        msg += "'";
        std::perror(msg.c_str());

        fp = stdout;
    }

    return fp;
}

/* Return the output files specified by the environment. */
FILE *get_bb_outfile ()
{
    return get_outfile("CILKPROF_BB_OUT", ".bb.csv");
}

FILE *get_cc_outfile ()
{
    return get_outfile("CILKPROF_CC_OUT", ".cc.csv");
}
