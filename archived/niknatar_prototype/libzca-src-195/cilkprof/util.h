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

#ifndef _UTIL_H_
#define _UTIL_H_ 1

/* FIXME: It's silly that _I_ have to define this.  Send a bug report to
   the Pin group. */
#define UINT32_MAX 0xFFFFFFFF

/* Assert macro. */
#define CILKPROF_ASSERTIONS 1
#ifdef CILKPROF_ASSERTIONS
#define cilkprof_assert(c) \
    do { if (!(c)) { die("%s:%d assertion failure: %s\n",       \
                         __FILE__, __LINE__, #c);} } while (0)
#else
#define cilkprof_assert(c)
#endif

/* Print out an error message and exit. */
void die (const char *fmt, ...);

/* Return the output files specified by the environment. */
FILE *get_bb_outfile ();
FILE *get_cc_outfile ();

/* Save the main executable to set defaults for bb & cc files */
void save_executable_file_name (const char *name);

#endif // ! defined _UTIL_H_
