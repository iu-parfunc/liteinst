
/*                           -*- C language -*-                    */

/** 
 * 
 * @brief Typedefs modeling the structure of the ZCA ELF headers.
 * 
 * @details
 * This code was taken from Intel's released "libzca", specifically libzca-src-195:
 *
 * http://software.intel.com/en-us/articles/download-intel-cilk-plus-software-development-kit
 *
 * Therefore, the license for this code is:
 *
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

#ifndef _ZCA_TYPES_H_
#define _ZCA_TYPES_H_
#ifdef __cplusplus
extern "C"
{
#endif

#include <inttypes.h>

// --------------------------------------------------------------------------------

// using namespace std;

/*
static REG reg_vals[16] =
{
    REG_RAX,
    REG_RDX,
    REG_RCX,
    REG_RBX,
    REG_RSI,
    REG_RDI,
    REG_RBP,
    REG_STACK_PTR,
    REG_R8,
    REG_R9,
    REG_R10,
    REG_R11,
    REG_R12,
    REG_R13,
    REG_R14,
    REG_R15
};
*/

/* ZCA table stuff */
typedef unsigned char byte;

#define STUB_SIZE 72 // In practice this is 66 bytes but making 72 for alignment
#define PROBE_SIZE 6 // Size of a probespace within a probe site

#define CHUNK_SIZE 4294967296 // 2^32
/*
 * Header for a table of V1.1 annotations.
 *
 * Structures derived from Low Cost ITT Notify document and the data
 * actually generated by the compiler.
 */
struct zca_header_11_t
{
  // static const std::size_t magic_sz = 16;
    #define magic_sz 16

    uint8_t     magic[magic_sz];// Magic value - ".itt_notify_tab"
    uint16_t    version;        // Major/Minor version number
    uint16_t    entry_count;    // Count of entries that follow
    uint32_t    strings;        // Offset in bytes to string table
    uint32_t    strings_len;    // Size of string table (bytes)
    uint32_t    exprs;          // Offset in bytes to expression table
    uint32_t    exprs_len;      // Size of expression table (bytes)
};

const uint16_t ZCA_INVALID_VERSION=0xffff;
const uint16_t ZCA_1_1_VERSION=0x0101;

// Zero-padded magic value in an itt_notify annotation group
// const char itt_notify_magic[zca_header_11_t::magic_sz] = ".itt_notify_tab";
const char itt_notify_magic[magic_sz] = ".itt_notify_tab";

/*
 * Annotation within a V1.1 table of metadata
 */
#if defined(_MSC_VER)
#pragma pack(push, 4)
#endif
struct zca_row_11_t
{
    uint64_t    anchor;     // Instruction pointer of entry
    uint32_t    probespace; // Bytes of instruction to be copied by probe
    uint32_t    annotation; // Offset of annotation string in strings table
    uint32_t    expr;       // How to compute tag parameter in intrinsic
}
#if defined(_MSC_VER)
;
#pragma pack(pop)
#else
__attribute__((packed));
#endif

/*
 * Header for a table of V1.2 annotations.
 *
 * Structures derived from Low Cost ITT Notify document and the data
 * actually generated by the compiler.
 */
struct zca_header_12_t
{
  // static const std::size_t magic_sz = 16;
  // size_t      magic_sz = 16;
    
    uint8_t     magic[magic_sz];// Magic value - ".itt_notify_tab"
    uint16_t    version;        // Major/Minor version number
    uint16_t    header_size;    // Size of this structure in bytes
    uint32_t    entry_count;    // Count of entries that follow
    uint32_t    strings;        // Offset in bytes to string table
    uint32_t    strings_len;    // Size of string table (bytes)
    uint32_t    exprs;          // Offset in bytes to expression table
    uint32_t    exprs_len;      // Size of expression table (bytes)
    uint64_t    flags;          // Flags
};

const uint16_t ZCA_1_2_VERSION=0x0102;

const uint64_t zca_c_anchors_pcrel = 0x01;      // Anchor address is PC-
                                                // relative.  If not present,
                                                // anchor address is absolute

const uint64_t zca_c_have_probe_region = 0x02;  // ZCA table row has field
                                                // specifying bytes that can
                                                // be used for a probe

const uint64_t zca_c_32bit_anchor=0x04;         // The anchor address is 32bits

// Data structure to hold additional data related to an annotation and the stub related to it
typedef struct ann_data
{
  unsigned long* stubLocation;                  // Address of the stub related to this probe
  short int probe_offset;                       // Offset at which original probe sequence can be found at the stub site
  const char* expr;                             // Decoded annotation tag which serve as a look up key
  bool active; 									// Whether this probe site is currently active or not
  void (*fun) ();
  uint64_t anchor;     // Instruction pointer of entry
  uint32_t probespace; // Bytes of instruction to be copied by probe
  uint32_t annotation; // Offset of annotation string in strings table
  uint32_t expr_dwarf;       // How to compute tag parameter in intrinsic
/*  ann_data(unsigned long* sL, int sS, void (*f)(), const byte* const i, const uint32_t ps,
   	   const byte* const e):
    stubLocation(sL), stubSize(sS), fun(f), ip(i), probespace(ps), expr(e) {}*/
} ann_data;

typedef struct mem_island {
	unsigned long* start_addr;    // Memory island start address
	int32_t mem_chunk;            // 2^32 memory chunk this island belongs to
  //	bool allocated; 			      // Has this island been actually allocated
	uint8_t* insertion_ptr;       // Next stub insertion pointer
	uint64_t size;                // Size required for this memory island in bytes
	uint64_t unallocated_size;    // The size that was left unallocated due to memory constraints.
	                              // (e.g: due to fragmentation of vm address space)
	                              // Should be allocated to another mem_island at a different location a lazy manner
	uint64_t remaining_size;      // Remaining size available
	uint8_t* last_probe_address;  // Address of the last probe serviced by this stub island.
} mem_island;

typedef struct exprReg {
  int reg;
  int32_t offset;
} exprReg;
  
// --------------------------------------------------------------------------------
// Convenience functions for dealing with rows

inline uint32_t getProbespace(struct zca_row_11_t* row)
{
  return (uint32_t) row->probespace;
}

inline const byte* getIP(zca_row_11_t* row)
{
  return
     (const byte*) row->anchor;
}

inline const byte* getExpr(struct zca_header_11_t* table, struct zca_row_11_t* row)
{
  return (const byte*) ((byte*) table + table->exprs + row->expr);
}

inline const char* getAnnotation(struct zca_header_11_t* table, struct zca_row_11_t *row)
{
  return (const char*) ((byte*) table + table->strings + row->annotation);
}

#ifdef __cplusplus
}
#endif
#endif  // _ZCA_TYPES_H_
