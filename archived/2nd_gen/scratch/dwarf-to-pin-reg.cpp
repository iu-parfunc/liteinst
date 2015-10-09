/* dwarf-to-pin-reg.cpp                  -*-C++-*-
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

#include <cstdio>
#include <pin.H>

/*
 * Include stdint.h to define the standard integer types.
 *
 * Unfortunately Microsoft doesn't provide stdint.h until Visual Studio 2010,
 * so use our own definitions until those are available
 */

#if ! defined(_MSC_VER) || (_MSC_VER >= 1600)
#include <stdint.h>
#else
#ifndef __MS_STDINT_TYPES_DEFINED__
#define __MS_STDINT_TYPES_DEFINED__
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef __int64 int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned __int64 uint64_t;
#endif  /* __MS_STDINT_TYPES_DEFINED__ */
#endif  /* ! defined(_MSC_VER) || (_MSC_VER >= 1600) */

#define LIBZCA_ASSERT(expr)                            \
    do {                                               \
      if (! (expr)) {                                  \
        std::fprintf(stderr, "warning: %s\n", #expr);  \
      }                                                \
    } while (0)

/*
 * reg_vals: array of possible registers for storing data for Pin to pick up.
 * high_register: the highest possible index into reg_vals.
 *
 * These two variables are specific to x86 vs. x86-64, but are consistent across
 * Linux and Windows.
 */

#if defined _WIN64 || defined __x86_64__
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
static int high_register = 15;
#else
static REG reg_vals[8] =
{
    REG_EAX,
    REG_ECX,
    REG_EDX,
    REG_EBX,
    REG_STACK_PTR,
    REG_EBP,
    REG_ESI,
    REG_EDI
};
static int high_register = 7;
#endif

/*
 * decode_LEB128
 *
 * Decode a signed Little Endian Base 128 number.  Based on the DWARF spec and
 * the Wikipedia article at http://en.wikipedia.org/wiki/LEB128
 *
 * Note that this routine assumes that all decoded values will fit in a signed
 * 32-bit word.
 */

static
int decode_LEB128(const uint8_t *p, int32_t *value, size_t *len)
{
    int32_t result = 0;
    uint32_t shift = 0;
    const uint8_t *start = p;
    uint32_t size = sizeof(result) * 8;
    uint8_t byte;

    while(1)
    {
        byte = *p++;
        result |= (byte & 0x7f) << shift;
        shift += 7;
        /* sign bit of byte is second high order bit (0x40) */
        if ((byte & 0x80) == 0)
            break;
    }

    if ((shift < size) && (byte & 0x40))
        /* sign extend */
        result |= - (1 << shift);

    *len = p - start;
    *value = (int32_t)result;

    return 0;
}

/*
 * decode_ULEB128
 *
 * Decode an unsigned Little Endian Base 128 number.  Based on the DWARF spec
 * and the Wikipedia article at http://en.wikipedia.org/wiki/LEB128 .
 *
 * Note that this routine assumes that all values will fit in an
 * unsigned 32-bit word.
 */

static
int decode_ULEB128(const uint8_t *p, uint32_t *value, size_t *len)
{
    uint32_t result = 0;
    uint32_t shift = 0;
    const uint8_t *start = p;
    uint8_t byte;

    while(1)
    {
        byte = *p++;
        result |= (byte & 0x7f) << shift;
        if (0 == (byte & 0x80))
        {
            *len = p - start;
            *value = result;
            return 0;
        }
        shift += 7;
    }
}

/*
 * dwarf_expr_to_pin
 *
 * Convert a DWARF expression from the ZCA annotation to a PIN register and
 * offset. Supported DWARF expressions:
 *
 *  DW_OP_lit* - Simple small integers.  reg is set to REG_NONE.
 *  DW_OP_reg* - Simple registers.  offset is set to 0.
 *  DW_OP_breg* - A register and an offset
 *
 * Returns:
 * 0 - success
 * 1 - failure
 */

int dwarf_expr_to_pin(const unsigned char *expression,
                      LEVEL_BASE::REG *reg,
                      INT32 *offset)
{
    // The format is a leading "ULEB128" value specifying length,
    // followed by a number of "Dwarf location atoms".  The expressions
    // I've seen have been (in hex):
    //  - "01 30" 1 byte, literal 0, or not specified,
    //  - "01 54" 1 byte, register 4.  For x86, that would be ESP
    //
    // Full DWARF specification: http://www.dwarfstd.org/doc/DWARF4.pdf
    // DWARF expr specification: http://dwarfstd.org/doc/040408.1.html
    // The complete enum dwarf_location_atom for the Dwarf codes can be found
    // in subversion at cilk/trunk/eng/prod/intel/gcc/gcc/dwarf2.h

    // The expression starts with a length as an unsigned LEB128 value
    size_t leb128_bytes;
    uint32_t expression_length;
    decode_ULEB128(expression, &expression_length, &leb128_bytes);
    expression += leb128_bytes;

    // Subset of dwarf opcodes known to occur in our ZCA entries
    enum dwarf_opcode
    {
        DW_OP_lit0  = 0x30,     // Literal values
        DW_OP_lit31 = 0x4f,
        DW_OP_reg0  = 0x50,     // Simple segisters
        DW_OP_reg31 = 0x6f,
        DW_OP_breg0 = 0x70,     // Registers + offset as a signed LEB128 value
        DW_OP_breg31 = 0x8f,
    };

    // Extract the opcode
    unsigned char opcode = *expression;

    // If this is a simple register, decode it
    if ((opcode >= DW_OP_reg0) && (opcode <= DW_OP_reg31))
    {
        // Length should have been 1 since the expression consists of a single
        // byte opcode
        LIBZCA_ASSERT(1 == expression_length);

        unsigned reg_number = opcode - DW_OP_reg0;
        if (reg_number <= high_register)
        {
            *reg = reg_vals[reg_number];
            *offset = 0;
            return 0;
        }
        else
        {
            *reg = REG_INVALID();
            *offset = 0;
            LIBZCA_ASSERT(! "Unexpected DWARF register code");
            return 1;
        }
    }

    // If this is a simple literal value, decode it
    if ((opcode >= DW_OP_lit0) && (opcode <= DW_OP_lit31))
    {
        // Length should have been 1 since the expression consists of a single
        // byte opcode
        LIBZCA_ASSERT(1 == expression_length);

        *reg = REG_NONE;
        *offset = opcode - DW_OP_lit0;
        return 0;
    }

    // If this is a register and offset, decode it
    if ((opcode >= DW_OP_breg0) && (opcode <= DW_OP_breg31))
    {
        // The opcode specifies the register
        unsigned reg_number = opcode - DW_OP_breg0;
        if (reg_number <= high_register)
        {
            *reg = reg_vals[reg_number];
        }
        else
        {
            *reg = REG_INVALID();
            *offset = 0;
            LIBZCA_ASSERT(! "Unexpected DWARF register code");
            return 1;
        }

        // Advance past the opcode
        expression++;
        expression_length--;

        // The offset is a signed LEB128 value
        decode_LEB128(expression, offset, &leb128_bytes);

        return 0;
    }

    LIBZCA_ASSERT(! "Unexpected DWARF register code");
    return 1;
}
