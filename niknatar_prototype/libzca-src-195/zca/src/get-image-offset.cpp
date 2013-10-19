/*
 * get-image-offset.cpp
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
 **************************************************************************
 *
 * This module provides sysdep_get_image_offset, an OS-dependent function to
 * calculate the offset between a module's preferred load address and the
 * address it's been loaded at.  This offset will be passed to zca_table::parse
 * to allow it to rebase the addresses in a module's metadata
 *
 * sysdep_get_image_offset is in it's own file because it's shared by both
 * Cilkscreen and Cilkview.
 *
 **************************************************************************/

#include <pin.H>
#include "get-image-offset.h"

#ifdef _WIN32

// Bring in windows.h and put it in it's own namespace so it won't clash with
// pin.H
namespace WINDOWS
{
#include <windows.h>
}

// MakePtr is a macro that allows you to easily add to values (including
// pointers) together without dealing with C's pointer arithmetic.  It
// essentially treats the last two parameters as DWORDs.  The first
// parameter is used to typecast the result to the appropriate pointer type.
#define MakePtr( cast, ptr, addValue ) (cast)( (WINDOWS::DWORD_PTR)(ptr) + (WINDOWS::DWORD_PTR)(addValue))

/*
 * sysdep_get_image_offset - Windows version
 *
 * Calculate the offset between the image's preferred load address and the
 * address that the image is actually loaded at.  This will be added to the
 * code addresses in the metadata to normalize them.
 */

std::ptrdiff_t sysdep_get_image_offset(IMG img)
{
    WINDOWS::IMAGE_DOS_HEADER *pDosHeader;
    WINDOWS::IMAGE_NT_HEADERS *pNTHeader;
    WINDOWS::IMAGE_OPTIONAL_HEADER *pOptionalHeader;

    char *base_address = (char *)IMG_LowAddress(img);

    // Fetch the handle for the image.
    pDosHeader = (WINDOWS::IMAGE_DOS_HEADER *)base_address;
    if (IMAGE_DOS_SIGNATURE != pDosHeader->e_magic)
        return 0;

    pNTHeader = MakePtr (WINDOWS::IMAGE_NT_HEADERS *, pDosHeader, pDosHeader->e_lfanew);
    if (IMAGE_NT_SIGNATURE != pNTHeader->Signature)
        return 0;

    pOptionalHeader = &pNTHeader->OptionalHeader;

//    trace ("Preferred load address: %p, current load address: %p, offset: %p\n",
//           (char *)pOptionalHeader->ImageBase, base_address,
//           base_address - (char *)pOptionalHeader->ImageBase);

    return base_address - (char *)pOptionalHeader->ImageBase;
}
#else

/*
 * sysdep_get_image_offset - Linux version
 *
 * PIN provides IMG_LoadOffset for Linux, which should be exactly what we want.
 */

std::ptrdiff_t sysdep_get_image_offset(IMG img)
{
    // Will - need to test this!
//    return (std::ptrdiff_t)IMG_LoadOffset(x);

    return 0;
}
#endif
