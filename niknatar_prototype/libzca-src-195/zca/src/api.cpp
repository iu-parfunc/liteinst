/*  api.cpp                 -*- C++ -*-
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

#include <pin.H>
#include "api.h"
#include <cstring>
#include "get-image-offset.h"
#include "metadata.h"
#include <iostream>

namespace ZCA {

    /* Scan the image for ZCA metadata and return a handle.  If no metadata
       was present, return METADATA_Invalid(). */
    METADATA GetMetadata (LEVEL_PINCLIENT::IMG img)
    {
        METADATA handle = METADATA_Invalid();
        std::ptrdiff_t image_offset = sysdep_get_image_offset(img);
        std::size_t itt_notify_len = std::strlen(ITT_NOTIFY_SECTION_NAME);

        // Iterate through all of the sections in the image object.  There
        // may be more than one metadata section in an image.  This is
        // abstracted away for the users so that the metadata for an image
        // is presented as a single, cohesive entity.
        for (SEC sec = IMG_SecHead(img); SEC_Valid(sec); sec = SEC_Next(sec)) {
            if (0 == std::strncmp(SEC_Name(sec).c_str(),
                                  ITT_NOTIFY_SECTION_NAME,
                                  itt_notify_len)) {
                // This is a metadata section.
                if (METADATA_Invalid() == handle) {
                    handle = create_metadata_handle(LEVEL_PINCLIENT::IMG_LowAddress(img),
                                                    LEVEL_PINCLIENT::IMG_HighAddress(img));
                }
                //std::cerr<<"Metadata section found at absolute addr: "<<(void*)SEC_Address(sec)<<std::endl;
                if (! add_metadata(handle,
                                   image_offset,
                                   (void*)SEC_Address(sec),
                                   SEC_Size(sec)))
                {
                    std::cerr << "Failed to add metadata for " <<
                        IMG_Name(img) <<
                        std::endl;
                }
            }
        }
        return handle;
    }

    int PrintMetadata (METADATA m)
    {
        if (false == METADATA_Valid(m))
        {
            return -1;
        }

        for (ANN a=METADATA_HeadAnnotation(m);
             ANN_Invalid()!=a;
             a=ANN_Next(a))
            Print_ANN(a);
    }

    /* Return an ANN handle for the first ZCA metadata annotation entry in
       the given collection. */
    ANN METADATA_HeadAnnotation (METADATA m)
    {
        return head_annotation(m);
    }

    /* Return an ANN handle for the last ZCA metadata annotation entry in
       the given collection. */
    ANN METADATA_TailAnnotation (METADATA m)
    {
        return tail_annotation(m);
    }

    /* true if the given collection is valid, false otherwise. */
    bool METADATA_Valid (METADATA m)
    {
        if (METADATA_Invalid() == m) return false;
        return verify_metadata(m);
    }

    /* Return the special METADATA value that indicates that an IMG has no
       metadata. */
    METADATA METADATA_Invalid ()
    {
        return invalid_metadata_handle();
    }

    /* Print the DWARF expression in the ANN */
    void Print_ANN (ANN a)
    {
        print_annotation(a);
    }

    /* Return the ANN that follows a, or ANN_Invalid() if a is the last
       annotation in the collection. */
    ANN ANN_Next (ANN a)
    {
        return next_annotation(a);
    }

    /* Return the ANN that precedes a, or ANN_Invalid() if a is the first
       annotation in the collection. */
    ANN ANN_Prev (ANN a)
    {
        return prev_annotation(a);
    }

    /* Return the annotation's name, or NULL if a is invalid. */
    const char *ANN_Name (ANN a)
    {
        return annotation_name(a);
    }

    /* Return the address associated with the annotation, or zero if the
       annotation is invalid. */
    ADDRINT ANN_Address (ANN a)
    {
        return annotation_address(a);
    }

    /* Return the annotation data expression.  This is a sequence of DWARF
       expression atoms prefixed with a ULEB128 value specifying length.
       Return NULL if a is invalid. */
    const unsigned char *ANN_DataExpression (ANN a)
    {
        return annotation_data_expression(a);
    }

    /* Decode the annotation and populate the reg and offset values that
       contain the data associated with the annotation.  Return true if the
       register offsets are valid, false if the annotation does not specify
       a register and offset. */
    bool ANN_RegisterAndOffset (ANN a,
                                LEVEL_PINCLIENT::REG *reg,
                                LEVEL_PINCLIENT::INT32 *offset)
    {
        return annotation_register_and_offset(a, reg, offset);
    }

    /* Return the number of bytes available for inserting a probe.  This is
       rounded up to the nearest full instruction.  If the return size is
       zero, there is no space for a probe. */
    LEVEL_PINCLIENT::USIZE ANN_ProbeSpaceBytes (ANN a)
    {
        return annotation_get_probespace_bytes(a);
    }

    /* True if the annotation is valid, false otherwise. */
    bool ANN_Valid (ANN a)
    {
        return (ANN_Invalid() == a) ;
    }

    /* Return a special ANN value used to denote invalid annotations. */
    ANN ANN_Invalid ()
    {
        return invalid_annotation_handle();
    }

}
