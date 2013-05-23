/*  zca.h                 -*- C++ -*-
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

#ifndef _ZCA_H_
#define _ZCA_H_

#include <pin.H>

// Zero Cost Annotations are stored in a section with the name .itt_not
#ifdef _WIN32
#define ITT_NOTIFY_SECTION_NAME ".itt_not"
#else
#define ITT_NOTIFY_SECTION_NAME ".itt_notify_tab"
#endif

namespace ZCA
{

    /* Handle to an object containing the collection of metadata annotations
       in a given module. */
    typedef int METADATA;

    /* Handle to an annotation within a section of metadata. */
    class ANN
    {
    private:
        void *opaque_0;
        void *opaque_1;
    public:
	ANN (void *ptr_0, void *ptr_1)
	    : opaque_0 (ptr_0), opaque_1 (ptr_1)
	    {}
	bool operator==(ANN& a)
	{
	    return (opaque_0 == a.opaque_0
		    && opaque_1 == a.opaque_1);
	}
	bool operator!=(ANN&a)
	{
	    return (opaque_0 != a.opaque_0
		    || opaque_1 != a.opaque_1);
	}
    };

    /* Scan the image for ZCA metadata and return a handle.  If no metadata
       was present, return METADATA_Invalid(). */
    METADATA GetMetadata (LEVEL_PINCLIENT::IMG img);

    /* Print info about the METADATA handle, and all the ZCA entries in it */
    int PrintMetadata (METADATA m);     
       
    /* Return an ANN handle for the first ZCA metadata annotation entry in
       the given collection. */
    ANN METADATA_HeadAnnotation (METADATA m);

    /* Return an ANN handle for the last ZCA metadata annotation entry in
       the given collection. */
    ANN METADATA_TailAnnotation (METADATA m);

    /* true if the given collection is valid, false otherwise. */
    bool METADATA_Valid (METADATA m);

    /* Return the special METADATA value that indicates that an IMG has no
       metadata. */
    METADATA METADATA_Invalid ();

    /* Print the DWARF expression in the ANN */
    void Print_ANN (ANN a);

    /* Return the ANN that follows a, or ANN_Invalid() if a is the last
       annotation in the collection. */
    ANN ANN_Next (ANN a);

    /* Return the ANN that precedes a, or ANN_Invalid() if a is the first
       annotation in the collection. */
    ANN ANN_Prev (ANN a);

    /* Return the annotation's name, or NULL if a is invalid. */
    const char *ANN_Name (ANN a);

    /* Return the address associated with the annotation, or zero if the
       annotation is invalid. */
    ADDRINT ANN_Address (ANN a);

    /* Return the annotation data expression.  This is a sequence of DWARF
       expression atoms prefixed with a ULEB128 value specifying length.
       Return NULL if a is invalid. */
    const unsigned char *ANN_DataExpression (ANN a);

    /* Decode the annotation and populate the reg and offset values that
       contain the data associated with the annotation.  Return true if the
       register offsets are valid, false if the annotation does not specify
       a register and offset. */
    bool ANN_RegisterAndOffset (ANN a,
                                LEVEL_PINCLIENT::REG *reg,
                                LEVEL_PINCLIENT::INT32 *offset);

    /* Return the number of bytes available for inserting a probe.  This is
       rounded up to the nearest full instruction.  If the return size is
       zero, there is no space for a probe. */
    LEVEL_PINCLIENT::USIZE ANN_ProbeSpaceBytes (ANN a);

    /* True if the annotation is valid, false otherwise. */
    bool ANN_Valid (ANN a);

    /* Return a special ANN value used to denote invalid annotations. */
    ANN ANN_Invalid ();
}

#endif /* !defined _ZCA_H_ */

