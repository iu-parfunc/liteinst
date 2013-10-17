/*  metadata.h                 -*- C++ -*-
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

#ifndef _METADATA_H_
#define _METADATA_H_

#include <zca.h>

/* Return a new handle for storing metadata metadata.
   Low and High addr are of the image to which this handle
   corresponds to. */
ZCA::METADATA create_metadata_handle (ADDRINT low_addr, ADDRINT high_addr);

/* Add the specified metadata section to the metadata pointed to by handle. */
bool add_metadata (ZCA::METADATA m,
                   std::ptrdiff_t img_offset,
                   void *addr,
                   std::size_t size);

/* Invalidate a metadata handle */
void delete_metadata (IMG img, void *metadata_handle_ptr);

/* Return a handle to the first annotation in the given section. */
ZCA::ANN head_annotation (ZCA::METADATA m);

/* Return a handle to the last annotation in the given section. */
ZCA::ANN tail_annotation (ZCA::METADATA m);

/* Verify the integrity of the metadata pointed to by the handle. */
bool verify_metadata (ZCA::METADATA m);

/* Return the "invalid" handle for ZCA::METADATA. */
ZCA::METADATA invalid_metadata_handle ();

/* Print the DWARF expression in the annotation */
void print_annotation (ZCA::ANN& a);

/* Return a handle to a's next annotation. */
ZCA::ANN next_annotation (ZCA::ANN& a);

/* Return a handle to a's previous annotation. */
ZCA::ANN prev_annotation (ZCA::ANN& a);

/* Return the name of the given annotation. */
const char *annotation_name (ZCA::ANN& a);

/* Return the annotation address */
ADDRINT annotation_address (ZCA::ANN& a);

/* Return a pointer to the annotation's data expression. */
const unsigned char *annotation_data_expression (ZCA::ANN& a);

/* Return the register and offset corresponding to the data in the annotation,
   if they are valid, and return true.  If they are invalid, return false. */
bool annotation_register_and_offset (ZCA::ANN &a,
                                     LEVEL_PINCLIENT::REG *reg,
                                     LEVEL_PINCLIENT::INT32 *offset);

/* Return the amount of space reserved for inserting a probe on this
   annotation. */
LEVEL_PINCLIENT::USIZE annotation_get_probespace_bytes(ZCA::ANN &a);

/* Verify the integrity of the annotation pointed to by the handle. */
bool verify_annotation (ZCA::ANN& a);

/* Return the "invalid" handle for ZCA::ANN. */
ZCA::ANN invalid_annotation_handle ();

#endif /* !defined _METADATA_H_ */
