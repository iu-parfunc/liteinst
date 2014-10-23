//===- ARMInstPrinter.h - Convert ARM MCInst to assembly syntax -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an ARM MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

/* Capstone Disassembler Engine */
/* By Nguyen Anh Quynh <aquynh@gmail.com>, 2013> */

#ifndef CS_ARMINSTPRINTER_H
#define CS_ARMINSTPRINTER_H

#include "../../MCInst.h"
#include "../../MCRegisterInfo.h"
#include "../../SStream.h"

void ARM_printInst(MCInst *MI, SStream *O, void *Info);
void ARM_post_printer(csh handle, cs_insn *pub_insn, char *mnem);

#endif
