
#include <set>
#include <algorithm>

#include "assert.h"
#include "analysis.hpp"

using namespace std;
using namespace disassembly;

namespace analysis {

  /* Private type definitions */
  typedef struct {
    set<Address>* block_starts;
    set<uint32_t>* block_start_offsets;
    set<Address>* block_ends;
    set<uint32_t>* block_end_offsets;

    list<ControlReturn>* returns;
  } BlockBoundaries;

  typedef struct {
    Address addr;
    uint32_t offset;
  } AddressInfo;

  /* Private helper functions */

  inline bool isRelativeJump(_DInst i) {
    return (i.opcode == I_JA || i.opcode == I_JAE || 
      i.opcode == I_JB || i.opcode == I_JBE || i.opcode == I_JCXZ || 
      i.opcode == I_JECXZ || i.opcode == I_JG || i.opcode == I_JGE || 
      i.opcode == I_JL || i.opcode == I_JLE || i.opcode == I_JMP || 
      i.opcode == I_JNO || i.opcode == I_JNP || 
      i.opcode == I_JNS || i.opcode == I_JNZ || i.opcode == I_JO || 
      i.opcode == I_JP || i.opcode == I_JRCXZ || i.opcode == I_JS || 
      i.opcode == I_JZ);
  }

  inline bool isFarJump(_DInst i) {
    return (i.opcode = I_JMP_FAR);
  }

  inline bool isJump(_DInst i) {
    return isRelativeJump(i) || isFarJump(i);
  }

  inline bool isReturn(_DInst i) {
   return (i.opcode == I_RET);
  } 

  inline bool endsBasicBlock(_DInst i) {
    return isReturn(i) || isJump(i);
  }

  inline Address extractJumpTarget(_DInst instruction) {
    return (Address) INSTRUCTION_GET_TARGET(&instruction);
  }

  inline Address extractFarJumpTarget(_DInst instruction) {
    return (Address) instruction.imm.ptr.off;
  }

  uint32_t findAddrOffset(Address start, Address addr, 
      Decoded d) {

    _DInst* decoded = d.decoded_instructions;
    uint32_t offset = 0;
    // Address ip = start;
    for (unsigned int i=0; i < d.n_instructions; i++) {
      if ((Address) decoded[i].addr == addr) {
        return offset;
      }
      // ip += decoded[i].size;
      offset++;
    }

    fprintf(stderr, "Invalid offset..\n");
    assert(false);
    return 0;
  }

  AddressInfo findPrevInstructionAddressInfo(Address start, Address addr, 
      Decoded d) {

    AddressInfo addrInfo;
    addrInfo.addr = NULL;
    _DInst* decoded = d.decoded_instructions;
    uint32_t offset = 0;
    // Address ip = start;
    for(unsigned int i=0; i < d.n_instructions; i++) {
      if ((i+1) < d.n_instructions &&
          addr == (Address) decoded[i].addr + decoded[i].size) {
        addrInfo.addr = (Address) decoded[i].addr;
        addrInfo.offset = offset;

        return addrInfo;
      } else {
        // ip += decoded[i].size;
        offset++;
      }
    }

    fprintf(stderr, "Couldn't find a valid previous instruction..\n");
    return addrInfo;
  }

  BlockBoundaries generateBlockBoundaries(Address start, Address end, Decoded d) {

    _DInst* decoded = d.decoded_instructions;

    Address ip = start;
    list<ControlReturn>* returns = new list<ControlReturn>;
    bool prev_block_end = true;

    set<Address>* block_starts = new set<Address>;
    set<uint32_t>* block_start_offsets = new set<uint32_t>;
    set<Address>* block_ends = new set<Address>;
    set<uint32_t>* block_end_offsets = new set<uint32_t>;

    for (unsigned int i=0; i < d.n_instructions; i++) {
      if (prev_block_end) {
        block_starts->insert(ip);
        block_start_offsets->insert(i);

        prev_block_end = false;
      }

      if (endsBasicBlock(decoded[i])) {

        if (isReturn(decoded[i])) {
          ControlReturn r;
          r.addr = ip;
          r.target =  (Address)__builtin_extract_return_addr(
              __builtin_return_address(0)); 
          r.type = RET; 

          returns->push_back(r);

        } else if (isJump(decoded[i])) {

          if (isRelativeJump(decoded[i])) {
            Address addr = extractJumpTarget(decoded[i]);
            // If the jmp target is not within the function then it must be a 
            // tail call
            if (addr <= start || addr >= end) {
              ControlReturn r;
              r.addr = ip;
              r.target = addr;
              r.type = TAIL_CALL;

              returns->push_back(r);
            } else {
              // If the jmp target is within the function then the target must
              // be a block leader
              block_starts->insert(addr);
              block_start_offsets->insert(findAddrOffset(start, addr, d));

              // If we are jumping back up in the instruction stream this 
              // might be inducing a new block end which we
              // haven't encountered while parsing that region before
              if (addr < ip) {
                AddressInfo addrInfo = findPrevInstructionAddressInfo(start, addr,
                    d);
                // This could be NULL, for example if the jmp target is the 
                // function start it self. Then it would not make sense to 
                // find a previous instruction since it would be out of range
                // for the function currently being considered
                if (addrInfo.addr != NULL) {
                  block_ends->insert(addrInfo.addr);
                  block_end_offsets->insert(addrInfo.offset);
                }
              }
            }
          } else if (isFarJump(decoded[i])) {
            ControlReturn r;
            r.addr = ip;
            r.target = extractFarJumpTarget(decoded[i]);
            r.type = TAIL_CALL;

            returns->push_back(r);
          }
        }

        block_ends->insert(ip);
        block_end_offsets->insert(i);

        prev_block_end = true;
      }

      Address next_ip = ip + decoded[i].size;
      // Find if the next instruction is a jump target from a previously
      // encountered block. If so this instruction marks the end of the  
      // current block
      bool is_block_end = (find(block_starts->begin(), block_starts->end(), 
          next_ip) != block_starts->end());

      if (is_block_end) {
        block_ends->insert(ip); 
        block_end_offsets->insert(i);

        prev_block_end = true;
      }

      ip += decoded[i].size;
    }

    BlockBoundaries bb;
    bb.block_starts = block_starts;
    bb.block_start_offsets = block_start_offsets;
    bb.block_ends = block_ends;
    bb.block_end_offsets = block_end_offsets;
    bb.returns = returns;

    return bb;
  }

  list<BasicBlock>* generateBasicBlocks(BlockBoundaries bb) {

    assert(bb.block_starts->size() == bb.block_start_offsets->size());
    assert(bb.block_ends->size() == bb.block_end_offsets->size());
    assert(bb.block_starts->size() == bb.block_ends->size());

    vector<Address> block_starts = vector<Address>(bb.block_starts->begin(),
        bb.block_starts->end());
    vector<uint32_t> block_start_offsets = 
      vector<uint32_t>(bb.block_start_offsets->begin(), 
          bb.block_start_offsets->end());
    vector<Address> block_ends = vector<Address>(bb.block_ends->begin(),
        bb.block_ends->end());
    vector<uint32_t> block_end_offsets = 
      vector<uint32_t>(bb.block_end_offsets->begin(), 
          bb.block_end_offsets->end());

    list<BasicBlock>* bbl = new list<BasicBlock>();
    for(unsigned int i=0; i < block_starts.size(); i++) {
      BasicBlock bb;
      bb.start = block_starts[i];
      bb.end = block_ends[i];

      bbl->push_back(bb);
    }

    return bbl;
  }

  /* Public API implementation */
  BlockStructure getBlockStructure(Address start, Address end, Decoded d) {

    BlockStructure bs;
    bs.bbl = NULL;

    if (d.decoded_instructions != NULL) {

      BlockBoundaries bb = generateBlockBoundaries(start, end, d);

      bs.bbl = generateBasicBlocks(bb);
      bs.returns = bb.returns;
      bs.fn_end = end;
      // TODO : To detect correct function end in case there is some padding
      // between consecutive functions. As for now we just set what we get.

      return bs;
    }

    return bs;
  }

}
