
#include <vector>
#include <list>
#include <set>
#include <algorithm>

#include "analysis.hpp"
#include "process.hpp"
#include "assembly.hpp"
#include "assert.h"
#include "distorm.h"
#include "mnemonics.h"

namespace liteinst {
namespace liteprobes {

using std::set;
using std::list;
using std::vector;
using std::find;
using std::pair;
using std::iterator;
using utils::Address;


const int FunctionAnalyzer::PROBE_READY_INSTRUCTION_SIZE = 5;

/****************** Internal type + function definitions *********************/

/* Private type definitions */
typedef struct {
  set<Address> block_starts;
  set<uint32_t> block_start_offsets;
  set<Address> block_ends;
  set<uint32_t> block_end_offsets;

  list<ControlReturn> returns;

  Address fn_end;
  uint64_t end_padding_size;
} BlockBoundaries;

typedef struct {
  Address addr;
  uint32_t offset;
} AddressInfo;

/* Private helper functions */

bool isCall(_DInst i) {
  return (i.opcode == I_CALL || i.opcode == I_CALL_FAR);
}

bool isRelativeJump(_DInst i) {
  return (i.opcode == I_JA || i.opcode == I_JAE || 
      i.opcode == I_JB || i.opcode == I_JBE || i.opcode == I_JCXZ || 
      i.opcode == I_JECXZ || i.opcode == I_JG || i.opcode == I_JGE || 
      i.opcode == I_JL || i.opcode == I_JLE || i.opcode == I_JMP || 
      i.opcode == I_JNO || i.opcode == I_JNP || 
      i.opcode == I_JNS || i.opcode == I_JNZ || i.opcode == I_JO || 
      i.opcode == I_JP || i.opcode == I_JRCXZ || i.opcode == I_JS || 
      i.opcode == I_JZ);
}

bool isFarJump(_DInst i) {
  return (i.opcode == I_JMP_FAR);
}

bool isJump(_DInst i) {
  return isRelativeJump(i) || isFarJump(i);
}

bool isHalt(_DInst i) {
  return (i.opcode == I_HLT);
}

bool isReturn(_DInst i) {
  return (i.opcode == I_RET);
} 

bool endsBasicBlock(_DInst i) {
  return isReturn(i) || isJump(i) || isHalt(i);
}

Address extractJumpTarget(Address ip, _DInst instruction) {
  return (ip + instruction.imm.addr + instruction.size);
}

Address extractFarJumpTarget(_DInst instruction) {
  return (Address) instruction.imm.ptr.off;
}

uint32_t findAddrOffset(Address start, Address addr, Sequence seq) {

  // fprintf(stderr, "[findAddrOffset] Start : %p Addr : %p \n", start, addr);
  _DInst* decoded = static_cast<_DInst*>(seq.instructions);
  uint32_t offset = 0;
  Address ip = start;
  for (int i=0; i < seq.n_instructions; i++) {
    if (ip == addr) {
      return offset;
    }
    ip += decoded[i].size;
    offset++;
  }

  fprintf(stderr, "Invalid offset..\n");
  assert(false);
}

AddressInfo findPrevInstructionAddressInfo(Address start, Address addr, 
    Sequence seq) {

  AddressInfo addrInfo;
  addrInfo.addr = NULL;
  _DInst* decoded = static_cast<_DInst*>(seq.instructions);
  uint32_t offset = 0;
  Address ip = start;
  for(int i=0; i < seq.n_instructions; i++) {
    if (addr == (ip + decoded[i].size)) {
      addrInfo.addr = ip;
      addrInfo.offset = offset;

      return addrInfo;
    } else {
      ip += decoded[i].size;
      offset++;
    }
  }

  fprintf(stderr, "Couldn't find a valid previous instruction..\n");
  return addrInfo;
}

BlockBoundaries generateBlockBoundaries(Address start, Address end,
    Sequence seq) {

  // fprintf(stderr, "[generateBlockBoundaries] Start : %p End : %p\n", start, end);
  _DInst* decoded = static_cast<_DInst*>(seq.instructions);

  Address ip = start;
  list<ControlReturn> returns;
  bool prev_block_end = true;

  set<Address> block_starts;
  set<uint32_t> block_start_offsets;
  set<Address> block_ends;
  set<uint32_t> block_end_offsets;

  Address fn_end = end;
  uint64_t end_padding_size = 0;
  uint64_t current_block_size = 0;

  for (int i=0; i < seq.n_instructions; i++) {
    if (prev_block_end) {
      block_starts.insert(ip);
      block_start_offsets.insert(i);

      prev_block_end = false;
      current_block_size = 0;
    }

    current_block_size += decoded[i].size;
    if (endsBasicBlock(decoded[i])) {

      // Fix the padding space to be zero since there is no padding
      // between this function end and the next function
      if (i == seq.n_instructions - 1) {
        current_block_size = 0;
      }

      if (isReturn(decoded[i])) {
        ControlReturn r;
        r.addr = ip;
        r.target =  (Address) NULL; 
        r.type = ReturnType::RET; 

        returns.push_back(r);

      } else if (isJump(decoded[i])) {

        if (isRelativeJump(decoded[i])) {
          Address addr = extractJumpTarget(ip, decoded[i]);
          // If the jmp target is not within the function then it must be a 
          // tail call
          if (addr <= start || addr >= end) {
            ControlReturn r;
            r.addr = ip;
            r.target = addr;
            r.type = ReturnType::TAIL_CALL;

            returns.push_back(r);
          } else {
            // If the jmp target is within the function then the target must
            // be a block leader
            block_starts.insert(addr);
            block_start_offsets.insert(findAddrOffset(start, addr, seq));

            // If we are jumping back up in the instruction stream this 
            // might be inducing a new block end which we
            // haven't encountered while parsing that region before
            if (addr < ip) {
              AddressInfo addrInfo = findPrevInstructionAddressInfo(start, addr,
                  seq);
              // This could be NULL, for example if the jmp target is the 
              // function start it self. Then it would not make sense to 
              // find a previous instruction since it would be out of range
              // for the function currently being considered
              if (addrInfo.addr != NULL) {
                block_ends.insert(addrInfo.addr);
                block_end_offsets.insert(addrInfo.offset);
              }
            }
          }
        } else if (isFarJump(decoded[i])) {
          ControlReturn r;
          r.addr = ip;
          r.target = extractFarJumpTarget(decoded[i]);
          r.type = ReturnType::TAIL_CALL;

          returns.push_back(r);
        }
      } else if (isHalt(decoded[i])) {
        ControlReturn r;
        r.addr = ip;
        r.target = NULL;
        r.type = ReturnType::HALT;

        returns.push_back(r);
      }

      block_ends.insert(ip);
      block_end_offsets.insert(i);

      prev_block_end = true;
    } else {
      // Function ends with a tail call
      if (i == seq.n_instructions - 1) {
        if (isCall(decoded[i])) {
          ControlReturn r;
          r.addr = ip;
          r.target = extractFarJumpTarget(decoded[i]);
          r.type = ReturnType::TAIL_CALL;

          returns.push_back(r);
          block_ends.insert(ip);
          block_end_offsets.insert(i);
        } else {
          // Function ends wtihout either a block ending instruction or a 
          // control transfer in the form of a tail call. This should not 
          // happen in our books. Fail fast.
          assert(false);
        }
      }
    }

    Address next_ip = ip + decoded[i].size;
    // Find if the next instruction is a jump target from a previously
    // encountered block. If so this instruction marks the end of the  
    // current block
    bool is_block_end = (find(block_starts.begin(), block_starts.end(), 
          next_ip) != block_starts.end());

    if (is_block_end) {
      block_ends.insert(ip); 
      block_end_offsets.insert(i);

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
  bb.fn_end = fn_end;
  bb.end_padding_size = end_padding_size; 

  return bb;
}

list<BasicBlock> generateBasicBlocks(BlockBoundaries bb) {

  assert(bb.block_starts.size() == bb.block_start_offsets.size());
  assert(bb.block_ends.size() == bb.block_end_offsets.size());
  assert(bb.block_starts.size() == bb.block_ends.size());
  vector<Address> block_starts = vector<Address>(bb.block_starts.begin(),
      bb.block_starts.end());
  vector<uint32_t> block_start_offsets = 
    vector<uint32_t>(bb.block_start_offsets.begin(), 
        bb.block_start_offsets.end());
  vector<Address> block_ends = vector<Address>(bb.block_ends.begin(),
      bb.block_ends.end());
  vector<uint32_t> block_end_offsets = 
    vector<uint32_t>(bb.block_end_offsets.begin(), 
        bb.block_end_offsets.end());

  list<BasicBlock> bbl;
  for(unsigned int i=0; i < block_starts.size(); i++) {
    BasicBlock bb;
    bb.start = block_starts[i];
    bb.end = block_ends[i];

    bbl.push_back(bb);
  }

  return bbl;
}

void populateStructuralInformation(Function& func, Sequence seq) {

  if (seq.n_instructions > 0) {

    BlockBoundaries bb = generateBlockBoundaries(func.start, func.end, seq);

    list<BasicBlock> bbl = generateBasicBlocks(bb);
    for (BasicBlock bb : bbl) {
      func.basic_blocks.insert(pair<Address, BasicBlock>(bb.start, bb));
    }

    func.end_padding = (int64_t) func.next - (int64_t) func.end;
    // TODO : To detect correct function end in case there is some padding
    // between consecutive functions. As for now we just set what we get.
  }
}

void populateProbeReadinessInformation(Function& func, Sequence seq) {
  _DInst* decoded = static_cast<_DInst*>(seq.instructions);
  Address ip = func.start;
  for (int i=0; i < seq.n_instructions; i++) {
    if (decoded[i].size >= FunctionAnalyzer::PROBE_READY_INSTRUCTION_SIZE) {
      func.probe_ready_sites.push_back(ip);
    }

    ip += decoded[i].size;
  }
}

/****************** End internal type + function definitions ******************/

/************************ Public API implementation ***************************/

void FunctionAnalyzer::analyzeFunction(Function& func) {

  Sequence seq = disas.disassemble(func.start, func.end);
  populateStructuralInformation(func, seq);
  populateProbeReadinessInformation(func, seq);
}

/********************** End public API implementation *************************/

} // End liteprobes 
} // End liteinst 
