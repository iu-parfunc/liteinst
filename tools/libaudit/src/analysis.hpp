
#ifndef _ANALYSIS_HPP_
#define _ANALYSIS_HPP_

#include <list>

#include "defs.hpp"
#include "disassembly.hpp"

namespace analysis {

  typedef struct {
    uint32_t total_instruction_count;
    uint32_t probe_able_count;
    uint32_t pii_count;
    uint8_t start_instruction_size;
    int32_t start_probe_able_inst_distance;
    uint8_t end_instruction_size;
    int32_t end_probe_able_inst_distance;
  } ProbeMetaData;

  typedef struct {
    Address start;
    Address end;

    // Instruction offset from the function start to this basic block 
    uint64_t start_ins_offset; 
    // Instruction offset from the function start to this end of this basic block 
    uint64_t end_ins_offset;
    ProbeMetaData pmd;
  } BasicBlock;

  enum ReturnType { RET, TAIL_CALL };

  typedef struct {
    Address addr;
    Address target;
    ReturnType type;
  } ControlReturn;

  typedef struct {
    Address start;
    Address end;
    ProbeMetaData pmd;
    std::list<BasicBlock>* bbl;
    std::list<ControlReturn>* returns;
  } Function;

  typedef struct {
    std::list<BasicBlock>* bbl;
    std::list<ControlReturn>* returns; 
    Address fn_end;
  } BlockStructure;

  BlockStructure getBlockStructure(Address start, Address end, 
      disassembly::Decoded d);

}

#endif /* _ANALYSIS_HPP_ */
