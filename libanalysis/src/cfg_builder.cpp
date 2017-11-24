
#include <cassert>
#include <set>
#include "cfg.hpp"
#include "analyzer.hpp"
#include "assembly.hpp"
#include "process.hpp"

namespace analysis {

using namespace utils::assembly;

using std::set;
using std::vector;
using utils::Address;

bool endsBasicBlock(_DInst i) {
  Disassembler disas;
  return disas.isReturn(i) || disas.isJump(i) || disas.isHalt(i);
}

void analyzeControlTransfer(ControlTransfer* ct, BasicBlock* bbl, CFG* cfg) {
  Disassembler disas;
  Address addr = ct->address;

  Address start = bbl->start;
  Address end   = bbl->end;
  if (start == end) {
    end = end + 15; // Be conservative and use maximum instruction length of x86
  }

  const Sequence* seq = disas.disassemble(end, end + 15);
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);

  assert(seq->n_instructions >= 1);
  
  // First instruction in the decoded sequence must be the last instruction of 
  // the basic block
  _DInst ins = decoded[0];
  Address successorBB;
  if (!endsBasicBlock(ins)) {
    // This is a fall through basic block. The target is the next basic block
    // right after this basic block
    successorBB = addr + ins.size;
  } else {
    if (disas.isRelativeJump(ins)) {
      Address offset = reinterpret_cast<Address>(ins.imm.addr);
      successorBB = addr + ins.size + (uint64_t) offset;
    } else if (disas.isIndirectJump(ins)) {
      ct->is_indirect = true;
      return;
    } else if (disas.isReturn(ins)) {
      ct->is_indirect = true;
      ct->is_return = true;
      return;
    }
  } 

  BasicBlock* bb = cfg->getBasicBlock(successorBB); 
  if (bb != nullptr) {
    ct->targets.insert(bb);
  }
}

Graph* CFGBuilder ::run(Graph* g, FILE* fp) {

  if (g->cfg == nullptr) {
    g->cfg = new CFG();
  }

  utils::process::Process p; 
  p.getFunctions();

  vector<utils::process::Function*> fns = p.getFunctions();
  g->cfg->functions.reserve(fns.size());

  long n_bbs = 0;
  for (utils::process::Function* fn : fns) {
    n_bbs += fn->basic_blocks.size();
  }

  g->cfg->basic_blocks.reserve(n_bbs);

  set<Address> seen;
  for (utils::process::Function* fn : fns) {
    Function* function = new Function;
    function->name  = fn->name;
    function->start = fn->start;
    function->end   = fn->end;

    for (utils::process::BasicBlock* bb : fn->getBasicBlocks()) {
      // Skip already seen basic blocks. This may happen if multiple 
      // functions share the same basic block.
      auto it = seen.find(bb->start);
      if (it != seen.end()) {
        auto iter = g->cfg->basic_blocks_by_addr.find(bb->start);
        assert(iter != g->cfg->basic_blocks_by_addr.end());
        function->basic_blocks.push_back(iter->second);
        continue;
      }

      seen.insert(bb->start);
      BasicBlock* basic_block = new BasicBlock();
      basic_block->start = bb->start;
      basic_block->end   = bb->end;

      ControlTransfer* ct = new ControlTransfer();
      ct->address = bb->end;

      // analyzeControlTransfer(ct, basic_block, g->cfg);

      basic_block->out_edge = ct;

      function->basic_blocks.push_back(basic_block);
      g->cfg->basic_blocks_by_addr.insert(
          std::pair<Address, BasicBlock*>(bb->start, basic_block));
    }

    g->cfg->functions.push_back(function);
    g->cfg->fn_by_addr.insert(
        std::pair<Address, Function*>(fn->start, function));
  }

  assert(fns.size() == g->cfg->functions.size());

  // Now do a second pass over basic blocks to determine the control
  // flow edges between the basic blocks
  for (Function* fn : g->cfg->functions) {
    for (BasicBlock* bb : fn->basic_blocks) {
      analyzeControlTransfer(bb->out_edge, bb, g->cfg);
    }
  }

  return g;
}

} // End analysis
