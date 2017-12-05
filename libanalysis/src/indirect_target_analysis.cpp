
#include <cassert>
#include <set>
#include "cfg.hpp"
#include "analyzer.hpp"
#include "assembly.hpp"

namespace analysis {

using std::set;
using utils::Address;

Graph* IndirectTargetAnalysis::run(Graph* g, FILE* fp) {

  assert(g->cfg != nullptr);
  assert(g->cg != nullptr);

  set<Address> addrs;
  for (Function* fn : g->cfg->functions) {
    for (IndirectCallEdge* ice : fn->indirect_call_edges) {
      Address start = fn->start;
      Address end = ice->address;

      _DInst ins = ice->decoded;

      if (ins.opcode == I_CALL) {
        for (int j= 0; j < OPERANDS_NO; j++) {
          bool operand_found = false;
          switch (ins.ops[j].type) {
            case O_REG:
              if (ins.ops[j].index = R_RIP) {
                Address target = (Address) INSTRUCTION_GET_RIP_TARGET(&ins);
                auto it = g->cfg->fn_by_addr.find(target);
                if (it != g->cfg->fn_by_addr.end()) {
                  ice->targets.push_back(it->second);
                } else {
                  assert(false);
                }
              } else {
                // Backward slice logic
              }
              operand_found = true;
              break;
            case O_SMEM:
              operand_found = true;
              break;
            case O_MEM:
              operand_found = true;
              break;
          }

          if (operand_found) {
            break;
          }
        }
      }
    }

    for (BasicBlock* bb : fn->basic_blocks) {
      if (bb->out_edge->is_indirect) {
        // addrs.insert(bb->out_edge->address);
      }
    }
  }

  for (Address addr : addrs) {
    fprintf(fp, "%p\n", addr); 
  }

  return g;
}

}
