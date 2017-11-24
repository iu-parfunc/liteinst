
#include <cassert>
#include <set>
#include "cfg.hpp"
#include "analyzer.hpp"
#include "assembly.hpp"

namespace analysis {

using std::set;

using utils::Address;

Graph* IndirectControlFlowDump::run(Graph* g, FILE* fp) {

  assert(g->cfg != nullptr);
  assert(g->cg != nullptr);

  set<Address> addrs;
  for (Function* fn : g->cfg->functions) {
    for (IndirectCallEdge* ice : fn->indirect_call_edges) {
      addrs.insert(ice->address);
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
