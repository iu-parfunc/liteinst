
#include <cassert>
#include <map>
#include "analyzer.hpp"
#include "cfg.hpp"
#include "assembly.hpp"

namespace analysis {

using namespace utils::assembly;

using utils::Address;

Graph* CGBuilder::run(Graph* g, FILE* fp) {

  assert(g->cfg != nullptr);

  if (g->cg == nullptr) {
    g->cg = new CG();
  }

  Disassembler disas;
  for (Function* fn : g->cfg->functions) {
    const Sequence* seq = disas.disassemble(fn->start, fn->end);
    _DInst* decoded = static_cast<_DInst*>(seq->instructions);

    Address pc = fn->start;
    for (int i=0; i < seq->n_instructions; i++) {
      _DInst ins = decoded[i];
      if (disas.isIndirectCall(ins)) {
        IndirectCallEdge* ice = new IndirectCallEdge();
        ice->address = pc;
        fn->indirect_call_edges.push_back(ice);
      } else if (disas.isDirectCall(ins)) {
        DirectCallEdge* dce = new DirectCallEdge();
        dce->address = pc;

        Address offset = reinterpret_cast<Address>(ins.imm.addr);
        Address fn_address = pc + ins.size + (uint64_t) offset;
        auto it = g->cfg->fn_by_addr.find(fn_address);
        if (it != g->cfg->fn_by_addr.end()) {
          dce->target = it->second; 
        }
      }
      pc += ins.size;
    }
  }

  return g;
}

} // End analysis
