
#include "process.hpp"

namespace liteinst {
namespace rprobes {

using std::map;
using std::vector;
using std::pair;

BasicBlock Function::getBasicBlock(Address addr) {
  auto it = basic_blocks.lower_bound(addr);
  if (it != basic_blocks.end()) {
    if (it->second.start == addr) {
      return it->second;
    } else {
      if (it != basic_blocks.begin()) {
        it--;
        return it->second;
      }
    }
  }

  BasicBlock bb;
  bb.isValid = false;
  return bb;
}

vector<BasicBlock> Function::getBasicBlocks() {
  vector<BasicBlock> bbs;
  for (auto it : basic_blocks) {
    bbs.push_back(it.second);
  }

  return bbs;
}

vector<ControlReturn> Function::getReturns() {
  return returns;
}

void Function::show(FILE* fp, int nspaces) {
  std::string left_pad = getPadding(nspaces);
  fprintf(fp, "%sFunction: %s\n", left_pad.c_str(), name.c_str());
  fprintf(fp, "%s  Address range : [%p - %p]\n", left_pad.c_str(), start,
      end);
  fprintf(fp, "%s  End padding   : %ld\n", left_pad.c_str(),
      end_padding);

  for (auto it : basic_blocks) {
    it.second.show(fp, nspaces + 2);
  }

  for (ControlReturn r : returns) {
    r.show(fp, nspaces + 2);
  }

  fprintf(fp, "\n");
}

} // End rprobes
} // End liteinst 
