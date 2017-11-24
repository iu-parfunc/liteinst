
#include "cfg.hpp"

namespace analysis {

void Function::show(FILE* fp, int nspaces) {
  std::string left_pad = getPadding(nspaces);
  fprintf(fp, "%sFunction: %s\n", left_pad.c_str(), name.c_str());
  fprintf(fp, "%s  Address range : [%p - %p]\n", left_pad.c_str(), start,
      end);

  for (BasicBlock* bb : basic_blocks) {
    bb->show(fp, nspaces + 2);
  }

  for (auto& r : returns) {
    r->show(fp, nspaces + 2);
  }

  fprintf(fp, "\n");
}

} // End analysis
