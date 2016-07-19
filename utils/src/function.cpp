
#include "process.hpp"

namespace utils {
namespace process {

using std::map;
using std::vector;
using std::pair;
using std::move;
using std::unique_ptr;
using utils::Address;

utils::concurrency::SpinLock Function::init_lock;
bool Function::is_initialized;

std::unique_ptr<BasicBlock> Function::invalid_bb;
std::unique_ptr<ControlReturn> Function::invalid_cr;


Function::Function() {

  if (is_initialized) {
    return;
  }

  init_lock.lock();
  if (!is_initialized) {   
    BasicBlock* bb = new BasicBlock();
    bb->is_valid = false;
    unique_ptr<BasicBlock> bb_ptr(bb);
    invalid_bb = move(bb_ptr);

    ControlReturn* cr = new ControlReturn();
    cr->is_valid = false;
    unique_ptr<ControlReturn> cr_ptr(cr);
    invalid_cr = move(cr_ptr);

    is_initialized = true;
  }
}

Function::~Function() {
}

BasicBlock* Function::getBasicBlock(Address addr) {
  auto it = basic_blocks.lower_bound(addr);
  if (it != basic_blocks.end()) {
    if (it->second->start == addr) {
      return it->second.get();
    } else {
      if (it != basic_blocks.begin()) {
        it--;
        return it->second.get();
      }
    }
  }

  return invalid_bb.get();
}

vector<BasicBlock*> Function::getBasicBlocks() {
  vector<BasicBlock*> bbs;
  for (auto& it : basic_blocks) {
    bbs.push_back(it.second.get());
  }

  return bbs;
}

vector<ControlReturn*> Function::getReturns() {
  vector<ControlReturn*> rs;
  for (auto& r : returns) {
    rs.push_back(r.get());
  }

  return rs;
}

void Function::show(FILE* fp, int nspaces) {
  std::string left_pad = getPadding(nspaces);
  fprintf(fp, "%sFunction: %s\n", left_pad.c_str(), name.c_str());
  fprintf(fp, "%s  Address range : [%p - %p]\n", left_pad.c_str(), start,
      end);
  fprintf(fp, "%s  End padding   : %ld\n", left_pad.c_str(),
      end_padding);

  for (auto& it : basic_blocks) {
    it.second->show(fp, nspaces + 2);
  }

  for (auto& r : returns) {
    r->show(fp, nspaces + 2);
  }

  fprintf(fp, "\n");
}

} // End liteprobes 
} // End liteinst 
