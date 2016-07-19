
#include "process.hpp"
#include "analysis.hpp"

namespace utils { 
namespace process { 

using std::map;
using std::vector;
using std::string;
using utils::Address;
using std::unique_ptr;
using std::move;
using utils::range::Range;

utils::range::Range Process::text;
utils::range::Range Process::stack;
utils::range::Range Process::heap;

utils::concurrency::SpinLock Process::init_lock;
bool Process::is_initialized = false;
std::unique_ptr<MappedRegion> Process::invalid_mr;
std::unique_ptr<Function> Process::invalid_fn;

/// Function start address mappings
map<Address, Function*>* Process::fn_by_address = new map<Address, Function*>;  

/// Function name mappings
map<string, unique_ptr<Function>>* Process::fn_by_name = 
  new map<string, unique_ptr<Function>>;  

/// Memory region start address mappings
map<Address, unique_ptr<MappedRegion>>* Process::mapped = 
  new map<Address, unique_ptr<MappedRegion>>; 

Process::Process() {

  if (is_initialized) {
    return;
  }

  init_lock.lock();
  if (!is_initialized) {   
    ProcessAnalyzer pa;
    pa.populateFunctions(fn_by_address, fn_by_name);
    pa.populateMappedRegions(mapped);

    // Finds text, stack and heap ranges
    bool first_region = true;
    Address text_start = nullptr;
    Address prev_end = nullptr;
    for (auto& it : *mapped) {
      MappedRegion* mr = it.second.get();
      if (first_region) {
        text_start = mr->start;
        first_region = false;
      }

      if (string("[heap]").compare(mr->file)) {
        text = Range(text_start, prev_end);
        heap = Range(mr->start, mr->end);
      }

      if (string("[stack]").compare(mr->file)) {
        stack = Range(mr->start, mr->end);
        // stack_reserved = Range(mr->start - stack_allowance, mr->end);
      }
    }

    assert(text.start != nullptr);
    assert(heap.start != nullptr);
    assert(stack.start != nullptr);

    FunctionAnalyzer fa;
    for (auto it : *fn_by_address) {
      fa.analyzeFunction(*(it.second));
    }

    MappedRegion* mr = new MappedRegion();
    mr->is_valid = false;
    unique_ptr<MappedRegion> mr_ptr(mr);
    invalid_mr = move(mr_ptr);

    Function* fn = new Function();
    fn->is_valid = false;
    unique_ptr<Function> fn_ptr(fn);
    invalid_fn = move(fn_ptr);

    is_initialized = true;
  }
  init_lock.unlock();

}

Process::~Process() {
}

Function* Process::getContainedFunction(Address addr) {
  auto it = fn_by_address->lower_bound(addr);
  if (it != fn_by_address->end()) {
    if (it->second->start == addr) {
      return it->second;
    } else {
      if (it != fn_by_address->begin()) {
        it--;
        return it->second;
      }
    }
  }

  return invalid_fn.get();
}

Function* Process::getFunction(Address addr) {
  auto it = fn_by_address->find(addr);
  if (it != fn_by_address->end()) {
    return it->second;
  } else {
    return invalid_fn.get();
  }
}

Function* Process::getFunction(std::string name) {
  auto it = fn_by_name->find(name);
  if (it != fn_by_name->end()) {
    return it->second.get();
  } else {
    return invalid_fn.get();
  }
}

vector<Function*> Process::getFunctions() {
  vector<Function*> fns;
  for (auto it : *fn_by_address) {
    fns.push_back(it.second);
  }

  return fns;
}

int Process::getNumberOfFunctions() {
  return fn_by_address->size();
}

MappedRegion* Process::getMappedRegion(Address addr) {
  auto it = mapped->find(addr);
  if (it != mapped->end()) {
    return it->second.get();
  } else {
    return invalid_mr.get();
  }
}

MappedRegion* Process::getContainedMappedRegion(Address addr) {
  auto it = mapped->lower_bound(addr);
  if (it != mapped->end()) {
    if (it->second->start == addr) {
      return it->second.get();
    } else {
      if (it != mapped->begin()) {
        it--;
        return it->second.get();
      }
    }
  }

  return invalid_mr.get(); 
}

vector<MappedRegion*> Process::getMappedRegions() {
  vector<MappedRegion*> mrs;
  for (auto& it : *mapped) {
    mrs.push_back(it.second.get());
  }

  return mrs;
}

void Process::show(FILE* fp, int nspaces) {
  string left_pad = getPadding(nspaces);

  ProcessAnalyzer pa;
  fprintf(fp, "%sExecutable: %s\n", left_pad.c_str(), 
      pa.getProgramPath().c_str());

  for (auto it : *fn_by_address) {
    it.second->show(fp, nspaces + 2);
  }

  for (auto& it : *mapped) {
    it.second->show(fp, nspaces + 2);
  }

  fprintf(fp, "\n");
}

Range Process::getStack() {
  return stack;
}

Range Process::getHeap() {
  return heap;
}

Range Process::getText() {
  return text;
}

} // End process 
} // End utils 
