
#include "process.hpp"
#include "analysis.hpp"

namespace fastinst { 
namespace rprobes { 

using std::vector;
using std::string;

Process::Process() {

  if (is_initialized) {
    return;
  }

  init_lock.lock();
  if (!is_initialized) {   
    ProcessAnalyzer pa;
    functions = pa.getFunctions();
    mapped = pa.getMappedRegions();

    FunctionAnalyzer fa;
    for (auto& it : functions) {
      fa.analyzeFunction(it.second);
    }
    is_initialized = true;
  }
  init_lock.unlock();

}

Process::~Process() {
}

Function Process::getContainedFunction(Address addr) {
  auto it = functions.lower_bound(addr);
  if (it != functions.end()) {
    if (it->second.start == addr) {
      return it->second;
    } else {
      if (it != functions.begin()) {
        it--;
        return it->second;
      }
    }
  }

  Function f;
  f.isValid = false;
  return f;
}

Function Process::getFunction(Address addr) {
  auto it = functions.find(addr);
  if (it != functions.end()) {
    return it->second;
  } else {
    Function f;
    f.isValid = false;
    return f;
  }
}

vector<Function> Process::getFunctions() {
  vector<Function> fns;
  for (auto it : functions) {
    fns.push_back(it.second);
  }

  return fns;
}

MappedRegion Process::getMappedRegion(Address addr) {
  auto it = mapped.find(addr);
  if (it != mapped.end()) {
    return it->second;
  } else {
    MappedRegion mr;
    mr.isValid = false;
    return mr;
  }
}

MappedRegion Process::getContainedMappedRegion(Address addr) {
  auto it = mapped.lower_bound(addr);
  if (it != mapped.end()) {
    if (it->second.start == addr) {
      return it->second;
    } else {
      if (it != mapped.begin()) {
        it--;
        return it->second;
      }
    }
  }

  MappedRegion mr;
  mr.isValid = false;
  return mr;
}

vector<MappedRegion> Process::getMappedRegions() {
  vector<MappedRegion> mrs;
  for (auto it : mapped) {
    mrs.push_back(it.second);
  }

  return mrs;
}

void Process::show(FILE* fp, int nspaces) {
  string left_pad = getPadding(nspaces);

  ProcessAnalyzer pa;
  fprintf(fp, "%sExecutable: %s\n", left_pad.c_str(), 
      pa.getProgramPath().c_str());

  for (auto it : functions) {
    it.second.show(fp, nspaces + 2);
  }

  for (auto it : mapped) {
    it.second.show(fp, nspaces + 2);
  }

  fprintf(fp, "\n");
}

} // End rprobes 
} // End fastinst 
