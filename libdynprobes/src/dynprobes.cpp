
#include "dynprobes.hpp"
#include "utils.hpp"

#include "patcher.h"

#include <map>
#include <sstream>
#include <fstream>
#include <iostream>
#include <vector>
#include <cstdio>

using namespace std;

/******** Private type definitions ********/
typedef struct ProbedFunction {
  uint32_t lock;
  ProbeContext probed_contexts;
} ProbedFunction; 

typedef std::map<Address, ProbedFunction> ProbedFunctionInfo; 
typedef std::map<std::string, Address> FuncNameMapping;
typedef std::map<Address, std::string> FuncAddrMapping;

/******** Global variables ********/
ProbedFunctionInfo probed_function_info;
FuncNameMapping func_name_mappings;
FuncAddrMapping func_addr_mappings;

/******** Private Helpers ********/

void readFunctionInfo() {
  string line;
  ifstream fp ("functions.txt");
  std::string::size_type sz = 16;

  if (fp.fail()) {
    fprintf(stderr, "[DynProbes] ERROR : Failed opening "
        "functions.txt. Exiting program\n");
    throw -1;
  }

  if (fp.is_open()) {
    while (getline (fp,line)){
      string token;
      istringstream iss(line);

      vector<string> tokens;
      int num_tokens = tokenize(line, tokens, ",");
      if (num_tokens < 2) {
        continue;
      }

      Address func_addr = (Address) strtoul(tokens[0].c_str(), NULL, 16);
      string func_name = tokens[1];

      func_name_mappings.insert(make_pair(func_name, func_addr));
      func_addr_mappings.insert(make_pair(func_addr, func_name));
    }

    fp.close();
  }
}

// TODO: May be something like a binary tree should be more efficient here.
Address getFunctionEnd(Address func_addr) {
  bool function_found = false;
  for (auto it=func_addr_mappings.begin(); it!=func_addr_mappings.end(); ++it) {
    if (function_found) {
      return it->first;
    }

    if (func_addr == it->first) {
      function_found = true;  
   }
  }
  return NULL;
}

// Gets the start of the function which this address is contained
Address getContainedFunctionStart(Address addr) {
  Address previous = NULL;
  for (auto it=func_addr_mappings.begin(); it!=func_addr_mappings.end(); ++it) {
    if (addr < it->first) {
      return previous;
    }
    previous = it->first;  
  }
  return previous;
}

Address getContainedFunctionEnd(Address addr) {
  bool function_found = false;
  for (auto it=func_addr_mappings.begin(); it!=func_addr_mappings.end(); ++it) {
    if (function_found) {
      return it->first;
    }

    if (addr > it->first) {
      function_found = true;
    }
  }
  return NULL;
}

Address geFunctionAddress(string function) {
  auto it = func_name_mappings.find(function);
  if (it != func_name_mappings.end()) {
    return it->second;
  }
  return NULL;
}

Probes* handleFunctionEntry(Address func_addr) {
  return injectProbe(func_addr);
}

Probes* handleFunctionExit(Address func_addr) {
  Address fn_end = getFunctionEnd(func_addr);
  if (fn_end != NULL) {
    // Distorm the function
    // Should handle
    // 1. ret, leave
    // 2. tail calls to other functions
  }
  return injectProbe(func_addr);
}

Probes* mergeProbes(Probes* p1, Probes* p2) {
  Probes* p = new Probes[p1->size + p2->size];
  p->size = p1->size + p2->size;

  for (int i=0; i < p->size; i++) {
    if (i < p1->size) {
      p[i] = p1[i];
    } else {
      p[i] = p2[i - p1->size];
    }
  }

  delete p1;
  delete p2;

  return p;
}

/********* Public API Implementation **********/

__attribute__((constructor)) void initialize(); 
void initialize(){
  readFunctionInfo();
}

Probes* injectProbe(Address addr) {
  // Use distorm
  Address fn_end = getContainedFunctionEnd(addr);
  // We cannot chuck in a relaive jmp before end of the function
  if (addr + 5 < fn_end) { 
  }
  return NULL;
}

Probes* injectProbesAtFunction(Address func_addr, ProbeContext ctx) {
  Probes* p = NULL;
  if (ctx & LINE_NUM) {
    fprintf(stderr, "[DynProbes] Line number based probe injection not "
        "implemented yet..\n");
    return p;
  }
  
  if (ctx & ENTRY) {
    p = handleFunctionEntry(func_addr);
  }

  if (ctx & EXIT) {
    Probes* exit = handleFunctionExit(func_addr);
    p = mergeProbes(p, exit);
  }

  if (ctx & OUTCALL_ENTRY) {
    /*
    Probes* outcall_entry = handleOutcallEntry(func_addr);
    p = mergeProbes(p, outcall_entry);  
    */
  }

  if (ctx & OUTCALL_EXIT) {
    /*
    Probes* outcall_exit = handleOutcallExit(func_addr);
    p = mergeProbes(p, outcall_exit);
    */
  }
  
  return p;
}

Probes* injectProbesAtFunction(string function, ProbeContext ctx) {
  
  return NULL;
} 
