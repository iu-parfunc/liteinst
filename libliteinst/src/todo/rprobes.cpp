
#include "dynprobes.hpp"
#include "dynprobes_internal.hpp"
#include "utils.hpp"

#include "patcher.h"

#include <map>
#include <sstream>
#include <fstream>
#include <iostream>
#include <vector>
#include <cstdio>

using namespace std;



/******** Global variables ********/
ProbedFunctionInfo probed_function_info;
ProbeInfo probe_info;
IslandInfo island_info;
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

Probes* getProbeMetaData(Address func_addr, ProbeContext ctx) {

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

Probes* handleOutcallEntry(Address func_addr) {
  // Handle
  // 1. Regular relative calls.
  // 2. Long calls.
  // 3. Shared library calls 

}

Probes* handleOutcallExit(Address func_addr) {
  // Handle
  // 1. Regular relative calls.
  // 2. Long calls.
  // 3. Shared library calls 


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

Decoded decodeRange(Address start_addr, Address end_addr){
  uint64_t n_decode_bytes = (uint64_t)((uint64_t)end_addr - (uint64_t)start_addr);

  Decoded d;

  /* why * 2 * n_decode_bytes */
  _DInst* result = (_DInst*)malloc(sizeof(_DInst) * 2 * n_decode_bytes);
  unsigned int instruction_count = 0;

  _CodeInfo ci = {0}; /* another modernity */
  ci.code = (uint8_t*)start_addr;
  ci.codeLen = n_decode_bytes;
  ci.dt = Decode64Bits;   /* here we make this AMD64 specific ? */
  ci.codeOffset = 0x100000;

  _DecodeResult res = distorm_decompose(&ci, result, n_decode_bytes, &instruction_count);
  if (res != DECRES_SUCCESS) {
    free(result);
    result = NULL;

    d.decoded_instructions = NULL;
    d.n_instructions = 0;

    return d;
  }

  d.decoded_instructions = result;
  d.n_instructions = instruction_count;
  return d;

}

// Assumes decoded instruction stream either starts with with addr
// or terminates with it depending on whether the direction is up
// or down
ProbeOffset findProbeAbleInstruction(Address addr, Decoded d, 
    TraverseDirection direction) {
  ProbeOffset o;
  o.offset = 0;
  o.direction = direction;
  o.n_intermediate_instructions = 0;
  o.intermediate_addrs = new Vector<Address>;

  int offset_ptr = 0;
  switch (direction) {
    case DOWN:
      /* Search through instructions from the end */
      for (unsigned int i = d.n_instructions-1; i >= 0; i--) {
        /* skip non-decodable */
        if (decoded[i].flags == FLAG_NOT_DECODABLE) continue;

        o.offset -= decoded[i].size;
        o.n_intermediate_instructions++;
        o.intermediate_addrs->push(addr + offset);
        
        // Instruction length has to be equal to or greater than 5 bytes in 
        // order to be probe-able
        if (decoded[i].size >= 5) {
          return o;
        } 
      }
      break;
    case UP:
      /* Search through instructions from the start */
      for (unsigned int i = 0; i < d.n_instructions; i++) {
        /* skip non-decodable */
        if (decoded[i].flags == FLAG_NOT_DECODABLE) continue;

        o.offset += decoded[i].size;
        o.n_intermediate_instructions++;
        o.intermediate_addrs->push(addr + offset);
        
        if (decoded[i].size >= 5) {
          return o;
        } 
      }
      break;
    default:
      break;
  }

  // No probe-able instructions found
  o.n_intermediate_instructions = -1;
  return o;
}

/********* Public API Implementation **********/

__attribute__((constructor)) void initialize(); 
void initialize(){
  readFunctionInfo();
}

// Algorithm
// if length < 5
//   Try instruction punning
//   OR 
//   Try instruction jump
//   OR
//   Try relocation (Need to handle loop back edges) 
//   OR
//   Use INT3
Probes* injectProbe(Address addr, ProbeFn fn) {
  // Use distorm
  Address fn_end = getContainedFunctionEnd(addr);
  Address fn_start = getContainedFunctionStart(addr);

  int instruction_size = getInstructionSize(addr);

  if (size < 5) {
    
  } else { 
    auto it = probe_info.find(addr);
    if (it != probe_info.end()) {
      Probe p = it->second;
      p.injection_fn();
    } else { // New probe site
      int32_t mem_chunk = ((uint64_t)addr) >> 32;
      auto it = island_info.find(mem_chunk);
      if (it != island_info.end()) {
        MemIslandGroup ig = it->second;
        MemIsle current_isle = ig.current_isle;
        int32_t stub_size = genericStubSize + instruction_size; 
        // if (current_isle.atomic_increment(stub_size) <= current_isle.end) {
          generateStub(addr, addr_content, instruction_size, fn); 
        // } else {
          ig.isle_lock.lock();
          // if (current_isle.atomic_increment(stub_size) <= current_isle.end) {
            ig.isle_lock.unlock();
          // } else {
            // create_isle
            // add_isle 
            // set current_isle 
               ig.isle_lock.unlock(); 
          //}
          generateStub(addr, addr_content, instruction_size, fn); 
      }
    }
    // if (island_not_in_cache)
    //   Get mem island address
    //   JIT the stub
    //     Save context
    //     Call profiler
    //     Restore context
    //     Displace the original instruction with fix up if necessary
    //     Write the jump back
    //   Pointpatch the relative jump to original instruction 
    // } else {
    //   Pointpatch the jump to existing stub
    // }
  }

  // We cannot chuck in a relaive jmp before end of the function
  if (addr + 5 < fn_end) { 
    Decoded d = decodeRange(fn_start, addr);
    ProbeOffset o = findProbeAbleInstruction(addr, d, UP);
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
