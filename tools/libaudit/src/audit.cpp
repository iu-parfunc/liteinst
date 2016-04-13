
#include <list>
#include <map>
#include <sstream>
#include <fstream>
#include <iostream>
#include <vector>
#include <cstdio>
#include <string>
#include <algorithm>

#include "assert.h"

#include "disassembly.hpp"
#include "analysis.hpp"
#include "statistics.hpp"
#include "utils.hpp"

using namespace std;
using namespace disassembly;
using namespace analysis;
using namespace statistics;
using namespace defs;

typedef map<string, Address> FuncNameMapping;
typedef map<Address, string> FuncAddrMapping;

/*
typedef std::map<std:string, uint64_t> Sequences;

Sequences bb_start_sequences;
Sequences bb_end_sequences;
Sequences fn_start_sequences;
Sequences fn_end_sequences; */

FuncNameMapping* func_name_mappings;
FuncAddrMapping* func_addr_mappings;

list<Function> auditFunctions(vector<string> fns) {
  auto it=func_addr_mappings->begin();
  Address end = NULL;
  if (it != func_addr_mappings->end()) {
    end = it->first;
  }

  list<Function> functions;
  while (it != func_addr_mappings->end()) {
    Address start = end;
    string func_name = it->second;
    ++it;

    if (it != func_addr_mappings->end()) {
      end = it->first;
    } else {
      fprintf(stderr, "Reached the last function at %p\n",
          start);
      break;
    }

    if (start != (Address) NULL && 
        find(fns.begin(), fns.end(), func_name) != fns.end()) { 
      Decoded d = disassembleRange(start, end);

      BlockStructure bs = getBlockStructure(start, end, d);

      Function fn = generateMetaData(start, end, bs, d);
      fn.name = func_name;

      functions.push_back(fn);
    }
  }

  return functions;
}

list<Function> auditFunctions() {
  auto it=func_addr_mappings->begin();
  Address end = NULL;
  if (it != func_addr_mappings->end()) {
    end = it->first;
  }

  list<Function> fns;
  while (it != func_addr_mappings->end()) {
    Address start = end;
    string func_name = it->second;
    ++it;

    if (it != func_addr_mappings->end()) {
      end = it->first;
    } else {
      fprintf(stderr, "Reached the last function at %p\n",
          start);
      break;
    }

    fprintf(stderr, "Decoding function %s\n", func_name.c_str());
    if (start != (Address) NULL) { 
      Decoded d = disassembleRange(start, end);

      BlockStructure bs = getBlockStructure(start, end, d);

      Function fn = generateMetaData(start, end, bs, d);
      fn.name = func_name;

      fns.push_back(fn);
    }
  }

  return fns;
}

void readFunctionInfo() {

  func_name_mappings = new FuncNameMapping;
  func_addr_mappings = new FuncAddrMapping;

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

      func_name_mappings->insert(make_pair(func_name, func_addr));
      func_addr_mappings->insert(make_pair(func_addr, func_name));
    }

    fp.close();
  }
}

__attribute__((constructor))
void audit() {

  readFunctionInfo();

  list<Function> fns;
  vector<string> audit_fns;
  char *audit_fns_str = getenv("AUDIT_FUNCTIONS");
  if (audit_fns_str != NULL) {
    tokenize(string(audit_fns_str), audit_fns, ",");
    fns = auditFunctions(audit_fns);
  } else {
    fns = auditFunctions();
  }

  Statistics stats = generateStatistics(&fns);

  FILE* fp1 = fopen("block_info.txt", "a");
  FILE* fp2 = fopen("probes.txt", "a");

  bool verbose = true;
  printBlockInfo(fns, fp1);
  printStatistics(stats, fp2, verbose);

}
