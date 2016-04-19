
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
#include "elf.hpp"

using namespace std;
using namespace disassembly;
using namespace analysis;
using namespace statistics;
using namespace defs;
using namespace elf;
using namespace utils;

typedef map<Address, FunctionSymbolInfo> FunctionSymbols;

/*
typedef std::map<std:string, uint64_t> Sequences;

Sequences bb_start_sequences;
Sequences bb_end_sequences;
Sequences fn_start_sequences;
Sequences fn_end_sequences; */

FunctionSymbols* func_symbols;
vector<string> fn_black_list = {"_init"}; // Functions not to be touched

list<Function> auditFunctions(vector<string> fns) {
  auto it = func_symbols->begin();
  Address next_fn_start = NULL;
  if (it != func_symbols->end()) {
    next_fn_start = it->first;
  }

  list<Function> functions;
  while (it != func_symbols->end()) {
    Address start = next_fn_start;
    FunctionSymbolInfo fsi = it->second;
    string func_name = fsi.name;
    ++it;

    if (it != func_symbols->end()) {
      next_fn_start = it->first;
    } else {
      fprintf(stderr, "Reached the last function at %p\n",
          start);
      break;
    }

    if (start != (Address) NULL && start != fsi.end && 
        find(fns.begin(), fns.end() , func_name) != fns.end()) { 
      Decoded d = disassembleRange(start, fsi.end);

      BlockStructure bs = getBlockStructure(start, fsi.end, next_fn_start, d);

      Function fn = generateMetaDataForFunction(bs, d);
      fn.name = func_name;

      functions.push_back(fn);
    }
  }

  return functions;
}

list<Function> auditFunctions() {
  auto it = func_symbols->begin();
  Address next_fn_start = NULL;
  if (it != func_symbols->end()) {
    next_fn_start = it->first;
  }

  list<Function> fns;
  while (it != func_symbols->end()) {
    Address start = next_fn_start;
    FunctionSymbolInfo fsi = it->second;
    string func_name = fsi.name;
    ++it;

    if (it != func_symbols->end()) {
      next_fn_start = it->first;
    } else {
      fprintf(stderr, "Reached the last function at %p\n",
          start);
      break;
    }

    /*
    fprintf(stderr, "Decoding function %s start : %p end : %p\n", 
      func_name.c_str(), start, fsi.end); */
    bool is_black_listed = (find(fn_black_list.begin(), fn_black_list.end() , 
        func_name) != fn_black_list.end());
    if (start != (Address) NULL && start != fsi.end) {
      fprintf(stderr, "Function name : %s\n", func_name.c_str());
      Decoded d = disassembleRange(start, fsi.end);

      BlockStructure bs = getBlockStructure(start, fsi.end, next_fn_start, d);

      Function fn = generateMetaDataForFunction(bs, d);
      fn.name = func_name;

      fns.push_back(fn);
    }
  }

  return fns;
}

void readFunctionInfo() {

  func_symbols = new FunctionSymbols;

  vector<FunctionSymbolInfo> fsis = readFunctionSymbols();

  for (FunctionSymbolInfo fsi : fsis) {
    func_symbols->insert(make_pair(fsi.start, fsi));
  }

  /*
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

      if (func_addr != NULL) {
        func_name_mappings->insert(make_pair(func_name, func_addr));
        func_addr_mappings->insert(make_pair(func_addr, func_name));
      }
    }

    fp.close();
  }
  */
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

  fclose(fp1);
  fclose(fp2);
}
