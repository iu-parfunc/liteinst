
#include <list>
#include <map>
#include <sstream>
#include <fstream>
#include <iostream>
#include <vector>
#include <cstdio>
#include <string>

#include "assert.h"

#include "disassembly.hpp"
#include "analysis.hpp"
#include "statistics.hpp"
#include "utils.hpp"

using namespace std;
using namespace disassembly;
using namespace analysis;
using namespace statistics;

typedef std::map<std::string, Address> FuncNameMapping;
typedef std::map<Address, std::string> FuncAddrMapping;

/*
typedef std::map<std:string, uint64_t> Sequences;

Sequences bb_start_sequences;
Sequences bb_end_sequences;
Sequences fn_start_sequences;
Sequences fn_end_sequences; */

FuncNameMapping func_name_mappings;
FuncAddrMapping func_addr_mappings;

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

void auditProbes() {

  readFunctionInfo();

  auto it=func_addr_mappings.begin();
  Address end = NULL;
  if (it != func_addr_mappings.end()) {
    end = it->first;
  }

  list<Function> fns;
  while (it != func_addr_mappings.end()) {
    Address start = end;
    ++it;

    if (it != func_addr_mappings.end()) {
      end = it->first;
    } else {
      fprintf(stderr, "Reached the last function at %p\n",
          start);
      break;
    }

    Decoded d = disassembleRange(start, end);

    BlockStructure bs = getBlockStructure(start, end, d);

    Function fn = generateMetaData(start, end, bs, d);

    fns.push_back(fn);
  }

  Statistics stats = generateStatistics(&fns);

  FILE* fp = fopen("probes.out", "a");
  bool verbose = true;
  printStatistics(stats, fp, verbose);
}
