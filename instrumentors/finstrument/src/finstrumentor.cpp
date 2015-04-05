
#include "finstrumentor.hpp"
#include "patch_utils.hpp"
#include "dynamicarray.h"

#include <stdlib.h>

#include <cstdio>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

// func_table functions; // Function address to function id mappings
volatile uint16_t func_id_counter;
// probe_map probe_info;
Instrumentor* INSTRUMENTOR_INSTANCE = 0;
InstrumentationFunc prologFunction;
InstrumentationFunc epilogFunction;
void* probeInfo;

#ifdef PROBE_HIST_ON
ProbeStatistics* g_probe_stats; 
uint64_t* g_epilog_timings; 
uint64_t* g_prolog_timings; 
uint64_t* g_probe_timings; 
int g_num_bins;
#endif

#ifdef PROBE_TRUE_EMPTY_ON
ProbeStatistics* g_probe_stats; 
uint64_t* g_epilog_timings; 
uint64_t* g_prolog_timings; 
uint64_t* g_probe_timings; 
int g_num_bins;
#endif


Finstrumentor::Finstrumentor(InstrumentationFunc prolog, InstrumentationFunc epilog) {
  this->prologFunc = prolog; 
  this->epilogFunc = epilog; 
  prologFunction = prolog;
  epilogFunction = epilog;
}

void Finstrumentor::initialize() {
  // Here we are leaking object data to global variables so that we don't incur
  // overhead of object access to get to these data. This should be fine since 
  // Finstrumentor is really a singleton object.
  INSTRUMENTOR_INSTANCE = this;
  // this->probeInfo;

  fprintf(stderr, "[Finstrumentor] Initializing the instrumentor\n");

  //setting up global data structures
  // this->probeInfo = new DynamicArray<FinsProbeInfo>(DEFAULT_PROBE_COUNT); 
  this->functions = new func_table; 
  this->function_ids = new func_id_table;
  this->probe_info = new probe_map; 

#ifdef PROBE_HIST_ON
  g_probe_stats = new ProbeStatistics; 
  g_num_bins = PROBE_HIST_MAX_VALUE / BIN_SIZE + 1; // +1 for MAX_VALUE+ bin
  g_probe_stats->prolog_timings = new uint64_t[g_num_bins](); // C++ value initializtion
  g_probe_stats->epilog_timings = new uint64_t[g_num_bins]();
  g_probe_stats->probe_timings = new uint64_t[g_num_bins]();
  g_prolog_timings = g_probe_stats->prolog_timings;
  g_epilog_timings = g_probe_stats->epilog_timings;
  g_probe_timings = g_probe_stats->probe_timings;
#endif

#ifdef PROBE_TRUE_EMPTY_ON
  g_probe_stats = new ProbeStatistics; 
  g_num_bins = PROBE_HIST_MAX_VALUE / BIN_SIZE + 1; // +1 for MAX_VALUE+ bin
  g_probe_stats->prolog_timings = new uint64_t[g_num_bins](); // C++ value initializtion
  g_probe_stats->epilog_timings = new uint64_t[g_num_bins]();
  g_probe_stats->probe_timings = new uint64_t[g_num_bins]();
  g_prolog_timings = g_probe_stats->prolog_timings;
  g_epilog_timings = g_probe_stats->epilog_timings;
  g_probe_timings = g_probe_stats->probe_timings;
#endif

  readFunctionInfo();
}

/*
 * Parameters :
 * probe_id = This is the address of the function
 * flag = Specifies which probes within the probe site to be disabled. Currently 
 *        defaults to function start and end
 */
int Finstrumentor::activateProbeByName(void* probe_id, int flag) {

  uint64_t func_addr = (uint64_t)probe_id;
  uint64_t func_id; 
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    func_id =  func_addr_mappings->find(func_addr)->second->func_id;
  } else {
    return -1;
  }

  return activateProbe(&func_id, flag);

}

/*
 * Parameters :
 * probe_id = This is an integer representing the function.To be used by profiler
 *            implementations internally 
 * flag = Specifies which probes within the probe site to be disabled. Currently 
 *        defaults to function start and end
 */
// Here probe_id is actually function id. We only have function level 
// probe toggling granularity at the moment
// TODO : Protect this call with a lock
int Finstrumentor::activateProbe(void* probe_id, int flag) {

  uint16_t func_id = *(uint16_t*)probe_id;
  uint64_t func_addr; 
  if (func_id_mappings->find(func_id) != func_id_mappings->end()) {
    func_addr =  func_id_mappings->find(func_id)->second->func_addr;
  } else {
    return -1;
  }

  std::list<FinsProbeInfo*>* ls = probe_info->find(func_addr)->second;  
  for (std::list<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
    FinsProbeInfo* info = *it;

    if (info->isActive) {
      continue;
    }

    bool status = modify_page_permissions(info->probeStartAddr);
    if (!status) {
      LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", info->probeStartAddr);
      return -1;
    }

    // fprintf(stderr, "Activating with sequence : %p\n", info->activeSequence);
    uint64_t fetch = *((uint64_t*) info->probeStartAddr);
    uint64_t res = __sync_val_compare_and_swap((uint64_t*)info->probeStartAddr, 
        *((uint64_t*)info->probeStartAddr), info->activeSequence); 
    // fprintf(stderr, "Result : %p\n", res);

    // __sync_bool_compare_and_swap(info->probeStartAddr, 
    //     *(info->probeStartAddr), info->activeSequence); 

    info->isActive = true;
  }

  return 0;

}

/*
 * Parameters :
 * probe_id = This is the address of the function
 * flag = Specifies which probes within the probe site to be disabled. Currently 
 *        defaults to function start and end
 */
int Finstrumentor::deactivateProbeByName(void* probe_id, int flag) {

  uint64_t func_addr = (uint64_t)probe_id;
  uint64_t func_id;
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    func_id =  func_addr_mappings->find(func_addr)->second->func_id;
  } else {
    return -1;
  }

  return deactivateProbe(&func_id, flag);

}

/*
 * Parameters :
 * probe_id = This is an integer representing the function.To be used by profiler
 *            implementations internally 
 * flag = Specifies which probes within the probe site to be disabled. Currently 
 *        defaults to function start and end
 */
// Here probe_id is actually function id. We only have function level 
// probe toggling granularity at the moment
// TODO : Protect this call with a lock
int Finstrumentor::deactivateProbe(void* probe_id, int flag) {

  uint16_t func_id = *(uint16_t*)probe_id;
  uint64_t func_addr;
  if (func_id_mappings->find(func_id) != func_id_mappings->end()) { 
    func_addr =  func_id_mappings->find(func_id)->second->func_addr;
  } else {
    return -1;
  }
  // fprintf(stderr, "Deactivating the probes for function %d..\n", func_id);
  // fprintf(stderr, "probe_info address is : %p\n", probe_info);

  std::list<FinsProbeInfo*>* ls = probe_info->find(func_addr)->second;  

  // fprintf(stderr, "List address for func id %d : %p\n", func_id, ls);
  int count = 0;
  for (std::list<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
    FinsProbeInfo* info = *it;

    if (!info->isActive) {
      continue;
    }

    bool status = modify_page_permissions(info->probeStartAddr);
    if (!status) {
      LOG_ERROR("Patching the probesite failed at %p. Skipping..\n", info->probeStartAddr);
      return -1;
    }

    // fprintf(stderr, "Probe start address %04x\n", (info->probeStartAddr));

    /*
       uint64_t sequence = *((uint64_t*)info->probeStartAddr);
       uint64_t mask = 0x0000000000FFFFFF;
       uint64_t deactiveSequence = (uint64_t) (sequence & mask); 
       mask = 0x9090909090000000;
       deactiveSequence = deactiveSequence | mask;
       */

    // fprintf(stderr, "Active sequence %08x\n", info->activeSequence);
    // fprintf(stderr, "Deactive sequence %08x\n", info->deactiveSequence);

    // fprintf(stderr, "Deactivating with sequence : %p\n", info->deactiveSequence);
    uint64_t fetch = *((uint64_t*) info->probeStartAddr);
    uint64_t res = __sync_val_compare_and_swap((uint64_t*)info->probeStartAddr, 
        *((uint64_t*)info->probeStartAddr), info->deactiveSequence); 

    // fprintf(stderr, "Result : %p\n", res);

    info->isActive = false;
    count++;
  }

  // fprintf(stderr, "Deactivated probe count: %d\n", count);

  return 0;

}

void tokenize(const string& str, vector<string>& tokens, const string& delimiters = " ") {
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (string::npos != pos || string::npos != lastPos)
  {
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
}

uint16_t Finstrumentor::getFunctionId(uint64_t func_addr) {
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    return func_addr_mappings->find(func_addr)->second->func_id;
    // return func_addr_mappings->find(func_addr)->second->func_id;
  } else {
   return 0;
  } 
}

uint64_t Finstrumentor::getFunctionAddress(uint16_t func_id) {
  if (func_id_mappings->find(func_id) != func_id_mappings->end()) {
    return func_id_mappings->find(func_id)->second->func_addr;
  } else {
    return 0;
  }
}

uint64_t* Finstrumentor::getLock(uint64_t func_addr) {
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    return &(func_addr_mappings->find(func_addr)->second->lock);
    // return func_addr_mappings->find(func_addr)->second->func_id;
  } else {
   return NULL;
  } 
}

string Finstrumentor::getFunctionName(uint16_t func_id) {
  if (func_id_mappings->find(func_id) != func_id_mappings->end()) {
    return func_id_mappings->find(func_id)->second->func_name;
  } else {
    return NULL;
  }
}

/*
void printMap(FuncIDMappings* func_id_mappings) {
 for (std::map<uint16_t,FunctionInfo*>::iterator it=func_id_mappings->begin(); it!=func_id_mappings->end(); ++it)
      std::cout << it->first << " => " << it->second->func_name << '\n'; 
}
*/

void Finstrumentor::addFunction(uint64_t addr, char* name) {
  FunctionInfo* func_info = new FunctionInfo;
  // func_addr = (uint64_t) std::stol(tokens[0], &sz);
  func_info->func_addr = addr;
  func_info->func_name = string(name);
  func_info->func_id = func_count-1;
  func_info->lock = 0;

  func_addr_mappings->insert(make_pair(func_info->func_addr, func_info));
  func_id_mappings->insert(make_pair(func_info->func_id, func_info));

  fprintf(stderr, "[DEBUG] Before count : %ld\n", func_count);
  // func_count++;
  fprintf(stderr, "[DEBUG] After count : %ld\n", func_count);
}

void Finstrumentor::readFunctionInfo() {

  fprintf(stderr, "Initializing mappings data structure..\n");
  func_addr_mappings = new FuncAddrMappings;
  func_id_mappings = new FuncIDMappings;

  string line;
  ifstream fp ("functions.txt");
  std::string::size_type sz = 16;

  func_count = 1; // Starts with 1 so that id 0 can be used to signal errors
  if (fp.is_open()) {
    while (getline (fp,line)){
      string token;
      istringstream iss(line);

      vector<string> tokens;
      tokenize(line, tokens, ",");

      FunctionInfo* func_info = new FunctionInfo;
      // func_addr = (uint64_t) std::stol(tokens[0], &sz);
      func_info->func_addr = (uint64_t) strtoul(tokens[0].c_str(), NULL, 16);
      func_info->func_name = tokens[1];
      func_info->func_id = func_count++;
      func_info->lock = 0;

      // fprintf(stderr, "[Finstrumentor] Got line  %s\n", line.c_str());
      // fprintf(stderr, "[Finstrumentor] Addr : %lx Func name : %s\n", func_info->func_addr, func_info->func_name.c_str());

      func_addr_mappings->insert(make_pair(func_info->func_addr, func_info));
      func_id_mappings->insert(make_pair(func_info->func_id, func_info));
    }
    func_count++; // Hack to include calibrate_cache_effects
    // printMap(func_id_mappings);
    fp.close();
  }

  if (func_count == 1) {
    fprintf(stderr, "[Ubiprof] ERROR : functions.txt not present. Ubiprof will not profile this application...\n");
  }
}

#ifdef PROBE_HIST_ON 
void dumpProbeOverheadStatistics() {

  FILE* fp = fopen("overhead.out", "a");

  // prolog timings
  for (int i=0; i < g_num_bins; i++) {
    fprintf(fp, "%lu", g_prolog_timings[i]); 
    if (i < (g_num_bins-1)) {
      fprintf(fp, ",");
    }
  }

  fprintf(fp, "\n");

  // epilog timings
  for (int i=0; i < g_num_bins; i++) {
    fprintf(fp, "%lu", g_epilog_timings[i]); 
    if (i < (g_num_bins-1)) {
      fprintf(fp, ",");
    }
  }

  fprintf(fp, "\n");

  // total probe timings
  for (int i=0; i < g_num_bins; i++) {
    fprintf(fp, "%lu", g_probe_timings[i]); 
    if (i < (g_num_bins-1)) {
      fprintf(fp, ",");
    }
  }

  fprintf(fp, "\n>>\n");

}
#endif

#ifdef PROBE_TRUE_EMPTY_ON
void dumpProbeOverheadStatistics() {

  FILE* fp = fopen("histogram.out", "a");

  // prolog timings
  for (int i=0; i < g_num_bins; i++) {
    fprintf(fp, "%lu", g_prolog_timings[i]); 
    if (i < (g_num_bins-1)) {
      fprintf(fp, ",");
    }
  }

  fprintf(fp, "\n");

  // epilog timings
  for (int i=0; i < g_num_bins; i++) {
    fprintf(fp, "%lu", g_epilog_timings[i]); 
    if (i < (g_num_bins-1)) {
      fprintf(fp, ",");
    }
  }

  fprintf(fp, "\n");

  // total probe timings
  for (int i=0; i < g_num_bins; i++) {
    fprintf(fp, "%lu", g_probe_timings[i]); 
    if (i < (g_num_bins-1)) {
      fprintf(fp, ",");
    }
  }

  fprintf(fp, "\n>>\n");

}
#endif

Finstrumentor::~Finstrumentor() {
  fprintf(stderr, "[finstrumentor] NUM_ACCESSED_PROBE_SITES: %lu\n", ((Finstrumentor*) INSTRUMENTOR_INSTANCE)->probe_info->size()); 

#ifdef PROBE_HIST_ON 
  dumpProbeOverheadStatistics();
#endif

#ifdef PROBE_TRUE_EMPTY_ON
  dumpProbeOverheadStatistics();
#endif

  delete this->probe_info;
  delete this->functions;
  delete this->func_addr_mappings; // Delete individual entries as well 
  delete this->func_id_mappings; 
}
