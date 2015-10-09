
#include "finstrumentor.hpp"
#include "utils.hpp"
#include "bitmap.hpp"

#include <stdlib.h>
#include <math.h>
#include <limits.h>

#include <cstdio>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <pthread.h> 

#include <signal.h>
#include <assert.h> 

using namespace std;


FinsProbeInfo* populateProbeInfo(uint8_t* probe_addr, bool unpatched);

volatile uint16_t func_id_counter;
uint8_t* g_straddlers_bitmap;
Instrumentor* INSTRUMENTOR_INSTANCE = 0;
InstrumentationFunc prologFunction;
InstrumentationFunc epilogFunction;

uint64_t g_finstrumentor_overhead = 0;
uint64_t g_int3_interrupt_overhead = 0;
uint64_t g_straddler_count = 0;
uint64_t g_int3_interrupt_count = 0;

#ifdef PROBE_HIST_ON
ProbeStatistics* g_probe_stats; 
uint64_t* g_epilog_timings; 
uint64_t* g_prolog_timings; 
uint64_t* g_probe_timings; 

uint64_t g_epilog_count = 0;
double g_epilog_mean = 0;
double g_epilog_variance = 0;

uint64_t g_prolog_count = 0;
double g_prolog_mean = 0;
double g_prolog_variance = 0;

uint64_t g_total_probe_count = 0;
double g_probe_mean = 0;
double g_probe_variance = 0;

int g_num_bins;
#endif

#ifdef PROBE_TRUE_EMPTY_ON
ProbeStatistics* g_probe_stats; 
uint64_t* g_epilog_timings; 
uint64_t* g_prolog_timings; 
uint64_t* g_probe_timings; 
int g_num_bins;
#endif

static void empty_int3_handler(int signo, siginfo_t *inf, void* ptr) {
  return;
}

static void int3_handler(int signo, siginfo_t *inf, void* ptr) {

  ticks start = getticks();
  ucontext_t *ucontext = (ucontext_t*)ptr;

  // Resuming the thread after skipping the call instruction.
  ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)ucontext->uc_mcontext.gregs[REG_RIP] + 4;

  g_int3_interrupt_count++;
  ticks end = getticks();
  g_finstrumentor_overhead += (g_int3_interrupt_overhead + end - start);

}

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

  fprintf(stderr, "[Finstrumentor] Initializing the instrumentor\n");

  //setting up global data structures
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

  g_straddlers_bitmap = new uint8_t[func_count/8 + 1](); 

  // Set signal handling overhead using empty signal handler
  struct sigaction newact; 
  struct sigaction oldact; 
  memset( &newact, 0, sizeof newact);
  newact.sa_sigaction = & empty_int3_handler;
  newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (newact.sa_mask));

  sigaction(SIGTRAP, &newact, &oldact);

  ticks start;
  ticks end;
  int N_TIMES = 3;
  for (int i=0; i < N_TIMES; i++) {
    start = getticks();
    __asm__ ("int $0x03" : : );
    end = getticks();
    g_int3_interrupt_overhead += (end - start);
  }

  g_int3_interrupt_overhead = g_int3_interrupt_overhead / N_TIMES;

  fprintf(stderr, "[Finstrumentor] INT3 Overhead : %lu\n", g_int3_interrupt_overhead);

  // Set real signal handler
  memset( &newact, 0, sizeof newact);
  newact.sa_sigaction = & int3_handler;
  newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (newact.sa_mask));

  sigaction(SIGTRAP, &newact, &oldact);

  // DEBUG LOG ONLY
  // fprintf(stderr, "Sigaction set, old funptr %p\n", (void*)oldact.sa_handler);

  // __asm__ ("nop, nop, nop, nop" : :);

}

/*
 * Parameters :
 * probe_id = This is the address of the function
 * flag = Specifies which probes within the probe site to be disabled. Currently 
 *        defaults to function start and end
 */
bool Finstrumentor::activateFunction(string name) {

  uint16_t func_id = getFunctionId(name);
  /*
  uint64_t func_addr = (uint64_t)probe_id;
  uint64_t func_id; 
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    func_id =  func_addr_mappings->find(func_addr)->second->func_id;
  } else {
    return -1;
  }
  */

  return activateFunction(func_id);

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
bool Finstrumentor::activateFunction(uint16_t func_id) {

  // uint16_t func_id = *(uint16_t*)probe_id;
  uint64_t func_addr; 
  uint64_t* lock = NULL;

  // BJS: Does this mean Straddlers are not profiled ?
  
  // straddler check
  if (get_index(g_straddlers_bitmap, func_id)) {
    // fprintf(stderr, "Skip activating the straddler function %d\n", func_id);
    return false;
  }

  if (func_id_mappings->find(func_id) != func_id_mappings->end()) {
    func_addr =  func_id_mappings->find(func_id)->second->func_addr;
    lock = &(func_addr_mappings->find(func_addr)->second->lock);
  } else {
    return false;
  }

  if (lock!= NULL) {
    if (__sync_bool_compare_and_swap(lock, 0, 1)) {
      std::vector<FinsProbeInfo*>* ls = probe_info->find(func_addr)->second;  
      for (std::vector<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
        FinsProbeInfo* info = *it;

        if (info->isActive) {
          continue;
        }

        if (info->straddler) {
 
           __sync_val_compare_and_swap((uint64_t*) info->straddle_part_1_start,
                  *((uint64_t*)info->straddle_part_1_start), info->straddle_int3_sequence);
          for(long i = 0; i < 1000; i++) { asm(""); }
          // __sync_synchronize(); 
          // clflush(info->straddle_part_1_start);
          __sync_val_compare_and_swap((uint64_t*) info->straddle_part_2_start,
                  *((uint64_t*)info->straddle_part_2_start), info->activation_sequence_2);
          __sync_val_compare_and_swap((uint64_t*) info->straddle_part_1_start,
                  *((uint64_t*)info->straddle_part_1_start), info->activation_sequence_1);
        } else {
	        __sync_val_compare_and_swap((uint64_t*) info->probeStartAddr,
                  *((uint64_t*)info->probeStartAddr), info->activeSequence);
        }

        info->isActive = true;
      }
    } else {
      return false;
    }   
  } else {
    return false;
  }

  __sync_bool_compare_and_swap(lock, 1 , 0);
  return true;

}

/*
 * Parameters :
 * probe_id = This is the address of the function
 * flag = Specifies which probes within the probe site to be disabled. Currently 
 *        defaults to function start and end
 */
bool Finstrumentor::deactivateFunction(string name) {

  uint16_t func_id = getFunctionId(name);
  /*
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    func_id =  func_addr_mappings->find(func_addr)->second->func_id;
  } else {
    return false;
  }
  */

  return deactivateFunction(func_id);

}

std::vector<FinsProbeInfo*>* Finstrumentor::getProbes(uint16_t func_id) {
  // uint16_t func_id = *(uint16_t*)probe_id;
  uint64_t func_addr;
  
  // BJS: something is strange here. 
 
  if (func_id_mappings->find(func_id) != func_id_mappings->end()) { 
    func_addr =  func_id_mappings->find(func_id)->second->func_addr;
  } else {
    return NULL;
  }

  if (probe_info->find(func_addr) == probe_info->end()) {
    return NULL;
  }

  std::vector<FinsProbeInfo*>* ls = probe_info->find(func_addr)->second;  
  return ls;
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
bool Finstrumentor::deactivateFunction(uint16_t func_id) {

  // uint16_t func_id = *(uint16_t*)probe_id;
  uint64_t func_addr;
  uint64_t* lock = NULL;

  // straddler check
  if (get_index(g_straddlers_bitmap, func_id)) {
    return false;
  }

  if (func_id_mappings->find(func_id) != func_id_mappings->end()) { 
    func_addr =  func_id_mappings->find(func_id)->second->func_addr;
    lock = &(func_addr_mappings->find(func_addr)->second->lock);
  } else {
    return false;
  }

  std::vector<FinsProbeInfo*>* ls = probe_info->find(func_addr)->second;  

  if (ls == NULL) {
    return false;
  }

  if (lock != NULL) {
    if (__sync_bool_compare_and_swap(lock, 0, 1)) {
      for (std::vector<FinsProbeInfo*>::iterator it = ls->begin(); it != ls->end(); it++) {
        FinsProbeInfo* info = *it;

        if (!info->isActive) {
          // fprintf(stderr, "Escaping the probe for func : %d\n", func_id);
          continue;
        }

        if (info->straddler) {
          __sync_val_compare_and_swap((uint64_t*) info->straddle_part_1_start,
                  *((uint64_t*)info->straddle_part_1_start), info->straddle_int3_sequence);
          for(long i = 0; i < 1000; i++) { asm(""); }
          // __sync_synchronize(); 
          // clflush(info->straddle_part_1_start);
          __sync_val_compare_and_swap((uint64_t*) info->straddle_part_2_start,
                  *((uint64_t*)info->straddle_part_2_start), info->deactivation_sequence_2);
          __sync_val_compare_and_swap((uint64_t*) info->straddle_part_1_start,
                  *((uint64_t*)info->straddle_part_1_start), info->deactivation_sequence_1);
        } else {
      	  __sync_val_compare_and_swap((uint64_t*) info->probeStartAddr,
                  *((uint64_t*)info->probeStartAddr), info->deactiveSequence);
        }

        info->isActive = false;
      }
    } else {
      return false;
    }  
  } else {
    return false;
  }

  // fprintf(stderr, "Deactivated probe count: %d\n", count);
  __sync_bool_compare_and_swap(lock, 1 , 0);

  return true;

}

bool Finstrumentor::hasProbeInfo(uint64_t func_addr) {

  if (probe_info->find(func_addr) == probe_info->end()) {
    return false;
  } else {
    return true;
  }
}

FinsProbeInfo* Finstrumentor::getProbeInfo(uint64_t func_addr, uint8_t* addr) {
  if(probe_info->find(func_addr) != probe_info->end()) {
    std::vector<FinsProbeInfo*>* probe_list = probe_info->find(func_addr)->second;
    for(std::vector<FinsProbeInfo*>::iterator iter = probe_list->begin(); 
      iter != probe_list->end(); iter++) {
      FinsProbeInfo* probeInfo= *iter;
      if (probeInfo->probeStartAddr == addr-5) {
        return probeInfo;
      }
    }
  }
  return NULL;
}

void Finstrumentor::addProbeInfo_(uint64_t func_addr, uint8_t* probe_addr, bool unpatched) {
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  if(probe_info->find(func_addr) == probe_info->end()) {
    std::vector<FinsProbeInfo*>* probe_list = new std::vector<FinsProbeInfo*>;
    probe_list->reserve(5);
    
    FinsProbeInfo* probeInfo = populateProbeInfo(probe_addr, unpatched);
    probe_list->push_back(probeInfo);
    probe_info->insert(make_pair(func_addr, probe_list));
  } else {
    std::vector<FinsProbeInfo*>* probe_list = probe_info->find(func_addr)->second;
    for(std::vector<FinsProbeInfo*>::iterator iter = probe_list->begin(); iter != probe_list->end(); iter++) {
      FinsProbeInfo* probeInfo= *iter;
      if (probeInfo->probeStartAddr == (probe_addr-8)) {
	return; // Probe already initialized. Nothing to do.
      }
    }
    
    FinsProbeInfo* probeInfo = populateProbeInfo(probe_addr, unpatched);
    probe_list->push_back(probeInfo);
  }
}


void Finstrumentor::addProbeInfo(uint64_t func_addr, uint8_t* probe_addr, bool unpatched) {

  uint64_t* lock = getLock(func_addr);
  int spin_counter = 0;
  if (lock != NULL) {
    if (__sync_bool_compare_and_swap(lock, 0 , 1)) {
      if(probe_info->find(func_addr) == probe_info->end()) {
        std::vector<FinsProbeInfo*>* probe_list = new std::vector<FinsProbeInfo*>;
        probe_list->reserve(5);
	
        FinsProbeInfo* probeInfo = populateProbeInfo(probe_addr, unpatched);
        probe_list->push_back(probeInfo);
        probe_info->insert(make_pair(func_addr, probe_list));
      } else {
        std::vector<FinsProbeInfo*>* probe_list = probe_info->find(func_addr)->second;
        for(std::vector<FinsProbeInfo*>::iterator iter = probe_list->begin(); iter != probe_list->end(); iter++) {
          FinsProbeInfo* probeInfo= *iter;
          if (probeInfo->probeStartAddr == (probe_addr-8)) {
            while(!__sync_bool_compare_and_swap(lock, 1 , 0));
            return; // Probe already initialized. Nothing to do.
          }
        }
	
        FinsProbeInfo* probeInfo = populateProbeInfo(probe_addr, unpatched);
        probe_list->push_back(probeInfo);
      }
    } else { // We failed. Wait until the other thread finish and just return
      
      //BJS: I dont understand this. 
#ifndef NDEBUG
      ticks t0 = getticks(); 
#endif 
      while (*lock) {
        if (spin_counter == INT_MAX) {
          fprintf(stderr, "RESETTING the counter\n");
          fprintf(stderr, "Returning without adding the probe since lock is : %lu\n", *lock);
          break;
        }
        spin_counter += 1;
      }
#ifndef NDEBUG
      pthread_t tid = pthread_self();
      ticks t1 = getticks();
      fprintf( stderr
	       , "[Finstrumentor::addProbeInfo] ThreadID: %lu was busy-waiting for %d iterations.\nWait took %lu ticks.\n"
	       , (unsigned long int)tid,spin_counter,(t1-t0));
      
#endif 
      return;
    }
  }
      
  while(!__sync_bool_compare_and_swap(lock, 1 , 0));      
}
      
 

/*
static void print_probe_info() {

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  // fprintf(stderr, "Map size : %d\n", ins->probe_info->size());
  for(auto iter = ins->probe_info->begin(); iter != ins->probe_info->end(); iter++) {
    std::list<FinsProbeInfo*>* probe_list = iter->second;
    /// / fprintf(stderr, "Function address : %p\n", iter->first);

    for (std::list<FinsProbeInfo*>::iterator it = probe_list->begin(); it!= probe_list->end(); it++) {
      FinsProbeInfo* probeData = *it;

      fprintf(stderr, "Probe start address : %p\n", probeData->probeStartAddr);
    }

  }
}
*/

uint16_t Finstrumentor::getFunctionId(uint64_t func_addr) {
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    return func_addr_mappings->find(func_addr)->second->func_id;
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

uint16_t Finstrumentor::getFunctionId(string name) {
  if (func_name_mappings->find(name) != func_name_mappings->end()) {
    return func_name_mappings->find(name)->second->func_id;
  } else {
   return 0;
  } 
}


// BJS: It is apparently not good to return 0 (NULL) 
//      as a string. 
//      This seems to be a case where icpc and g++ are differ in implementation
//     
//      If we need to check for an error case here, there is a Boost::Optional. 
//      (Like Haskell Maybe) 
string Finstrumentor::getFunctionName(uint16_t func_id) {
  if (func_id_mappings->find(func_id) != func_id_mappings->end()) {
    return func_id_mappings->find(func_id)->second->func_name;
  } else {
    return "NO_NAME_MAPPING";
  }
}

uint64_t* Finstrumentor::getLock(uint64_t func_addr) {
  if (func_addr_mappings->find(func_addr) != func_addr_mappings->end()) {
    return &(func_addr_mappings->find(func_addr)->second->lock);
  } else {
   return NULL;
  } 
}

/*
string Finstrumentor::getFunctionName(uint16_t func_id) {
  if (func_id_mappings->find(func_id) != func_id_mappings->end()) {
    return func_id_mappings->find(func_id)->second->func_name;
  } else {
    return NULL;
  }
}
*/


// void printMap(FuncIDMappings* func_id_mappings) {
//  for (std::map<uint16_t,FunctionInfo*>::iterator it=func_id_mappings->begin(); it!=func_id_mappings->end(); ++it)
//       std::cout << it->first << " => " << it->second->func_name << '\n'; 
// }


void Finstrumentor::addFunction(uint64_t addr,string name) {
  FunctionInfo<FinsProbeInfo>* func_info = new FunctionInfo<FinsProbeInfo>;
  func_info->func_addr = addr;
  func_info->func_name = name;
  func_info->func_id = func_count-1;
  func_info->lock = 0;

  func_addr_mappings->insert(make_pair(func_info->func_addr, func_info));
  func_id_mappings->insert(make_pair(func_info->func_id, func_info));
  func_name_mappings->insert(make_pair(func_info->func_name, func_info));

}

void Finstrumentor::readFunctionInfo() {

  fprintf(stderr, "Initializing mappings data structure..\n");
  func_addr_mappings = new FuncAddrMappings<FinsProbeInfo>;
  func_id_mappings = new FuncIDMappings<FinsProbeInfo>;
  func_name_mappings = new FuncNameMappings<FinsProbeInfo>;

  string line;
  ifstream fp ("functions.txt");
  std::string::size_type sz = 16;

  func_count = 1; // Starts with 1 so that id 0 can be used to signal errors
  if (fp.is_open()) {
    while (getline (fp,line)){
      string token;
      istringstream iss(line);

      vector<string> tokens;
      int num_tokens = tokenize(line, tokens, ",");
      if (num_tokens < 2) {
        continue;
      }

      FunctionInfo<FinsProbeInfo>* func_info = new FunctionInfo<FinsProbeInfo>;
      func_info->func_addr = (uint64_t) strtoul(tokens[0].c_str(), NULL, 16);
      func_info->func_name = tokens[1];
      //fprintf(stderr,"adding func: %s \n",tokens[1].c_str());
      func_info->func_id = func_count++;
      func_info->lock = 0;

      func_addr_mappings->insert(make_pair(func_info->func_addr, func_info));
      func_id_mappings->insert(make_pair(func_info->func_id, func_info));
      func_name_mappings->insert(make_pair(func_info->func_name, func_info));
    }
    func_count++; // Hack to include calibrate_cache_effects
    //printMap(func_id_mappings);
    fp.close();
  }

  if (func_count == 1) {
    fprintf(stderr, "[Ubiprof] ERROR : functions.txt not present. Ubiprof will not profile this application...\n");
  }
}



FinsProbeInfo* populateProbeInfo(uint8_t* probe_addr, bool unpatched){
  uint64_t* probe_start = (uint64_t*)(probe_addr - 5);

  FinsProbeInfo* probeInfo = new FinsProbeInfo;
  probeInfo->probeStartAddr = probe_addr-5;
  probeInfo->isActive = 1;
  probeInfo->unpatched = unpatched;

  uint64_t sequence = *probe_start;
  uint64_t mask = 0xFFFFFF0000000000;
  uint64_t deactive = (uint64_t) (sequence & mask); 
  mask = 0x0000000000441F0F; // Mask with a 5 byte NOP

  //uint64_t activeSequence = sequence;
  uint64_t deactiveSequence = deactive | mask;

  size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
  int offset = (uint64_t) probe_start % cache_line_size;
  if (offset >= 57) { // If this is a cache line straddler
    int cutoff_point = 64 - offset;

    g_straddler_count++;
    // printf("CUTOFF POINT : %d\n", cutoff_point);

    probeInfo->straddler = true;
   
    uint64_t* straddle_point = (uint64_t*)((uint8_t*)probe_start + cutoff_point);
    probeInfo->straddle_part_1_start = straddle_point - 1;
    probeInfo->straddle_part_2_start = straddle_point;
    probeInfo->activation_sequence_1 = *(probeInfo->straddle_part_1_start); 
    probeInfo->activation_sequence_2 = *(probeInfo->straddle_part_2_start); 

    int shift_size = 8 * (8 - cutoff_point);
    uint64_t int3mask = 0xCC;
    uint64_t ormask = 0xFF;

    int3mask = (int3mask << shift_size);
    ormask = (ormask << shift_size);

    probeInfo->straddle_int3_sequence = (*(probeInfo->straddle_part_1_start) & ~ormask) | int3mask;
      
    uint64_t temp0 = deactiveSequence & get_lsb_mask(cutoff_point);
    shift_size = (8 * (8-cutoff_point)); 
    temp0 = (temp0 << shift_size);
    uint64_t temp1 = probeInfo->activation_sequence_1 & (~get_msb_mask(cutoff_point));
    probeInfo->deactivation_sequence_1 = temp0 | temp1;

    temp0 = deactiveSequence & (~get_lsb_mask(cutoff_point));
    shift_size = (8 * cutoff_point);
    temp0 = (temp0 >> shift_size);
    temp1 = probeInfo->activation_sequence_2 & get_msb_mask(cutoff_point); 
    probeInfo->deactivation_sequence_2 = temp0 | temp1;
  } else {
    probeInfo->straddler = false;
  }

  probeInfo->activeSequence = sequence;
  probeInfo->deactiveSequence = deactiveSequence;

  return probeInfo;

}

#ifdef PROBE_HIST_ON 
void dumpProbeOverheadStatistics() {

  FILE* fp = fopen("probe_overhead.out", "a");

  fprintf(fp, "\n>>>>>\n");
  fprintf(fp, "PROLOG_MEAN: %.2lf\n", g_prolog_mean);
  fprintf(fp, "PROLOG_STD: %.2lf\n", sqrt(g_prolog_variance / g_prolog_count));
  fprintf(fp, "EPILOG_MEAN: %.2lf\n", g_epilog_mean);
  fprintf(fp, "EPILOG_STD: %.2lf\n", sqrt(g_epilog_variance / g_epilog_count));
  fprintf(fp, "PROBE_MEAN: %.2lf\n", g_probe_mean);
  fprintf(fp, "PROBE_STD: %.2lf\n", sqrt(g_probe_variance / g_total_probe_count));

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
  fclose(fp);

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
  fclose(fp);

}
#endif

uint64_t Finstrumentor::getInstrumentorBackgroundOverhead() {
  return g_finstrumentor_overhead;
}

Instrumentor* Finstrumentor::configureInstance(InstrumentationFunc prolog,
        InstrumentationFunc epilog) {
  this->prologFunc = prolog; 
  this->epilogFunc = epilog; 
  prologFunction = prolog;
  epilogFunction = epilog;

  return this;
 }


Finstrumentor::~Finstrumentor() {
  fprintf(stderr, "[Finstrumentor] NUM_ACCESSED_PROBE_SITES: %lu\n", ((Finstrumentor*) INSTRUMENTOR_INSTANCE)->probe_info->size()); 
  fprintf(stderr, "[Finstrumentor] NUM_INTERRUPTS : %lu\n", g_int3_interrupt_count);

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
  fprintf(stderr,"Finstrumentor destroyed!\n");
}
