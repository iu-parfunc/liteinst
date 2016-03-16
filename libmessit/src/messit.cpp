
#include <cstdio>
#include <cstdint>
#include <time.h>
#include <pthread.h>
#include <cstdbool>

#include <sstream>
#include <fstream>
#include <iostream>
#include <vector>

#include "messit.hpp"
#include "utils.hpp"

using namespace std;

uint64_t wait_time = 10;
uint64_t sleep_time = 10;
uint64_t duration = 100;

uint64_t frequency = 1000;

int num_probes;
Probe* probes;
FuncAddrMapping* func_addr_mappings;

void sleepit();
void waitit();

perturbation_fn fns[] = {sleepit, waitit};

void sleepit() {
  fprintf(stderr, "[Sleep] Inside sleepit..\n");
  struct timespec t;
  t.tv_sec = 0;
  t.tv_nsec = sleep_time*1000; 
  clock_nanosleep(CLOCK_REALTIME, 0, &t, NULL);
}

void waitit() {
  fprintf(stderr, "[Wait] Inside waitit..\n");
  struct timespec start, end;
  clock_gettime(CLOCK_REALTIME, &start);

  while(1) {
    for (int i=0; i<10000; i++) {
      i=i+1;
    }
    clock_gettime(CLOCK_REALTIME, &end);
    uint64_t diff = (uint64_t)end.tv_sec * 1000000LL + (uint64_t)end.tv_nsec / 1000LL
      - (uint64_t)start.tv_sec * 1000000LL + (uint64_t)start.tv_nsec / 1000LL;
    if (diff > wait_time) {
      break;
    }
  }
}

void perturbation_func() {
  for (int i=0; i < 2; i++){
    fns[i]();
  }
}

void* perturber(void* param) {

  bool patched = false;
  struct timespec t;
  while(1) {
    if (!patched) {
    // Activate perturation
    fprintf(stderr, "[Perturber] Running perturber thread..\n");
    for (int i=0; i<num_probes; i++) {
      Probe p = probes[i];
      init_patch_site((void*) p.ins.addr, 8);
      fprintf(stderr, "[Patching] Probe address : %p Patched : %p..\n", 
          p.ins.addr, p.ins.patched);
      patch_64((void*)p.ins.addr, p.ins.patched);  
    }
    t.tv_sec = 0;
    t.tv_nsec = duration*1000;
    clock_nanosleep(CLOCK_REALTIME, 0, &t, NULL);

    // Deactivate perturbation
    for (int i=0; i<num_probes; i++) {
      Probe p = probes[i];
      init_patch_site((void*) p.ins.addr, 8);
      fprintf(stderr, "[Restoring] Probe address : %p Original : %p..\n", 
          p.ins.addr, p.ins.original);
      patch_64((void*)p.ins.addr, p.ins.original);  
    }

    t.tv_sec = 0;
    t.tv_nsec = (frequency-duration)*1000;
    clock_nanosleep(CLOCK_REALTIME, 0, &t, NULL);
    }
  }

  return NULL;
}

__attribute__((constructor)) void messit(); 
void messit() {

  fprintf(stderr, "[Messit] Initializing messit..\n");

  string line;
  ifstream fp ("functions.txt");

  if (fp.fail()) {
    fprintf(stderr, "[Messit] ERROR : Failed opening "
        "functions.txt. Exiting program\n");
    throw -1;
  }

  // Read function information
  func_addr_mappings = new FuncAddrMapping;
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

      func_addr_mappings->insert(make_pair(func_name, func_addr));
    }

    fp.close();
  }

  // Read configuration
  char* perturbed_functions_str = getenv("PERTURBED_FUNCTIONS");

  // Setup probes
  if (perturbed_functions_str != NULL) {
    vector<string> tokens;
    int num_tokens = tokenize(string(perturbed_functions_str), tokens, ",");
    probes = new Probe[num_tokens]();

    for (int i=0; i < num_tokens; i++) {
      auto it = func_addr_mappings->find(tokens[i]);
      if (it != func_addr_mappings->end()) {
        Address fn_addr = it->second;
        probes[i] = setupProbe(fn_addr, (Address) perturbation_func);
        num_probes++;
      }
    }
  }

  pthread_t thr; 
  if (frequency > 0) {
    fprintf(stderr, "[Messit] Spawning the perturbation thread..\n");
    pthread_create(&thr, NULL, perturber, (void*)NULL);
  }
}
