
#include "liteinst.hpp"
#include "process.hpp"
#include <assert.h>
#include <pthread.h>
#include <time.h>

using namespace liteinst;
using namespace utils::process;

int64_t counter = 0;

struct ProfileData {
  int64_t count;
  int64_t last_snapshot;
  bool active;
};

ProfileData* stats;
ProbeProvider* p;
unsigned int num_funcs;

void* monitor(void* param) {

  struct timespec ts;
  while (true) {
    for (int i=0; i < num_funcs; i++) {
      if(!stats[i].active) {
        // printf("Activating probe group : %d\n", i);
        ProbeGroupInfo pgi(i);
        p->activate(pgi);
      }
    }

    ts.tv_sec = 0;
    ts.tv_nsec = 100000000;
    nanosleep(&ts, NULL);
  }
}

__attribute__((destructor))
void tear_down() {
  for (int i=0; i < num_funcs; i++) {
    if (stats[i].count > 0) {
      printf("ProbeGroupId : %lu\n", i);
      printf("Count : %ld\n", stats[i].count);
    }
  }
}

void entryInstrumentation() {
  ProbeInfo pi;
  LITEINST_SET_PROBE_INFO(pi);

  // printf("Entry..\n");
  /*
  printf("Probe Group ID : %lu\n", pi.ctx.pg_id);
  printf("Probe ID : %lu\n", pi.ctx.p_id);
  printf("Instrumentation ID : %d\n", pi.ctx.i_id);
  printf("Probe Placement : %d\n", pi.ctx.placement);
  printf("Register state : %p\n", pi.ctx.u_regs);
  printf("Address : %p\n", pi.address);
  */

  stats[pi.ctx.pg_id].count++;
  return;
}

void exitInstrumentation() {
  ProbeInfo pi;
  LITEINST_SET_PROBE_INFO(pi);

  // printf("Exit..\n");
  /*
  printf("Probe Group ID : %lu\n", pi.ctx.pg_id);
  printf("Probe ID : %lu\n", pi.ctx.p_id);
  printf("Instrumentation ID : %d\n", pi.ctx.i_id);
  printf("Probe Placement : %d\n", pi.ctx.placement);
  printf("Register state : %p\n", pi.ctx.u_regs);
  printf("Address : %p\n", pi.address);
  */
  int64_t current_count = stats[pi.ctx.pg_id].count;
  int64_t last_snapshot = stats[pi.ctx.pg_id].last_snapshot;
  if (current_count - last_snapshot > 1000) {
    // printf("Deactivating probe group : %lu\n", pi.ctx.pg_id);

    ProbeGroupInfo pgi(pi.ctx.pg_id);
    bool deactivated = p->deactivate(pgi);
    if (deactivated) {
      stats[pi.ctx.pg_id].last_snapshot = stats[pi.ctx.pg_id].count;
      stats[pi.ctx.pg_id].active = false;
    }
  }
}

void initCallback() {
  printf("At init call back..\n");
  p = liteinst::ProbeProvider::getGlobalProbeProvider();

  InstrumentationProvider i_provider("Sampling", entryInstrumentation, 
      exitInstrumentation);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(liteinst::Function("*~_ZnwmPv"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "Sampling"); 

  printf("Registered probes..\n");

  Process process;

  num_funcs = pr.getProbedFunctions().size();
  // assert(pr.getProbedFunctions().size() == num_funcs);

  stats = new ProfileData[num_funcs]();

  for (int i=0; i < num_funcs; i++) {
    stats[i].active = true;
  }

  printf("Stats at : %p\n", stats);

  for (const auto& it : pr.pg_by_function) {
    printf("Deactivating function : %s\n", it.first.c_str());

    for (ProbeGroupInfo pgi : it.second) {
      p->deactivate(pgi);
    }
  }

  /*
  pthread_t tr;
  pthread_create(&tr, NULL, monitor, (void*)NULL);
  */
}

__attribute__((constructor))
void initProfiler() {
  ProbeProvider* p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, initCallback);
}
