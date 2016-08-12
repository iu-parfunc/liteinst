
#include "liteinst.hpp"
#include "process.hpp"
#include <assert.h>
#include <pthread.h>
#include <time.h>
#include <string.h>
#include <cstdlib>

using namespace liteinst;
using namespace utils::process;

enum ProfilerType {
  SAMPLING,
  BACKOFF,
  NOPROF
};

ProfilerType g_liteprof_type = BACKOFF;
int64_t g_liteprof_sample_sz = 1000;

struct ProfileData {
  int64_t count;
  int64_t last_snapshot;
  bool active;
};

ProfileData* g_liteprof_stats;
ProbeProvider* g_liteprof_p;
unsigned int g_liteprof_num_funcs;

void* monitor(void* param) {

  struct timespec ts;
  while (true) {
    for (int i=0; i < g_liteprof_num_funcs; i++) {
      if(!g_liteprof_stats[i].active) {
        // printf("Activating probe group : %d\n", i);
        ProbeGroupInfo pgi(i);
        g_liteprof_p->activate(pgi);
      }
    }

    ts.tv_sec = 0;
    ts.tv_nsec = 100000000;
    nanosleep(&ts, NULL);
  }
}

__attribute__((destructor))
void tear_down() {
  for (int i=0; i < g_liteprof_num_funcs; i++) {
    if (g_liteprof_stats[i].count > 0) {
      printf("ProbeGroupId : %lu\n", i);
      printf("Count : %ld\n", g_liteprof_stats[i].count);
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

  g_liteprof_stats[pi.ctx.pg_id].count++;
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
  int64_t current_count = g_liteprof_stats[pi.ctx.pg_id].count;
  int64_t last_snapshot = g_liteprof_stats[pi.ctx.pg_id].last_snapshot;
  if (current_count - last_snapshot > g_liteprof_sample_sz) {
    printf("Deactivating probe group : %lu\n", pi.ctx.pg_id);

    ProbeGroupInfo pgi(pi.ctx.pg_id);
    bool deactivated = g_liteprof_p->deactivate(pgi);
    if (deactivated) {
      g_liteprof_stats[pi.ctx.pg_id].last_snapshot =
        g_liteprof_stats[pi.ctx.pg_id].count;
      g_liteprof_stats[pi.ctx.pg_id].active = false;
    }
  }
}

void initCallback() {
  printf("At init call back..\n");

  InstrumentationProvider i_provider("Sampling", entryInstrumentation, 
      exitInstrumentation);

  g_liteprof_p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(liteinst::Function("*~_ZnwmPv"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = g_liteprof_p->registerProbes(coords, "Sampling"); 

  printf("Registered probes..\n");

  if (g_liteprof_type == NOPROF) {
    g_liteprof_p->deactivate(pr);
    printf("Deactivated all probes..\n");
  }

  // Process process;

  g_liteprof_num_funcs = pr.getProbedFunctions().size();
  // assert(pr.getProbedFunctions().size() == num_funcs);

  g_liteprof_stats = new ProfileData[g_liteprof_num_funcs]();

  for (int i=0; i < g_liteprof_num_funcs; i++) {
    g_liteprof_stats[i].active = true;
    g_liteprof_stats[i].count = 0;
  }

  printf("Stats at : %p\n", g_liteprof_stats);

  if (g_liteprof_type == SAMPLING) {
    pthread_t tr;
    pthread_create(&tr, NULL, monitor, (void*)NULL);
  }
}

__attribute__((constructor))
void initProfiler() {

  if (const char* prof_type = std::getenv("PROF_TYPE")) {
    if (!strcmp(prof_type, "BACKOFF")) {
      g_liteprof_type = BACKOFF;
    } else if (!strcmp(prof_type, "SAMPLING")) {
      g_liteprof_type = SAMPLING;
    } else if (!strcmp(prof_type, "NOPROF")) {
      g_liteprof_type = NOPROF;
    } else {
      g_liteprof_type = NOPROF;
    }
  }

  if (const char* sample_sz = std::getenv("SAMPLE_SIZE")) {
    g_liteprof_sample_sz = atol(sample_sz);
    printf("[liteprof] Sample size : %ld\n", g_liteprof_sample_sz);
  } 

  if (g_liteprof_type == BACKOFF) {
    printf("[liteprof] Running BACKOFF profiler\n");
    printf("[liteprof] Sample size : %ld\n", g_liteprof_sample_sz);
  } else if (g_liteprof_type == SAMPLING) {
    printf("[liteprof] Running SAMPLING profiler\n");
    printf("[liteprof] Sample size : %ld\n", g_liteprof_sample_sz);
  } else if (g_liteprof_type == NOPROF) {
    printf("[liteprof] Running NOPORF profiler\n");
  }

  g_liteprof_p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, initCallback);
}
