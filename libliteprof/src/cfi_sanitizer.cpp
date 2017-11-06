
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
  NOPROF,
  EMPTY
};

ProfilerType g_liteprof_type = SAMPLING;
int64_t g_liteprof_sample_sz = 10;

/* 0x4006a6 */
uint8_t g_probe[] = { 0x49, 0xc7, 0xc2, 0xa6, 0x06, 0x40, 0x00, /* mov $val,%r10 */ 
                      0x4c, 0x39, 0xd0, /* cmp %r10,%rax */
                      0x74, 0x02,       /* je +2 */ 
                      0xc9,             /* pop %rbp */
                      0xc3,             /* retq     */
                    };
uint8_t g_probe_size = sizeof(g_probe) / sizeof(uint8_t);

struct ProfileData {
  int64_t count;
  int64_t last_snapshot;
  bool active;
};

ProfileData* g_liteprof_stats;
ProbeProvider* g_liteprof_p;
unsigned int g_liteprof_num_funcs;
pthread_t* g_monitor_thread;;

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

void emptyEntry() {

}

void emptyExit() {
  printf("EMPTY\n");
}

void entryInstrumentation() {
  printf("PSSST\n\n\n");
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
  /*
  int64_t current_count = g_liteprof_stats[pi.ctx.pg_id].count;
  int64_t last_snapshot = g_liteprof_stats[pi.ctx.pg_id].last_snapshot;
  if (current_count - last_snapshot > g_liteprof_sample_sz) {
    // printf("Deactivating probe group : %lu\n", pi.ctx.pg_id);

    ProbeGroupInfo pgi(pi.ctx.pg_id);
    bool deactivated = g_liteprof_p->deactivate(pgi);
    if (deactivated) {
      g_liteprof_stats[pi.ctx.pg_id].last_snapshot =
        g_liteprof_stats[pi.ctx.pg_id].count;
      g_liteprof_stats[pi.ctx.pg_id].active = false;
    } else {
      printf("Failed deactivating probe group : %lu\n", pi.ctx.pg_id);
    }
  }
  */
}

void initCallback() {
  printf("At init call back..\n");

  if (const char* prof_type = std::getenv("PROF_TYPE")) {
    if (!strcmp(prof_type, "BACKOFF")) {
      g_liteprof_type = BACKOFF;
    } else if (!strcmp(prof_type, "SAMPLING")) {
      g_liteprof_type = SAMPLING;
    } else if (!strcmp(prof_type, "NOPROF")) {
      g_liteprof_type = NOPROF;
    } else if (!strcmp(prof_type, "EMPTY")) {
      g_liteprof_type = EMPTY;
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
  } else if (g_liteprof_type == EMPTY) {
    printf("[liteprof] Running EMPTY profiler\n");
  }

  if (g_liteprof_type == EMPTY) {
    /*
    InstrumentationProvider i_provider("IProvider", emptyEntry, 
      emptyExit); */
    InstrumentationProvider i_provider("IProvider", g_probe, g_probe_size);
    g_liteprof_p->registerInstrumentationProvider(i_provider);
    printf("Registered probe provider with probe_size : %d\n", g_probe_size);
  } else {
    /*
    InstrumentationProvider i_provider("IProvider", entryInstrumentation, 
      exitInstrumentation); */
    InstrumentationProvider i_provider("IProvider", g_probe, g_probe_size);
    g_liteprof_p->registerInstrumentationProvider(i_provider);
    printf("Registered probe provider with probe_size : %d\n", g_probe_size);
  }

  Coordinates coords;
  coords.setAddress(liteinst::VMAddress("0x00000000004007a8"));
  // coords.setFunction(liteinst::Function(".*~_ZnwmPv"));
  // coords.setBasicBlock(liteinst::BasicBlock("*"));
  coords.setProbePlacement(ProbePlacement::ENTRY);

  ProbeRegistration pr = g_liteprof_p->registerProbes(coords, "IProvider"); 

  printf("Registered probes..\n");

  if (g_liteprof_type == NOPROF) {
    for (const auto& it : pr.pg_by_function) {
      // printf("Deactivating function : %s\n", it.first.c_str());

      for (ProbeGroupInfo pgi : it.second) {
        g_liteprof_p->deactivate(pgi);
      }
    }  
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

  g_monitor_thread = (pthread_t*) calloc(1, sizeof(pthread_t));
  if (g_liteprof_type == SAMPLING) {
    pthread_create(g_monitor_thread, NULL, monitor, (void*)NULL);
    pthread_detach(*g_monitor_thread);
  }
}

__attribute__((constructor))
void initProfiler() {
  printf("Registering Profiler...\n");
  g_liteprof_p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, initCallback);
}

__attribute__((destructor))
void tearDownProfiler() {
  printf("Tearing down..\n");
  // free(g_monitor_thread);
}
