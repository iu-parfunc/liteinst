#include "zca-toggle.hpp"
#include "elf-provider.h"
#include "profiler_types.hpp"
#include "dynaprof.h"
#include <stdio.h>
#include <pthread.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <string>
#include <string.h>
#include <vector>
#include <list>
#include <limits.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include "nbqueue.h"
#include "logger.h"
#include "cycle.h"

#define NO_BACKOFF 0
#define FIXED_BACKOFF 1
#define BOP_SIMPLE 2
#define BOP_ADVANCED 3
#define COUNT_ONLY 4
#define SAMPLING 5
#define EMPTY 6
#define EMPTY_PROLOG 7

using namespace std;

func_data* dyn_stats;

Profiler* prof;

list<int>* inactive_funcs;

volatile ticks epilog_overhead = 0;
volatile ticks prolog_overhead = 0;

volatile ticks epilog_overhead_1 = 0;
volatile ticks prolog_overhead_1 = 0;
volatile ticks basic_prolog_overhead = 0;

NBQueue* heavy_hitters;

__thread static unsigned long long leaf_counter;

static pthread_key_t key;
static pthread_once_t tls_init_flag = PTHREAD_ONCE_INIT;

int strategy = BOP_SIMPLE;
double target_overhead = 0.05; 
double overhead = 0.0;
long sample_size = 10000;


void prolog_func();
void sampling_epilog_func();
void count_only_prolog_func();
void empty_prolog_func();
void empty_epilog_func();

int transfer(long v) {
  
  int r = (v < 10) ? 0 : (v < 100) ? 1 : (v < 1000) ? 2 : (v < 10000) ? 3 : 
            (v < 100000) ? 4 : (v < 1000000) ? 5 : (v < 10000000) ? 6 : (v < 100000000) ? 7 : 
            (v < 1000000000) ? 8 : (v < 10000000000) ? 9 : 10; 

  return r;

}


void placement_delete(void *t) {

  FILE *out_file = fopen("thread.out", "w");
  fprintf(out_file, "[Other] Delete called on : %p\n", t);

  pid_t tid = syscall(SYS_gettid);

  fprintf(out_file, "[Other]Thread local stack is at : %p\n", (ts_stack*)pthread_getspecific(key));
  fprintf(out_file, "[Other]Current thread is : %d\n", tid);
   
  free(t);
  /*function_stats* f_stats = (function_stats*)t;
    for (auto iter = f_stats->begin(); iter != f_stats->end(); iter++) {
    prof_data* data = iter->second;
    if (data != NULL) {
  // printf("Data->count : %lu\n", data->count);
  free(data);
  }
  }*/

}

void create_key() {
  pthread_key_create(&key, placement_delete);
}

// Check this out later
//  http://stackoverflow.com/questions/2053029/how-exactly-does-attribute-constructor-work

void* probe_monitor_sampling(void* param) {

  list<int>::iterator it;
  while(true) {
    for (it=inactive_funcs->begin(); it!=inactive_funcs->end(); ++it){
      int func_id = *it;
      func_data* data = &dyn_stats[func_id];
      prof->start_profile(func_id, prolog_func, sampling_epilog_func);
      // data->newly_reactivated = true;
      data->last_activation = getticks();
      it = inactive_funcs->erase(it);
      // fprintf(stderr, "[Sampler] Reactivated function %d\n", func_id);
      // fprintf(stderr, "*****> Reactivated function %lu\n", func_id);
      break;
    } 

   nanosleep((struct timespec[]){{0, 50000000}}, NULL);
  }

  return NULL;

}

void* probe_monitor(void* param) {

  while (true) {
    int i;
    
    long total_invocations = 0L;
    for (i=0; i<function_count;i++) {
      total_invocations += dyn_stats[i].count;
    }

    double zcaOverhead = getZCAOverhead();
    long threadCPUTime = getThreadCPUTime();
    long processCPUTime = getProcessCPUTime();

    overhead = (double)(total_invocations * zcaOverhead + 
                              getInitOverhead() + threadCPUTime) /
                                 processCPUTime;  

    // double overhead = 0.04;

    //fprintf(stderr, "ZCA overhead is : %lf\n", zcaOverhead);
    fprintf(stderr, "\nTotal invocations up to now is : %lu\n", total_invocations);
    fprintf(stderr, "Thread cpu time up to now is : %lu\n", threadCPUTime);
    fprintf(stderr, "Process cpu time up to now is : %lu\n", processCPUTime);
    fprintf(stderr, "Overhead up to now is : %lf\n\n", overhead);

    fprintf(stderr, "OVERHEAD %.03f\n", overhead);

    if (target_overhead > overhead) {
      if (target_overhead * 0.50 > overhead) {
        int current_time = getticks();
        list<int>::iterator it;

        for (it=inactive_funcs->begin(); it!=inactive_funcs->end(); ++it){
          int func_id = *it;
          func_data* data = &dyn_stats[func_id];
          if (current_time - data->last_deactivation > 1000) {
            prof->start_profile(func_id, NULL, NULL);
            // data->newly_reactivated = true;
            data->last_activation = gettime_millis();
            it = inactive_funcs->erase(it);
            LOG_INFO("*****> Reactivated function %d\n", func_id);
            // fprintf(stderr, "*****> Reactivated function %lu\n", func_id);
            break;
          }
        } 
      } else {
        int current_time = getticks();
        list<int>::iterator it;

        for (it=inactive_funcs->begin(); it!=inactive_funcs->end(); ++it){
          int func_id = *it;
          func_data* data = &dyn_stats[func_id];
          if (current_time - data->last_deactivation > 1000) {
            prof->start_profile(func_id, NULL, NULL);
            // data->newly_reactivated = true;
            data->last_activation = gettime_millis();
            it = inactive_funcs->erase(it);
            LOG_INFO("*****> Reactivated function %d\n", func_id);
            // fprintf(stderr, "*****> Reactivated function %lu\n", func_id);
            break;
          }
        }   
      } 
    } else if (target_overhead <  overhead) {
      if (overhead / target_overhead > 1.5) {
        while (!heavy_hitters->empty()) {
          long func_id = heavy_hitters->dequeue();
          if (func_id  != -1) {
            //prof->start_profile(func_id,basic_prolog_func, NULL);
            prof->stop_profile(func_id);
            dyn_stats[func_id].deactivation_count++;
            dyn_stats[func_id].last_deactivation = getticks();
            dyn_stats[func_id].last_count = dyn_stats[func_id].count;
            inactive_funcs->push_back(func_id);
            LOG_INFO("=====> Deactivated function %lu\n", func_id);
            // fprintf(stderr, "=====> Deactivated function %lu\n", func_id);
          }
        }
      } else {
        long func_id = heavy_hitters->dequeue();
        if (func_id  != -1) {
          // prof->start_profile(func_id,basic_prolog_func, NULL);
          prof->stop_profile(func_id);
          dyn_stats[func_id].deactivation_count++;
          dyn_stats[func_id].last_deactivation = getticks();
          dyn_stats[func_id].last_count = dyn_stats[func_id].count;
          inactive_funcs->push_back(func_id);
          LOG_INFO("----> Deactivated function %lu\n", func_id);
          // fprintf(stderr, "----> Deactivated function %lu\n", func_id);
        }
      }
    }

    sleep(1);

  }

/*
  while(!deactivation_queue->empty()) {

    int func_id = deactivation_queue->dequeue();
    if (func_id != -1) {
      fprintf(stderr,"Function id : %d\n", func_id);
      prof->stop_profile(func_id);
      stats[func_id].deactivation_count++;
      stats[func_id].last_deactivation = getticks();
      inactive_funcs->push_back(func_id);
      // add to deactivated_funcs linked listed
    }
  }

*/

  return NULL;
}


// __attribute__((constructor))
void Basic_Profiler::initialize(void) {


  char* overhead_str = getenv("DYN_OVERHEAD");
  if (overhead_str != NULL) {
    target_overhead = atof(overhead_str);
  }

  char* sample_size_str = getenv("DYN_SAMPLE_SIZE");
  if (sample_size_str != NULL) {
    sample_size = atol(sample_size_str);
  }

  char* strategy_str = getenv("DYN_STRATEGY");
  /*
  char no_backoff[] = "\"NO_BACKOFF\"";
  char fixed_backoff[] = "\"FIXED_BACKOFF\"";
  char bop_simple[] = "\"BOP_SIMPLE\"";
  char bop_advanced[] = "\"BOP_ADVANCED\"";
  char count_only[] = "COUNT_ONLY";
  */
  char no_backoff[] = "NO_BACKOFF";
  char fixed_backoff[] = "FIXED_BACKOFF";
  char bop_simple[] = "BOP_SIMPLE";
  char bop_advanced[] = "BOP_ADVANCED";
  char count_only[] = "COUNT_ONLY";
  char sampling[] = "SAMPLING";
  char empty[] = "EMPTY";
  char empty_prolog[] = "EMPTY_PROLOG";

  if (strategy_str != NULL) {
    if (strcmp(strategy_str, no_backoff) == 0) {
      strategy = NO_BACKOFF;
    } else if (strcmp(strategy_str, fixed_backoff) == 0) {
      strategy = FIXED_BACKOFF;
    } else if (strcmp(strategy_str, bop_simple) == 0) {
      strategy = BOP_SIMPLE;
    } else if (strcmp(strategy_str, bop_advanced) == 0) {
      strategy = BOP_ADVANCED;
    } else if (strcmp(strategy_str, count_only) == 0) {
      strategy = COUNT_ONLY;
    } else if (strcmp(strategy_str, sampling) == 0) {
      strategy = SAMPLING;
    } else if (strcmp(strategy_str, empty) == 0) {
      strategy = EMPTY;
    } else if (strcmp(strategy_str, empty_prolog) == 0) {
      strategy = EMPTY_PROLOG; 
    }
  }

  set_profiler_function();

  fprintf(stderr, "[Init] Strategy : %s Overhead : %s Sample size : %s\n", strategy_str, overhead_str, sample_size_str);
  fprintf(stderr, "[Init] Strategy : %d Overhead : %lf Sample size : %lu\n", strategy, target_overhead, sample_size);

  dyn_stats = (func_data*)malloc(sizeof(func_data) * function_count);

  typedef std::map<std::string, int>::iterator it_type;
  for(auto iterator = functions->begin(); iterator != functions->end(); iterator++) {
    int func_id = iterator->second;

    const char* func_name = (iterator->first).c_str();
    // string func_name = iterator->first;
    dyn_stats[func_id].func_name = strdup(func_name); // dup this and delete functions??
    dyn_stats[func_id].min = 0;
    dyn_stats[func_id].max = 0;
    dyn_stats[func_id].sum = 0;
    dyn_stats[func_id].count = 0;
    dyn_stats[func_id].deactivation_count = 0;
    dyn_stats[func_id].last_checkpoint_count = 0;
    // dyn_stats[func_id].last_activation_count = 0;
    dyn_stats[func_id].last_deactivation = 0;
    dyn_stats[func_id].last_activation = gettime_millis();
    // dyn_stats[func_id].last_checkpoint = getticks();
    dyn_stats[func_id].last_count = 0;
    dyn_stats[func_id].start = -1;
    dyn_stats[func_id].lock = 0;
    dyn_stats[func_id].var = 0.0;
    dyn_stats[func_id].avg = 0.0;
    dyn_stats[func_id].is_leaf= true;
    for (int j = 0; j < 11; j++) {
      dyn_stats[func_id].time_histogram[j]=0;
      dyn_stats[func_id].rate_histogram[j]=0;
    }
    // dyn_stats[func_id].time_histogram = new NBQueue(20);
    // We don't need function id here at the moment if required later add from functions iterator->second.
  }

  // Spawns the overhead monitor thread if we are following sophisticated back off strategies
  pthread_t tid; 
  if (strategy == BOP_SIMPLE || strategy == BOP_ADVANCED) {
    pthread_create(&tid, NULL, probe_monitor, (void*)NULL);
  } else if (strategy == SAMPLING) {
    pthread_create(&tid, NULL, probe_monitor_sampling, (void*)NULL);
  }

}

// __attribute__((destructor)) - This doesn't seem to work properly with our heap data being tampered with when this gets called
void cleanup(void) {

  int counter = 0;
  FILE *out_file = fopen("prof.out", "w");

  // pid_t tid = syscall(SYS_gettid);

  // fprintf(out_file, "[Main]Thread local stack is at : %p\n", (ts_stack*)pthread_getspecific(key));
  // fprintf(out_file, "[Main]Current thread is : %d\n", tid);

  // fprintf(stderr, "FINAL_OVERHEAD %.03f\n", overhead);

  long total_invocations = 0L;
  for (int i=0; i<function_count;i++) {
    total_invocations += dyn_stats[i].count;
  }
  fprintf(stderr, "\nTotal invocations : %lu\n", total_invocations);
  fprintf(stderr, "\nTicks per nano seconds : %lf\n", getTicksPerNanoSec());
  fprintf(stderr, "Total overhead from invocations (s): %lf\n", (total_invocations * 1200 / getTicksPerNanoSec()/1000000000));

  fprintf(out_file, "%-30s%-13s%-13s%-11s%-8s%-10s%-10s%-10s%-10s%-5s\n\n", "Function", "Rate_Hist", "Time_Hist",
      "Count", "Samples", "Min", "Max", "Avg", "Variance", "Leaf?");

  for(int i=0; i < function_count; i++) {
    if (dyn_stats[i].count != 0) {
      bool first_row = true;

      uint64_t max_hist_time = 0;
      int max_hist_time_idx = 0;
      uint64_t max_hist_rate = 0;
      int max_hist_rate_idx = 0;

      for (int j=0; j < 11; j++) {
        if (dyn_stats[i].time_histogram[j] > max_hist_time) {
          max_hist_time = dyn_stats[i].time_histogram[j];
          max_hist_time_idx = j;
        }

        if (dyn_stats[i].rate_histogram[j] > max_hist_rate) {
          max_hist_rate = dyn_stats[i].rate_histogram[j];
          max_hist_rate_idx = j;
        }
      }

      char buf[14], buf1[14];
      snprintf(buf, 13, "%d: %llu", max_hist_time_idx, max_hist_time);
      snprintf(buf1, 13, "%d: %llu", max_hist_rate_idx, max_hist_rate);
        
      double variance = 0;
      if (dyn_stats[i].count > 2) {
        variance = dyn_stats[i].var / (dyn_stats[i].count - 1);
      }
      double avg = dyn_stats[i].sum / dyn_stats[i].count;

      fprintf(out_file, "%-30s%-13s%-13s%-11llu%-8d%-10llu%-10llu%-10.1lf%-10.1lf%-5s\n", dyn_stats[i].func_name, buf1, buf, 
              dyn_stats[i].count, dyn_stats[i].deactivation_count+1, dyn_stats[i].min, dyn_stats[i].max, avg, 
              0.0, dyn_stats[i].is_leaf ? "TRUE" : "FALSE");

    }
  }

  fprintf(out_file, "\n\n-------- Histograms ------------\n");
  fprintf(out_file, "%-50s%-13s%-13s\n\n", "Function", "Rate_Hist", "Time_Hist");

  for(int i=0; i < function_count; i++) {
    bool first_row = true;
    for(int j=0; j < 11 ; j++) {
      char buf[14], buf1[14];
      snprintf(buf, 13, "%d: %llu", j, dyn_stats[i].time_histogram[j]);
      snprintf(buf1, 13, "%d: %llu", j, dyn_stats[i].rate_histogram[j]);

      if (first_row) {
        fprintf(out_file, "%-50s%-13s%-13s\n", dyn_stats[i].func_name, buf1, buf);
        first_row = false;
      } else {
        fprintf(out_file, "%-50s%-13s%-13s\n", " ", buf1, buf);
      }
    }
  }

  /*
  for(int i=0; i < function_count; i++) {
    if (dyn_stats[i].count != 0) {
      fprintf(out_file, "\nFunction : %s\n", dyn_stats[i].func_name);
      fprintf(out_file, "Leaf: %s\n", dyn_stats[i].is_leaf ? "TRUE" : "FALSE");

      fprintf(out_file, "Count : %lu\n", dyn_stats[i].count);
      fprintf(out_file, "Min : %lu\n", dyn_stats[i].min);
      fprintf(out_file, "Max : %lu\n", dyn_stats[i].max);
      fprintf(out_file, "Avg : %lf\n", dyn_stats[i].avg);
      if (dyn_stats[i].count < 2) {
        fprintf(out_file, "Variance : %lf\n", 0.0);
      } else {
        fprintf(out_file, "Variance : %lf\n", dyn_stats[i].var / (dyn_stats[i].count - 1));
      }
      fprintf(out_file, "Deactivations : %d\n", dyn_stats[i].deactivation_count);
      fprintf(out_file, "Histogram :\n");
      
      fprintf(out_file, "\t0-10    : %lu,", dyn_stats[i].histogram[0]);
      fprintf(out_file, "\t10-10^2 : %lu,", dyn_stats[i].histogram[1]);
      fprintf(out_file, "\t10^2-10^3 : %lu,", dyn_stats[i].histogram[2]);
      fprintf(out_file, "\t10^3-10^4 : %lu,", dyn_stats[i].histogram[3]);
      fprintf(out_file, "\t10^4-10^5 : %lu,", dyn_stats[i].histogram[4]);
      fprintf(out_file, "\t10^5-10^6 : %lu,", dyn_stats[i].histogram[5]);
      fprintf(out_file, "\t10^6-10^7 : %lu,", dyn_stats[i].histogram[6]);
      fprintf(out_file, "\t10^7-10^8 : %lu,", dyn_stats[i].histogram[7]);
      fprintf(out_file, "\t10^8-10^9 : %lu,", dyn_stats[i].histogram[8]);
      fprintf(out_file, "\t10^9-10^10 : %lu,", dyn_stats[i].histogram[9]);
      fprintf(out_file, "\t10^10-Inf  : %lu,", dyn_stats[i].histogram[10]);

      fprintf(out_file, "\n");
      fprintf(out_file, "Deactivations : %d\n", dyn_stats[i].deactivation_count);
    }
  }
  */

  fclose(out_file);

  // Deallocate all the allocated stuff here

}

void activate_method_profiling(char* method, void (*prolog_func)(), void (*epilog_func)()) {
  prof->start_profile(string(method), prolog_func, epilog_func);
}

void deactivate_method_profiling(const char* method) {
  prof->stop_profile(string(method));
}

void start_profiler() {
  initZCAService();

  // deactivation_queue = new NBQueue(20);
  heavy_hitters = new NBQueue(20);
  inactive_funcs = new list<int>;

  prof = new Basic_Profiler;
  if (strategy == COUNT_ONLY) {
    prof->profile_all(count_only_prolog_func, NULL);
  } else if (strategy == EMPTY_PROLOG) {
    prof->profile_all(empty_prolog_func, NULL);
  }

  prof->profile_all(NULL, NULL);

  atexit(cleanup); // This seems to be a viable alternative to the destructor
}

void stop_profiler() {
  delete prof;
}

void Profiler::start_profile(string method, void (*prolog_func)() , void (*epilog_func)()) {

  string probe_start_annotation = method + ":start";
  string probe_end_annotation = method + ":end";

  if (prolog_func != NULL || epilog_func != NULL) {
    // Need to first deactivate before activating. Otherwise the return from the generated stub will be written with a junk address in a mysterious manner.
    // Probably this will be fixed by atomic modify_probe_site implementation
    deactivateProbe(probe_end_annotation);
    deactivateProbe(probe_start_annotation);
    
    if (prolog_func != NULL) {
     activateProbe(probe_start_annotation, prolog_func);
    }

    if (epilog_func != NULL) {
      activateProbe(probe_end_annotation, epilog_func);
    }
  } else {
    deactivateProbe(probe_end_annotation);
    deactivateProbe(probe_start_annotation);

    if (this->profiler_prolog != NULL) {
      activateProbe(probe_start_annotation, (this->profiler_prolog));
    }

    if (this->profiler_epilog != NULL) {
      activateProbe(probe_end_annotation, (this->profiler_epilog));
    }
  }
}

void Profiler::start_profile(int method_id, void (*prolog_func)(), void (*epilog_func)()) {

  string method = string(dyn_stats[method_id].func_name);
  string probe_start_annotation = method + ":start";
  string probe_end_annotation = method + ":end";

  if (prolog_func!= NULL || epilog_func != NULL) {
    // Need to first deactivate before activating. Otherwise the return from the generated stub will be written with a junk address in a mysterious manner.
    // Probably this will be fixed by atomic modify_probe_site implementation
    deactivateProbe(probe_end_annotation);
    deactivateProbe(probe_start_annotation);
    
    if (prolog_func != NULL) {
     activateProbe(probe_start_annotation, prolog_func);
    }

    if (epilog_func != NULL) {
      activateProbe(probe_end_annotation, epilog_func);
    }
  } else {
    deactivateProbe(probe_end_annotation);
    deactivateProbe(probe_start_annotation);

    if (this->profiler_prolog != NULL) {
      activateProbe(probe_start_annotation, (this->profiler_prolog));
    }

    if (this->profiler_epilog != NULL) {
      activateProbe(probe_end_annotation, (this->profiler_epilog));
    }
  }
}

void Profiler::stop_profile(string method) {

  string probe_start_annotation = method + ":start";
  string probe_end_annotation = method + ":end";

  deactivateProbe(probe_end_annotation);
  deactivateProbe(probe_start_annotation);
}

void Profiler::stop_profile(int method_id) {

  string method = string(dyn_stats[method_id].func_name);
  string probe_start_annotation = method + ":start";
  string probe_end_annotation = method + ":end";

  deactivateProbe(probe_end_annotation);
  deactivateProbe(probe_start_annotation);
}

void tokenize(const string& str,
    vector<string>& tokens,
    const string& delimiters = " ")
{
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (string::npos != pos || string::npos != lastPos)
  {
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
}

void Profiler::profile_all(void (*prolog_func)(), void (*epilog_func)()) {
  map<string, int>* activated_probes = new map<string, int>;

  for (auto iter = annotations.begin(); iter != annotations.end(); iter++) {
    string annotation = iter->first;

    vector<string> tokens;
    tokenize(annotation, tokens, ":");

    string func_name = tokens[0];

    map<string, int>::iterator it;
    it = activated_probes->find(func_name);

    if (it == activated_probes->end()) {
      start_profile(func_name, prolog_func, epilog_func);
    }

    activated_probes->insert(make_pair(func_name,1));
  }

  delete activated_probes;
}

void Profiler::turn_off_profiler() {
  map<string, int>* deactivated_probes = new map<string, int>;

  for (auto iter = annotations.begin(); iter != annotations.end(); iter++) {
    string annotation = iter->first;

    vector<string> tokens;
    tokenize(annotation, tokens, "_");

    string func_name = tokens[0];

    map<string, int>::iterator it;
    it = deactivated_probes->find(func_name);

    if (it == deactivated_probes->end()) {
      stop_profile(func_name); // TODO: Get the func id from functions and pass
    }

    deactivated_probes->insert(make_pair(func_name,1));
  }

  delete deactivated_probes;
}

void empty_prolog_func() {

}

void empty_epilog_func() {

}

void count_only_prolog_func() {
  
  /*
  long func_id = 0;

  asm(
      "movq %%rdx, %0\n\t"
      : "=r"(func_id)
      :
      : "%rdx"
     );


  func_data* data = &dyn_stats[func_id];

  while (!(__sync_bool_compare_and_swap(&(data->lock), 0 , 1)));

  data->count += 1;

  __sync_bool_compare_and_swap(&(data->lock), 1 , 0);
  */

}

void count_only_epilog_func() {

}

void prolog_func() {

  __thread static bool allocated;

  uint64_t addr;
  uint64_t offset = 2;

  // Gets [%rbp + 16] to addr. This is a hacky way to get the function parameter (annotation string) pushed to the stack
  // before the call to this method. Ideally this should be accessible by declaring an explicit method paramter according
  // x86 calling conventions AFAIK. But it fails to work that way hence we do the inline assembly to get it.
  // Fix this elegantly with a method parameter should be a TODO
  long func_id = 0;

  asm(
      "movq %%rdx, %0\n\t"
      : "=r"(func_id)
      :
      : "%rdx"
     ); 
 
  /*
  ticks prolog_start = 0;
  ticks prolog_start_1 = 0;
  if (prolog_overhead == 0) {
    prolog_start = getticks();
  } else {
    prolog_start_1 = getticks();
  }
  */

  ts_stack* ts;
  if (!allocated) {
    ts = new ts_stack;
    allocated = true;
    leaf_counter = 0;

    pthread_once(&tls_init_flag, create_key);
    pthread_setspecific(key, ts);
  } else {
    ts = (ts_stack*)pthread_getspecific(key);
    if (leaf_counter == ULLONG_MAX){
      leaf_counter = 0;
    } else {
      leaf_counter++;
    }
  }

  /*
  if (data->newly_reactivated) {
    // empty the stack
  } */

  ticks time = getticks();
  invocation_data i_data = {time, func_id, leaf_counter,};
  ts->push(i_data); 

  /*
  if (prolog_overhead == 0) {
    prolog_overhead = getticks() - prolog_start;
  } else {
    prolog_overhead_1 = getticks() - prolog_start_1;
  }
  */

}

void no_backoff_epilog_func() {

  uint64_t addr;
  uint64_t offset = 2;

  // Gets [%rbp + 16] to addr. This is a hacky way to get the function parameter (annotation string) pushed to the stack
  // before the call to this method. Ideally this should be accessible by declaring an explicit method paramter according
  // x86 calling conventions AFAIK. But it fails to work that way hence we do the inline assembly to get it.
  // Fix this elegantly with a method parameter should be a TODO
  long func_id = 0;

  asm(
      "movq %%rdx, %0\n\t"
      : "=r"(func_id)
      :
      : "%rdx"
     ); 

  func_data* data = &dyn_stats[func_id];
  ts_stack* ts = (ts_stack*)pthread_getspecific(key);

  ticks start;
  ticks elapsed;
  
  unsigned long long initial_leaf_count = 0;

  // This is to remove data from deactivated methods which failed to clean up
  while (!ts->empty() && func_id != ts->top().func_id) {
    ts->pop();
  } 
  
  if (!ts->empty()) {

    if (data->last_deactivation > ts->top().timestamp) {
      // This is when there has been a deactivation and current function prolog has not been 
      // executed due to the reactivation happening after the function entry.
      ts->pop();
      return;
    }
    start = ts->top().timestamp;
    initial_leaf_count = ts->top().leaf_count;
    ts->pop();
  } else {
    // LOG_ERROR("Mismatching function epilog..\n");
    return;
  }

  // Acquire lock
  while (!(__sync_bool_compare_and_swap(&(data->lock), 0 , 1)));

  ticks end = getticks();
  elapsed = end - start; 

  if (elapsed < data->min || data->min == 0) {
    data->min = elapsed;
  }

  if (elapsed > data->max) {
    data->max = elapsed;
  }

  // This segfaults mysteriously
  /*
  if (!strcmp(data->func_name, "CalculateOffsetParam")) {
     fprintf(stderr, "[Epilog] %lf\n", data->var);
  }
  */

  if (leaf_counter > initial_leaf_count) {
    data->is_leaf = false;
    ticks elapsed_backup = elapsed;
    elapsed = elapsed - 2 * getZCAOverheadTicks() * (leaf_counter - initial_leaf_count);
    if (elapsed <= 0) {
      elapsed = elapsed_backup;
    }
  } else if (leaf_counter < initial_leaf_count){
    fprintf(stderr, "%llu\n", leaf_counter);
    LOG_INFO("Leaf counter reset..\n");
  }

  data->sum = data->sum + elapsed;
  data->count += 1;
  double delta = elapsed - data->avg;
  data->avg = data->avg + delta / data->count;
  data->var = data->var + delta * (elapsed - data->avg);

  data->time_histogram[transfer(elapsed)]++;

  long new_count = data->count - data->last_count;
  if (new_count >= sample_size) {
    uint64_t message_rate = new_count / ((end - data->last_activation) / getTicksPerMilliSec());
    data->rate_histogram[transfer(message_rate)]++;
    data->last_count = data->count;
    data->last_activation = end;
  }
 
  __sync_bool_compare_and_swap(&(data->lock), 1 , 0);

}

void fixed_backoff_epilog_func() {

  uint64_t addr;
  uint64_t offset = 2;

  // Gets [%rbp + 16] to addr. This is a hacky way to get the function parameter (annotation string) pushed to the stack
  // before the call to this method. Ideally this should be accessible by declaring an explicit method paramter according
  // x86 calling conventions AFAIK. But it fails to work that way hence we do the inline assembly to get it.
  // Fix this elegantly with a method parameter should be a TODO
  long func_id = 0;

  asm(
      "movq %%rdx, %0\n\t"
      : "=r"(func_id)
      :
      : "%rdx"
     ); 

  func_data* data = &dyn_stats[func_id];
  ts_stack* ts = (ts_stack*)pthread_getspecific(key);

  ticks start;
  ticks elapsed;
  
  // This is to remove data from deactivated methods which failed to clean up
  while (!ts->empty() && func_id != ts->top().func_id) {
    ts->pop();
  } 
  
  if (!ts->empty()) {

    if (data->last_deactivation > ts->top().timestamp) {
      // This is when there has been a deactivation and current function prolog has not been 
      // executed due to the reactivation happening after the function entry.
      ts->pop();
      return;
    }
    start = ts->top().timestamp;
    ts->pop();
  } else {
    // LOG_ERROR("Mismatching function epilog..\n");
    return;
  }

  // Acquire lock
  while (!(__sync_bool_compare_and_swap(&(data->lock), 0 , 1)));

  ticks end = getticks();
  elapsed = end - start; 

  /*
  if (elapsed < data->min || data->min == 0) {
    data->min = elapsed;
  }

  if (elapsed > data->max) {
    data->max = elapsed;
  }
  */

  data->sum = data->sum + elapsed;
  data->count += 1;

  if (data->count >=  sample_size) {
    data->status = 0;
    prof->stop_profile(func_id);
  }
  
  __sync_bool_compare_and_swap(&(data->lock), 1 , 0);

}

void sampling_epilog_func() {

  uint64_t addr;
  uint64_t offset = 2;

  // Gets [%rbp + 16] to addr. This is a hacky way to get the function parameter (annotation string) pushed to the stack
  // before the call to this method. Ideally this should be accessible by declaring an explicit method paramter according
  // x86 calling conventions AFAIK. But it fails to work that way hence we do the inline assembly to get it.
  // Fix this elegantly with a method parameter should be a TODO
  long func_id = 0;

  asm(
      "movq %%rdx, %0\n\t"
      : "=r"(func_id)
      :
      : "%rdx"
     ); 

  func_data* data = &dyn_stats[func_id];
  ts_stack* ts = (ts_stack*)pthread_getspecific(key);

  ticks start;
  ticks elapsed;

  unsigned long long initial_leaf_count = 0;
  
  // This is to remove data from deactivated methods which failed to clean up
  while (!ts->empty() && func_id != ts->top().func_id) {
    ts->pop();
  } 
  
  if (!ts->empty()) {

    if (data->last_deactivation > ts->top().timestamp) {
      // This is when there has been a deactivation and current function prolog has not been 
      // executed due to the reactivation happening after the function entry.
      ts->pop();
      return;
    }
    start = ts->top().timestamp;
    initial_leaf_count = ts->top().leaf_count;
    ts->pop();
  } else {
    // LOG_ERROR("Mismatching function epilog..\n");
    return;
  }

  // Acquire lock
  while (!(__sync_bool_compare_and_swap(&(data->lock), 0 , 1)));

  ticks end = getticks();
  elapsed = end - start; 

  // Start
  //
  if (elapsed < data->min || data->min == 0) {
    data->min = elapsed;
  }

  if (elapsed > data->max) {
    data->max = elapsed;
  }

  // This segfaults mysteriously
  /*
  if (!strcmp(data->func_name, "CalculateOffsetParam")) {
     fprintf(stderr, "[Epilog] %lf\n", data->var);
  }
  */

  if (leaf_counter > initial_leaf_count) {
    data->is_leaf = false;
    ticks elapsed_backup = elapsed;
    elapsed = elapsed - 2 * getZCAOverheadTicks() * (leaf_counter - initial_leaf_count);
    if (elapsed <= 0) {
      elapsed = elapsed_backup;
    }
  } else if (leaf_counter < initial_leaf_count){
    fprintf(stderr, "%llu\n", leaf_counter);
    LOG_INFO("Leaf counter reset..\n");
  }

  data->sum = data->sum + elapsed;
  data->count += 1;
  double delta = elapsed - data->avg;
  data->avg = data->avg + delta / data->count;
  data->var = data->var + delta * (elapsed - data->avg);

  data->time_histogram[transfer(elapsed)]++;

  // End
  //

  long new_count = data->count - data->last_count;
  if (new_count >= sample_size) {
    data->last_deactivation = end;
    inactive_funcs->push_back(func_id);
    prof->stop_profile(func_id);
    data->deactivation_count++;
    uint64_t message_rate = new_count / ((end - data->last_activation) / getTicksPerMilliSec());
    data->rate_histogram[transfer(message_rate)]++;
    data->last_count = data->count;
  }
  
  __sync_bool_compare_and_swap(&(data->lock), 1 , 0);

}


void bop_simple_epilog_func() {

  uint64_t addr;
  uint64_t offset = 2;

  // Gets [%rbp + 16] to addr. This is a hacky way to get the function parameter (annotation string) pushed to the stack
  // before the call to this method. Ideally this should be accessible by declaring an explicit method paramter according
  // x86 calling conventions AFAIK. But it fails to work that way hence we do the inline assembly to get it.
  // Fix this elegantly with a method parameter should be a TODO
  long func_id = 0;

  asm(
      "movq %%rdx, %0\n\t"
      : "=r"(func_id)
      :
      : "%rdx"
     ); 

  /*
  ticks epilog_start = 0;
  ticks epilog_start_1 = 0;
  if (epilog_overhead == 0) {
    epilog_start = getticks();
  } else {
    epilog_start_1 = getticks();
  }
  */

  func_data* data = &dyn_stats[func_id];
  ts_stack* ts = (ts_stack*)pthread_getspecific(key);

  ticks start;
  ticks elapsed;
  
  // This is to remove data from deactivated methods which failed to clean up
  while (!ts->empty() && func_id != ts->top().func_id) {
    ts->pop();
  } 
  
  if (!ts->empty()) {

    if (data->last_deactivation > ts->top().timestamp) {
      // This is when there has been a deactivation and current function prolog has not been 
      // executed due to the reactivation happening after the function entry.
      ts->pop();
      return;
    }
    start = ts->top().timestamp;
    ts->pop();
  } else {
    // LOG_ERROR("Mismatching function epilog..\n");
    return;
  }

  // Acquire lock
  while (!(__sync_bool_compare_and_swap(&(data->lock), 0 , 1)));

  ticks end = getticks();
  elapsed = end - start; 

  /*
  if (elapsed < data->min || data->min == 0) {
    data->min = elapsed;
  }

  if (elapsed > data->max) {
    data->max = elapsed;
  }
  */

  data->sum = data->sum + elapsed;
  data->count += 1;
  
  long new_count = data->count - data->last_count;

  // Deactivate extended profiling after a reasonable sample size
  /*
  if (new_count >= DEACTIVATION_THRESHOLD) {
    prof->start_profile(func_id, basic_prolog_func, NULL);
  }
  */

  if (new_count >= sample_size) {
    // fprintf(stderr,"Registering %lu for deactivation..\n", func_id);
    // deactivation_queue->enqueue(func_id);
    long msg_rate = new_count / ((end / 1000000L - data->last_activation)/ getTicksPerMilliSec()); // Per millisecond
    // dyn_stats[func_id].histogram->enqueue(msg_rate);

    // fprintf(stderr, "Message rate for func id %lu : %lu\n", func_id, msg_rate);

    if (msg_rate > MSG_RATE_THRESHOLD){
      heavy_hitters->enqueue(func_id);
      // fprintf(stderr, "Message rate for heavy hitter %lu : is %lu\n", func_id, msg_rate);
    }

    // Put for this method in heavy hitters
    /*
    prof->stop_profile(func_id);
    dyn_stats[func_id].deactivation_count++;
    dyn_stats[func_id].last_deactivation = getticks();
    dyn_stats[func_id].last_count = data->count;
    inactive_funcs->push_back(func_id);
    */

    // prof->stop_profile(func_id);
  }

  __sync_bool_compare_and_swap(&(data->lock), 1 , 0);

  /*
  if (epilog_overhead == 0) {
    epilog_overhead = getticks() - epilog_start;
  } else {
    epilog_overhead_1 = getticks() - epilog_start_1;
  }
  */

}

void Basic_Profiler::set_profiler_function() {
  if (strategy == NO_BACKOFF) {
    this->profiler_prolog = prolog_func; 
    this->profiler_epilog = no_backoff_epilog_func;
  } else if (strategy == FIXED_BACKOFF) {
    this->profiler_prolog = prolog_func;
    this->profiler_epilog = fixed_backoff_epilog_func;
  } else if (strategy == BOP_SIMPLE) {
    this->profiler_prolog = prolog_func;
    this->profiler_epilog = bop_simple_epilog_func;
  } else if (strategy == BOP_ADVANCED) {
  
  } else if (strategy == COUNT_ONLY) {
    this->profiler_prolog = count_only_prolog_func; 
    this->profiler_epilog = NULL;
  } else if (strategy == SAMPLING) {
    this->profiler_prolog = prolog_func; 
    this->profiler_epilog = sampling_epilog_func;
  } else if (strategy == EMPTY) {
    this->profiler_prolog = empty_prolog_func; 
    this->profiler_epilog = empty_epilog_func;
  }
}
