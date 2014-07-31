#include "zca-toggle.hpp"
#include "elf-provider.h"
#include "ptypes.hpp"
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

#define NANO_SECONDS_IN_SEC 1000000000;

using namespace std;

Profiler* prof;

NBQueue* heavy_hitters;

__thread static unsigned long long leaf_counter = 0;

int strategy = BOP_SIMPLE;
double target_overhead = 0.05; 
double overhead = 0.0;
long sample_size = 10000;
double sample_rate = 0.01 * NANO_SECONDS_IN_SEC; // Default sampling rate is 10ms

/** global statistics **/
dyn_global_data* dyn_global_stats;

/** thread local statistics **/
__thread static dyn_thread_data* dyn_thread_stats; 

/** thread local statistics pointers **/
dyn_thread_data** dyn_thread_stats_arr;

/** implementation related variables **/
int thr_array_idx = 0;


int transfer(long v) {
  
  int r = (v < 10) ? 0 : (v < 100) ? 1 : (v < 1000) ? 2 : (v < 10000) ? 3 : 
            (v < 100000) ? 4 : (v < 1000000) ? 5 : (v < 10000000) ? 6 : (v < 100000000) ? 7 : 
            (v < 1000000000) ? 8 : (v < 10000000000) ? 9 : 10; 

  return r;
}

// Check this out later
//  http://stackoverflow.com/questions/2053029/how-exactly-does-attribute-constructor-work

/** Monitoring thread implementations for different strategies **/

void* probe_monitor(void* param) {

  while(true) {
    for (int j=0; j < function_count; j++) {
      dyn_global_stats[j].count = 0;
      for (int i=0; i < thr_array_idx; i++) {
        dyn_global_stats[j].count += dyn_thread_stats_arr[i][j].count;
      }
    }

    nanosleep((struct timespec[]){{0, sample_rate}}, NULL);
  }

  return NULL;

}

void* probe_monitor_sampling(void* param) {

  while(true) {
    ticks current_time = getticks();
    for (int j=0; j < function_count; j++) {
      if(dyn_global_stats[j].active) {
        dyn_global_stats[j].count = 0;
        for (int i=0; i < thr_array_idx; i++) {
          dyn_global_stats[j].count += dyn_thread_stats_arr[i][j].count;
        }
      } else {
        dyn_global_stats[j].count_at_last_activation = dyn_global_stats[j].count;
        dyn_global_stats[j].latest_activation_time = current_time;
        dyn_global_stats[j].active = true;
        prof->start_profile(j, NULL, NULL);
        // fprintf(stderr, "[Monitor_thread] Reactivated function : %d\n", j);
      }
    }

    nanosleep((struct timespec[]){{0, sample_rate}}, NULL);
  }

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

  char* sample_rate_str = getenv("DYN_SAMPLE_PERIOD");
  if (sample_rate_str != NULL) {
    sample_rate = atof(sample_rate_str);
    sample_rate *= NANO_SECONDS_IN_SEC;
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
    } else if (strcmp(strategy_str, fixed_backoff) == 0) {
      strategy = FIXED_BACKOFF; 
    }
  }

  set_profiler_function();

  fprintf(stderr, "[Init] Strategy : %s Overhead : %s Sample size : %s Sample_rate : %s\n", 
      strategy_str, overhead_str, sample_size_str, sample_rate_str);
  fprintf(stderr, "[Init] Strategy : %d Overhead : %lf Sample size : %lu Sample_rate : %lf\n", 
      strategy, target_overhead, sample_size, sample_rate);

  dyn_global_stats = (dyn_global_data*)calloc(function_count, sizeof(dyn_global_data));
  dyn_thread_stats_arr = (dyn_thread_data**) calloc(64, sizeof(dyn_thread_data*));

  ticks current_time = getticks();

  typedef std::map<std::string, int>::iterator it_type;
  for(auto iterator = functions->begin(); iterator != functions->end(); iterator++) {
    int func_id = iterator->second;

    const char* func_name = (iterator->first).c_str();
    dyn_global_stats[func_id].func_name = strdup(func_name); // dup this and delete functions??
    dyn_global_stats[func_id].active = true;
    dyn_global_stats[func_id].latest_activation_time = current_time;
    dyn_global_stats[func_id].is_leaf= true;

    /*
    dyn_global_stats[func_id].min = 0;
    dyn_global_stats[func_id].max = 0;
    dyn_global_stats[func_id].sum = 0;
    dyn_global_stats[func_id].count = 0;
    dyn_global_stats[func_id].var = 0.0;
    dyn_global_stats[func_id].avg = 0.0;
    dyn_global_stats[func_id].deactivation_count = 0;

    for (int j = 0; j < 11; j++) {
      dyn_global_stats[func_id].time_histogram[j]=0;
      dyn_global_stats[func_id].rate_histogram[j]=0;
    }
    dyn_global_stats[func_id].is_leaf= true;

    dyn_global_stats[func_id].count_at_last_activation = 0;
    dyn_global_stats[func_id].lock = 0;
    */
    // dyn_stats[func_id].time_histogram = new NBQueue(20);
    // We don't need function id here at the moment if required later add from functions iterator->second.
  }

  // Spawns the overhead monitor thread if we are following sophisticated back off strategies
  pthread_t tid; 
  pthread_create(&tid, NULL, probe_monitor, (void*)NULL);

  if (strategy == SAMPLING) {
    pthread_create(&tid, NULL, probe_monitor_sampling, (void*)NULL);
  } else if (strategy == NO_BACKOFF) {
    pthread_create(&tid, NULL, probe_monitor, (void*)NULL);
  }

}

void Profiler::register_thread_data(dyn_thread_data* data) {
  if (thr_array_idx + 1 < 64) {
    dyn_thread_stats_arr[thr_array_idx++] = data;
  } else {
    LOG_ERROR("Max thread count exceeded. This thread will not be profiled..\n");
  }
  // do realloc if we run out of indices; 
}

// __attribute__((destructor)) - This doesn't seem to work properly with our heap data being tampered with when this gets called
void cleanup(void) {

  int counter = 0;
  FILE *out_file = fopen("prof.out", "w");

  // pid_t tid = syscall(SYS_gettid);

  // fprintf(out_file, "[Main]Thread local stack is at : %p\n", (ts_stack*)pthread_getspecific(key));
  // fprintf(out_file, "[Main]Current thread is : %d\n", tid);

  // fprintf(stderr, "FINAL_OVERHEAD %.03f\n", overhead);

  // Aggreate thread local statistics
  for (int j=0; j < function_count; j++) {
    dyn_global_stats[j].count = 0;
    dyn_global_stats[j].sum = 0;
    for (int i=0; i < thr_array_idx; i++) {
      dyn_global_stats[j].count += dyn_thread_stats_arr[i][j].count;
      dyn_global_stats[j].sum += dyn_thread_stats_arr[i][j].sum;

      if (dyn_global_stats[j].min == 0 || dyn_global_stats[j].min > dyn_thread_stats_arr[i][j].min) {
        dyn_global_stats[j].min = dyn_thread_stats_arr[i][j].min;
      }

      if (dyn_global_stats[j].max < dyn_thread_stats_arr[i][j].max) {
        dyn_global_stats[j].max = dyn_thread_stats_arr[i][j].max;
      }

      for (int k=0; k < 11; k++) {
        dyn_global_stats[j].time_histogram[k] += dyn_thread_stats_arr[i][j].time_histogram[k];
      }

      if (dyn_global_stats[j].is_leaf && !dyn_thread_stats_arr[i][j].is_leaf) {
        dyn_global_stats[j].is_leaf = false;
      }
    }
  }

  long total_invocations = 0L;
  for (int i=0; i<function_count;i++) {
    total_invocations += dyn_global_stats[i].count;
  }
  fprintf(stderr, "\nTotal invocations : %lu\n", total_invocations);
  fprintf(stderr, "\nTicks per nano seconds : %lf\n", getTicksPerNanoSec());
  fprintf(stderr, "Total overhead from invocations (s): %lf\n", (total_invocations * 1200 / getTicksPerNanoSec()/1000000000));

  fprintf(out_file, "%-30s%-15s%-15s%-15s%-13s%-13s%-15s%-15s%-15s%-5s\n\n", "Function", "Rate_Hist", "Time_Hist",
      "Count", "Samples", "Min", "Max", "Avg", "Variance", "Leaf?");
  
  for(int i=0; i < function_count; i++) {
    if (dyn_global_stats[i].count != 0) {
      bool first_row = true;

      uint64_t max_hist_time = 0;
      int max_hist_time_idx = 0;
      uint64_t max_hist_rate = 0;
      int max_hist_rate_idx = 0;

      for (int j=0; j < 11; j++) {
        if (dyn_global_stats[i].time_histogram[j] > max_hist_time) {
          max_hist_time = dyn_global_stats[i].time_histogram[j];
          max_hist_time_idx = j;
        }

        if (dyn_global_stats[i].rate_histogram[j] > max_hist_rate) {
          max_hist_rate = dyn_global_stats[i].rate_histogram[j];
          max_hist_rate_idx = j;
        }
      }

      char buf[140] = {'\0'};
      snprintf(buf, 140, "%llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu ", 
          dyn_global_stats[i].time_histogram[0], 
          dyn_global_stats[i].time_histogram[1],
          dyn_global_stats[i].time_histogram[2], 
          dyn_global_stats[i].time_histogram[3],
          dyn_global_stats[i].time_histogram[4], 
          dyn_global_stats[i].time_histogram[5],
          dyn_global_stats[i].time_histogram[6], 
          dyn_global_stats[i].time_histogram[7],
          dyn_global_stats[i].time_histogram[8],
          dyn_global_stats[i].time_histogram[9], 
          dyn_global_stats[i].time_histogram[10]
          );

      char buf1[140] = {'\0'};
      snprintf(buf1, 140, "%llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu ", 
          dyn_global_stats[i].rate_histogram[0], 
          dyn_global_stats[i].rate_histogram[1],
          dyn_global_stats[i].rate_histogram[2], 
          dyn_global_stats[i].rate_histogram[3],
          dyn_global_stats[i].rate_histogram[4], 
          dyn_global_stats[i].rate_histogram[5],
          dyn_global_stats[i].rate_histogram[6], 
          dyn_global_stats[i].rate_histogram[7],
          dyn_global_stats[i].rate_histogram[8],
          dyn_global_stats[i].rate_histogram[9], 
          dyn_global_stats[i].rate_histogram[10]
          );
        
      double variance = 0;
      if (dyn_global_stats[i].count > 2) {
        variance = dyn_global_stats[i].var / (dyn_global_stats[i].count - 1);
      }

      double avg = (double) dyn_global_stats[i].sum / dyn_global_stats[i].count;

      fprintf(out_file, "%-30s%-15s%-15s%-15llu%-13d%-13llu%-15llu%-15.1lf%-15.1lf%-5s\n", dyn_global_stats[i].func_name, buf1, buf, 
              dyn_global_stats[i].count, dyn_global_stats[i].deactivation_count+1, dyn_global_stats[i].min, dyn_global_stats[i].max, avg, 
              0.0, dyn_global_stats[i].is_leaf ? "TRUE" : "FALSE");

    }
  }

  /*
  fprintf(out_file, "\n\n-------- Histograms ------------\n");
  fprintf(out_file, "%-50s%-13s%-13s\n\n", "Function", "Rate_Hist", "Time_Hist");

  for(int i=0; i < function_count; i++) {
    bool first_row = true;
    for(int j=0; j < 11 ; j++) {
      char buf[14], buf1[14];
      snprintf(buf, 13, "%d:%llu", j, dyn_global_stats[i].time_histogram[j]);
      snprintf(buf1, 13, "%d:%llu", j, dyn_global_stats[i].rate_histogram[j]);

      if (first_row) {
        fprintf(out_file, "%-50s%-13s%-13s\n", dyn_global_stats[i].func_name, buf1, buf);
        first_row = false;
      } else {
        fprintf(out_file, "%-50s%-13s%-13s\n", " ", buf1, buf);
      }
    }
  }*/

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

  prof = new Basic_Profiler;
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

  string method = string(dyn_global_stats[method_id].func_name);
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

  string method = string(dyn_global_stats[method_id].func_name);
  string probe_start_annotation = method + ":start";
  string probe_end_annotation = method + ":end";

  deactivateProbe(probe_end_annotation);
  deactivateProbe(probe_start_annotation);
}

void tokenize(const string& str,
    vector<string>& tokens,
    const string& delimiters = " ")
{
  // Skip delimiters at beginning.
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  // Find first "non-delimiter".
  string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (string::npos != pos || string::npos != lastPos)
  {
    // Found a token, add it to the vector.
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    // Skip delimiters.  Note the "not_of"
    lastPos = str.find_first_not_of(delimiters, pos);
    // Find next "non-delimiter"
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


/*** NO_BACKOFF_STRATEGY ***/

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

  if (!allocated) {
    dyn_thread_stats = (dyn_thread_data*)calloc(function_count, sizeof(dyn_thread_data));

    for (int i=0; i < function_count; i++) {
      dyn_thread_stats[i].is_leaf = true;
    }

    allocated = true;
    leaf_counter = 0;
    prof->register_thread_data(dyn_thread_stats); 
  } else {
    if (leaf_counter == ULLONG_MAX){
      leaf_counter = 0;
    } else {
      leaf_counter++;
    }
  }

  // Increment data->count and data->ignored_count

  dyn_thread_data* t_stats = &dyn_thread_stats[func_id];
  dyn_global_data* gl_stats = &dyn_global_stats[func_id];

  uint32_t stack_depth = t_stats->stack_depth;
  if (t_stats->stack_depth > 0 && t_stats->invocation_stack[stack_depth-1].timestamp < gl_stats->latest_activation_time) {
    t_stats->stack_depth = 0;
    t_stats->ignore_count = 0;
  }
  
  if (stack_depth < 20) {
    t_stats->invocation_stack[stack_depth].timestamp = getticks();
    t_stats->invocation_stack[stack_depth].func_id = func_id;
    t_stats->invocation_stack[stack_depth].leaf_count = leaf_counter;
    t_stats->stack_depth++;
  } else {
    t_stats->ignore_count++;
    // __sync_add_and_fetch(&dyn_stats[func_id].count, 1);
    // __sync_add_and_fetch(&dyn_stats[func_id].limited_count, 1);
    // dyn_stats[func_id].count++;
    // dyn_stats[func_id].limited_count++;
  }

  if (!dyn_global_stats[func_id].active) {
    t_stats->stack_depth = 0;
    t_stats->ignore_count = 0;
  }

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

  dyn_thread_data* t_stats = &dyn_thread_stats[func_id];
  dyn_global_data* gl_stats = &dyn_global_stats[func_id];

  int stack_depth = t_stats->stack_depth; 

  if (t_stats->stack_depth == 0) {
    return;
  }

  invocation_data i_data = t_stats->invocation_stack[stack_depth-1];
  if (i_data.timestamp < gl_stats->latest_activation_time) {
    t_stats->stack_depth = 0;
    t_stats->ignore_count = 0;
    return;
  } 

  if (stack_depth > 20 && t_stats->ignore_count > 0) {
    t_stats->ignore_count--;
    t_stats->limited_count++;
    t_stats->count++;

    return;
  }

  ticks start = i_data.timestamp;
  unsigned long long initial_leaf_count = i_data.leaf_count;

  ticks end = getticks();
  ticks elapsed = end - start; 

  if (elapsed < t_stats->min || t_stats->min == 0) {
    t_stats->min = elapsed;
  }

  if (elapsed > t_stats->max) {
    t_stats->max = elapsed;
  }

  // This segfaults mysteriously
  /*
  if (!strcmp(data->func_name, "CalculateOffsetParam")) {
     fprintf(stderr, "[Epilog] %lf\n", data->var);
  }
  */

  if (leaf_counter > initial_leaf_count) {
    t_stats->is_leaf = false;
    /*
    ticks elapsed_backup = elapsed;
    elapsed = elapsed - 2 * getZCAOverheadTicks() * (leaf_counter - initial_leaf_count);
    if (elapsed <= 0) {
      elapsed = elapsed_backup;
    }
    */
  } else if (leaf_counter < initial_leaf_count){
    fprintf(stderr, "%llu\n", leaf_counter);
    LOG_INFO("Leaf counter reset..\n");
  }

  t_stats->sum = t_stats->sum + elapsed;
  t_stats->count += 1;

  /*
  double delta = elapsed - data->avg;
  data->avg = data->avg + delta / data->count;
  data->var = data->var + delta * (elapsed - data->avg);
  */

  t_stats->time_histogram[transfer(elapsed)]++;

  long new_count = gl_stats->count - gl_stats->count_at_last_activation;
  if (new_count >= sample_size) {
    uint64_t message_rate = new_count / ((end - gl_stats->latest_activation_time) / getTicksPerMilliSec());
    gl_stats->rate_histogram[transfer(message_rate)]++;
    gl_stats->count_at_last_activation = gl_stats->count;
    gl_stats->latest_activation_time = end;
  }
  
  if (!dyn_global_stats[func_id].active) {
    t_stats->stack_depth = 0;
    t_stats->ignore_count = 0;
  } else {
    t_stats->stack_depth--;
  } 
}

/** BOUNDED_OVERHEAD_PROFILING_STRATEGY **/

void bop_epilog_func() {

}

/** COUNT_ONLY_STRATEGY **/

void count_only_prolog_func() {

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

  if (!allocated) {
    dyn_thread_stats = (dyn_thread_data*)calloc(function_count, sizeof(dyn_thread_data));

    for (int i=0; i < function_count; i++) {
      dyn_thread_stats[i].is_leaf = true;
    }

    allocated = true;
    prof->register_thread_data(dyn_thread_stats); 
  } 

  dyn_thread_data* t_stats = &dyn_thread_stats[func_id];
  t_stats->count += 1;

}

/*** SAMPLING_STRATEGY ***/

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

  dyn_thread_data* t_stats = &dyn_thread_stats[func_id];
  dyn_global_data* gl_stats = &dyn_global_stats[func_id];

  int stack_depth = t_stats->stack_depth; 

  if (t_stats->stack_depth == 0) {
    return;
  }

  invocation_data i_data = t_stats->invocation_stack[stack_depth-1];
  if (i_data.timestamp < gl_stats->latest_activation_time) {
    t_stats->stack_depth = 0;
    t_stats->ignore_count = 0;
    return;
  } 

  if (stack_depth > 20 && t_stats->ignore_count > 0) {
    t_stats->ignore_count--;
    t_stats->limited_count++;
    t_stats->count++;

    return;
  }

  ticks start = i_data.timestamp;
  unsigned long long initial_leaf_count = i_data.leaf_count;

  ticks end = getticks();
  ticks elapsed = end - start; 

  if (elapsed < t_stats->min || t_stats->min == 0) {
    t_stats->min = elapsed;
  }

  if (elapsed > t_stats->max) {
    t_stats->max = elapsed;
  }

  // This segfaults mysteriously
  /*
  if (!strcmp(data->func_name, "CalculateOffsetParam")) {
     fprintf(stderr, "[Epilog] %lf\n", data->var);
  }
  */

  if (leaf_counter > initial_leaf_count) {
    t_stats->is_leaf = false;
    /*
    ticks elapsed_backup = elapsed;
    elapsed = elapsed - 2 * getZCAOverheadTicks() * (leaf_counter - initial_leaf_count);
    if (elapsed <= 0) {
      elapsed = elapsed_backup;
    }
    */
  } else if (leaf_counter < initial_leaf_count){
    fprintf(stderr, "%llu\n", leaf_counter);
    LOG_INFO("Leaf counter reset..\n");
  }

  t_stats->sum = t_stats->sum + elapsed;
  t_stats->count += 1;

  /*
  double delta = elapsed - data->avg;
  data->avg = data->avg + delta / data->count;
  data->var = data->var + delta * (elapsed - data->avg);
  */

  t_stats->time_histogram[transfer(elapsed)]++;
  
  uint64_t global_count = 0;
  for (int i=0; i < thr_array_idx; i++) {
    global_count += dyn_thread_stats_arr[i][func_id].count;
  }

  long new_count = global_count - gl_stats->count_at_last_activation;
  if (new_count >= sample_size) {
    if (__sync_bool_compare_and_swap(&(gl_stats->lock), 0 , 1)) {
      uint64_t message_rate = new_count / ((end - gl_stats->latest_activation_time) / getTicksPerMilliSec());
      gl_stats->rate_histogram[transfer(message_rate)]++;
      // fprintf(stderr, "[Sampling_epilog] Deactivated function : %lu\n", func_id);
      prof->stop_profile(func_id);
      gl_stats->active = false;
      __sync_bool_compare_and_swap(&(gl_stats->lock), 1 , 0);
    }
  }
  
  if (!dyn_global_stats[func_id].active) {
    t_stats->stack_depth = 0;
    t_stats->ignore_count = 0;
  } else {
    t_stats->stack_depth--;
  } 
}

/** EMPTY_STRATEGY **/
void empty_func() {

}

void Basic_Profiler::set_profiler_function() {

  switch(strategy) {
    case NO_BACKOFF:
      this->profiler_prolog = prolog_func;
      this->profiler_epilog = no_backoff_epilog_func;
      break;
    case SAMPLING:
      this->profiler_prolog = prolog_func;
      this->profiler_epilog = sampling_epilog_func;
      break;
    case COUNT_ONLY:
      this->profiler_prolog = count_only_prolog_func;
      this->profiler_epilog = NULL;
      break;
    case EMPTY:
      this->profiler_prolog = empty_func;
      this->profiler_epilog = empty_func;
      break;
    case FIXED_BACKOFF:
      this->profiler_prolog = prolog_func;
      this->profiler_epilog = sampling_epilog_func;
      break;
    default:
      this->profiler_prolog = prolog_func;
      this->profiler_epilog = no_backoff_epilog_func;
  }
}
