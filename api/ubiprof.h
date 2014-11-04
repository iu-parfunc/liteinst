#ifndef _UBIPROF_H_
#define _UBIPROF_H_

/*
  Ubiprof API for method profiling 
  */

#ifdef __cplusplus
  extern "C"
{
#endif

  void activate_function(void* func_id);
  void deactivate_function(void* func_id);
  void stop_profiler();
  void start_profiler();
  void initialize();

#ifdef __cplusplus
}
#endif
#endif
