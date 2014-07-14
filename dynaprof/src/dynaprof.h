
#ifndef PROFILER_HPP_
#define PROFILER_HPP_
#ifdef __cplusplus
extern "C"
{
#endif

void activate_method_profiling(char* method, void (*prolog_func)(), void (*epilog_func)());

void deactivate_method_profiling(const char* method);

void start_profiler();

void stop_profiler();

void cleanup();

#ifdef __cplusplus
}
#endif
#endif /* PROFILER_HPP_ */
