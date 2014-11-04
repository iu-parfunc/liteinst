
#ifndef _UBIPROF_HPP_
#define _UBIPROF_HPP_

#include <cstdint>

#include "probe_API.hpp"

/** 
 * Ubiprof C++ API
 */

class Profiler {

  public:
    static Profiler* getInstance(int type);
    virtual void initialize() = 0;
    virtual int activateFunction(void* id);
    virtual int deactivateFunction(void* id);
    virtual void startProfiler();
    virtual void stopProfiler();
    virtual void dumpStatistics() = 0;
    virtual ~Profiler();

  protected:
    virtual void init_internal(InstrumentationFunc prologFunc, InstrumentationFunc epilogFunc);
    virtual void cleanup_internal();
    // static void prologFunction(uint16_t funcId) = 0;
    // static void epilogFunction(uint16_t funcId) = 0;

    Instrumentor* ins;

};

extern Profiler* PROFILER_INSTANCE;
extern void* stats;

#endif /* _UBIPROF_HPP_ */
