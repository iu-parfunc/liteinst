
#ifndef _PROBE_API_HPP_
#define _PROBE_API_HPP_

// #include "probe_API.h"
#include "../common/include/constants.h"
#include <string>

/*
 * C++ API for probe activation and deactivation 
 */

typedef void (*InstrumentationFunc)(uint16_t);

class Instrumentor {

  public :
    static Instrumentor* getInstance(int type, InstrumentationFunc prolog, InstrumentationFunc epilog); 
    virtual void initialize() = 0;
    virtual int activateProbe(void* id, int type) = 0;
    virtual int deactivateProbe(void* id, int type) = 0;
    // virtual void* get_profiler_epilog() = 0;
    // virtual void* get_profiler_prolog() = 0;
    virtual ~Instrumentor() {}

};

extern Instrumentor* INSTRUMENTOR_INSTANCE;

#endif /* _PROBE_API_HPP_ */





