
#ifndef _PROBE_API_HPP_
#define _PROBE_API_HPP_

#include "probe_API.h"
#include <string>

/*
 * C++ API for probe activation and deactivation 
 *
 */

class Instrumentor {

  public :
    virtual void initialize() = 0;
    virtual int activateProbe(void* id) = 0;
    virtual int deactivateProbe(void* id) = 0;
    // virtual void* get_profiler_epilog() = 0;
    // virtual void* get_profiler_prolog() = 0;
    virtual ~Instrumentor() {}

};

#endif /* _PROBE_API_HPP_ */





