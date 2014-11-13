#ifndef _EPROFILER_HPP_
#define _EPROFILER_HPP_

#include "profiler.hpp"

class EmptyProfiler : public Profiler {

  public:
    void initialize();
    void dumpStatistics();
    virtual ~EmptyProfiler();
};

#endif /* _EPROFILER_HPP_ */
