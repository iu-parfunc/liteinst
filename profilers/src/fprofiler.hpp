
#ifndef _FPROFILER_HPP_
#define _FPROFILER_HPP_

#include <unordered_map>
#include <map>
#include "profiler.hpp"

typedef struct Fstat {
  uint16_t funcId;
  uint64_t count;
} Fstat; 

typedef std::map<uint16_t, Fstat*> Fstats;  

class FBprofiler : public Profiler {

  public:
    void initialize();
    void dumpStatistics();
    virtual ~FBprofiler();

    Fstats* statistics; 
};

#endif /* _FPROFILER_HPP_ */
