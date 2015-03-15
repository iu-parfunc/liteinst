
#ifndef _PROBE_API_HPP_
#define _PROBE_API_HPP_

// #include "probe_API.h"
#include "../common/include/constants.h"
#include <string>

/*
 * C++ API for probe activation and deactivation 
 */

/// ProbeId type
typedef uint16_t ProbeId;

/**
 *  Common data structure where profilers can store thread local profiling statistics.
 *  func_stats structure holds profiler specific profiling statistics for each function.
 */
typedef struct TLStatistics {
  uint64_t thread_local_overhead; // Profiling overhead incurred by this thread
  uint64_t thread_local_count;    // Number of samples captured by this thread
  uint64_t prolog_overhead;       // Current entered function's instrumentation prolog overhead.
  bool deactivated;               
  void* func_stats;
} TLStatistics;

// Additional bookkeeping information for probe overhead histograms for benchmarking purposes
#ifdef PROBE_HIST_ON

#define PROBE_HIST_MAX_VALUE 100000 // Number up to which the histograms are explicitly binned. After that it's MAX_VALUE+ bin.
#define BIN_SIZE 10               // Maximum number of bins for each probe timing histogram bins
#define PROLOG 1
#define EPILOG 0

typedef struct ProbeStatistics {
  uint64_t* epilog_timings; // Histogram of individual epilog instrumentation timings
  uint64_t* prolog_timings; // Histogram of individual prolog instrumentation timings
  uint64_t* probe_timings; // Histogram of probe timings (epilog + prolog)
} ProbeStatistics;

extern ProbeStatistics* g_probe_stats; 
extern uint64_t* g_epilog_timings; 
extern uint64_t* g_prolog_timings; 
extern uint64_t* g_probe_timings; 
extern int g_num_bins;

#endif

/// Instrumentation function format
/** Instrumentation function should accept one argument for probe identifier.
 */
typedef TLStatistics* (*InstrumentationFunc)(uint16_t);

class Instrumentor {

  public :

    /// Returns a handle of the instrumentor 
    /** Instrumentor is a singleton. Hence all calls for getting a handle for
     *  the instrumentor instance must go through getInstance.  
     *  \param type Specifies the instrumentor type as a constant integer.
     *              Current types as provided in constants.h are
     *              FINSTRUMENT  
     *  \param prolog Prolog instrumentation function as provided by the calling
     *                profiler 
     *  \param epilog Epilog instrumentation function as provided by the calling
     *                profiler 
     */
    static Instrumentor* getInstance(int type, InstrumentationFunc prolog, InstrumentationFunc epilog); 

    /// Initializes the instrumentor
    /** All instrumentor specific data can be intitialized here 
     */
    virtual void initialize() = 0;

    /// Activates profiling for the probesite with given id.
    /// As of now probesite == function
    /** This method is mainly for use by profiler  
     *  functions for toggling on probesites at runtime.  This 
     *  method expects an integer id for uniquely identifying
     *  the function instaed of a string representation for efficency resasons. 
     *  The mapping from human readable function representation (e.g: name)
     *  to function is maintained by the instrumentor.     
     *
     *  \param id 16bit integer id uniquely identifying the function
     *  \parame mask Specifies the nature of the probes to be enabled
     *               Currently only following types are available as
     *               provided in Constants.h
     *               FUNC - For function entry and exit probes 
     */
    virtual int activateProbe(void* id, int mask) = 0;

    /// Activates profiling for the probesite with given name 
    /// As of now probesite == function
    /** This is the method which should be used for programatically
     *  toggling methods using Instrumentor API since access to function ids 
     *  is not available outside instrumentor functions. 
     *
     *  \param id function name 
     */
    virtual int activateProbeByName(void* id, int mask) = 0;

    /// Deactivates profiling for the probesite with given id.
    /// As of now probesite == function
    /** This method is mainly for use by profiler  
     *  functions for toggling on probesites at runtime.  This 
     *  method expects an integer id for uniquely identifying
     *  the function instaed of a string representation for efficency resasons. 
     *  The mapping from human readable function representation (e.g: name)
     *  to function is maintained by the instrumentor.     
     *
     *  \param id 16bit integer id uniquely identifying the function
     *  \parame mask Specifies the nature of the probes to be enabled
     *               Currently only following types are available as
     *               provided in Constants.h
     *               FUNC - For function entry and exit probes 
     */

    virtual int deactivateProbe(void* id, int mask) = 0;

    /// Deactivates profiling for the probesite with given name 
    /// As of now probesite == function
    /** This is the method which should be used for programatically
     *  toggling methods using Instrumentor API since access to function ids 
     *  is not available outside instrumentor functions. 
     *
     *  \param id function name 
     */
    virtual int deactivateProbeByName(void* id, int mask) = 0;

    /// Returns name of the function associated with given id
    /**
     * \param id function id
     */
    virtual std::string getFunctionName(uint16_t id) = 0;

    /// Returns the total number of functions being profiled
    long getFunctionCount() { return func_count; }

    virtual ~Instrumentor() {}

  protected:
    long func_count = 0; ///< Total number of functions being profiled

};

extern Instrumentor* INSTRUMENTOR_INSTANCE; ///< Global instrumentor instance handle

#endif /* _PROBE_API_HPP_ */





