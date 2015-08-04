
#ifndef _PROBE_API_HPP_
#define _PROBE_API_HPP_

// #include "probe_API.h"
#include "../common/include/constants.h"
#include <cstdint>
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

extern uint64_t g_epilog_count;
extern double g_epilog_mean;
extern double g_epilog_variance; 

extern uint64_t g_prolog_count;
extern double g_prolog_mean;
extern double g_prolog_variance;

extern uint64_t g_total_probe_count;
extern double g_probe_mean;
extern double g_probe_variance;

extern int g_num_bins;

#endif

#ifdef PROBE_TRUE_EMPTY_ON

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

extern volatile bool g_ubiprof_initialized;

/// Instrumentation function format
/** Instrumentation function should accept one argument for probe identifier.
 */
typedef TLStatistics* (*InstrumentationFunc)(uint16_t);

class Instrumentor {

  public :

    /// Creates a new Instrumentor instance 
    /** Instrumentor is a singleton. Hence all calls for getting a handle for
     *  the instrumentor instance must go through getInstance. Repeated calling
     *  of this function returns the existing instrumentor and will not create 
     *  a new instrumentor instance.  
     *  \param type Specifies the instrumentor type as a constant integer.
     *              Current types as provided in constants.h are
     *              FINSTRUMENT  
     *  \param prolog Prolog instrumentation function as provided by the calling
     *                profiler 
     *  \param epilog Epilog instrumentation function as provided by the calling
     *                profiler 
     */
    static Instrumentor* newInstance(int type, InstrumentationFunc prolog, 
        InstrumentationFunc epilog); 

    /// Retrieves current instrumentor instance
    /*  Returns NULL if not instrumentor is set yet
     */
    static Instrumentor* getInstance(); 

    /// Configures the current Instrumentor instance with new instrumentation 
    /// functions
    /**  \param prolog Prolog instrumentation function as provided by the calling
     *                profiler 
     *  \param epilog Epilog instrumentation function as provided by the calling
     *                profiler 
     */
    virtual Instrumentor* configureInstance(InstrumentationFunc prolog,
        InstrumentationFunc epilog) = 0;

    /// Initializes the instrumentor
    /** All instrumentor specific data can be intitialized here 
     */
    virtual void initialize() = 0;

    /// Activates profiling for the given function with given name.
    /** This method is mainly for use by profiler  
     *  functions for toggling on functions at runtime.  This 
     *  method expects a mangled function name for uniquely identifying
     *  the function.     
     *  This is the method which should be used for programatically
     *  toggling functions using Instrumentor API since access to function ids 
     *  is not available outside instrumentor functions. 
     *
     *  \param name  Mangled function name (According to C++ mangling rules) 
     */
    virtual bool activateFunction(std::string name) = 0;

    /// Activates profiling for the probesite with given id 
    /** This is used within profilers for efficiency reasons where function name
     *  to id mapping is available.     
     *  \param id function id 
     */
    virtual bool activateFunction(uint16_t id) = 0;

    /// Deactivates profiling for the given function with given name.
    /** This method is mainly for use by profiler  
     *  functions for toggling off functions at runtime.  This 
     *  method expects a mangled function name for uniquely identifying
     *  the function.     
     *  This is the method which should be used for programatically
     *  toggling functions using Instrumentor API since access to function ids 
     *  is not available outside instrumentor functions. 
     *
     *  \param name  Mangled function name (According to C++ mangling rules) 
     */
    virtual bool deactivateFunction(std::string name) = 0;

    /// Deactivates profiling for the probesite with given id 
    /** This is used within profilers for efficiency reasons where function name
     *  to id mapping is available.     
     *  \param id function id 
     */
    virtual bool deactivateFunction(uint16_t id) = 0;

    /// Returns name of the function associated with given id
    /**
     * \param id function id
     */
    virtual std::string getFunctionName(uint16_t id) = 0;

    /// Explicitly adds function to the instrumentor meta data.
    /**
     * \param addr function address
     * \param name function name
     */
    virtual void addFunction(uint64_t addr, std::string name) = 0;

    /// Returns the total number of functions being profiled
    long getFunctionCount() { return func_count; }

    /// Overhead which indirectly due to the methods of this class,
    /// but which does not directly appear within those calls.
    /// Returns a value in cycles (ticks).
    virtual uint64_t getInstrumentorBackgroundOverhead() = 0;

    virtual ~Instrumentor() {}

  protected:
    long func_count = 0; ///< Total number of functions being profiled

};

extern Instrumentor* INSTRUMENTOR_INSTANCE; ///< Global instrumentor instance handle

#endif /* _PROBE_API_HPP_ */
