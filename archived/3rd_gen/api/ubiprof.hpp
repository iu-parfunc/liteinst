
#ifndef _UBIPROF_HPP_
#define _UBIPROF_HPP_

#include <cstdint>

#include "probe_API.hpp"

/* 
 * Ubiprof C++ API
 */

/// Top level interface for profiler implementations
/** Profiler implementations need to provide instrumentation functions 
 *  for gathering relevant statistics of interest which can vary depending 
 *  on the profiler implementation. Maintaining statistics related data 
 *  and data strucutures is the responsibility of the profiler implementation.
 *  Additionally Profiler API provides functions for toggling profiling 
 *  at a granularity of functions. New profiler implementations extending
 *  from this class is not required to implement these functions since the 
 *  default implementations of activate/deactivateFunction methods would be 
 *  sufficient for almost all cases.  
 *
 *  The relationship between Profiler and Instrumentor layer at runtime is as
 *  follows.
 *
 *  Profiler (Singleton) --> Instrumentor (Singleton)
 *  [ --> denotes a 'has-a' relationship]
 *
 *  */ 
class Profiler {

  public:

    /// Creates a new profiler instance. 
    /** Profiler is a singleton. Hence all calls for getting a handle for
     *  the profiler instance must go through getInstance.  
     *  Repeated calls will return the existing instance instead of 
     *  returning a new instance.
     *  \param type Specifies the profiler type as a constant integer.
     *              Current types as provided in constants.h are
     *              BACKOFF | SAMPLING | EMPTY | ADAPTIVE | MINIMAL_ADAPTIVE
     *              | MINIMAL_BACKOFF | MINIMAL_SAMPLING
     */
    static Profiler* newInstance(int type);

    /// Retrieves the existing profiler instances. 
    /// Returns NULL of it is not yet set
    static Profiler* getInstance();

    /// Initializes the profiler
    /** Initializing the Instrumentor associated with this profiler 
     *  needs to be done within initialize at a minimum. For that 
     *  initInstrumentor should be called with profiler specific
     *  prolog and epilog instrumentation function pointer as 
     *  arguments. Addditionally initialize is a good place to create 
     *  data structures necessary for profiler statistics bookkeeping. 
     */
    virtual void initialize() = 0;

    /// Activates profiling for the function with given id
    /** This method is mainly for use within profiler instrumentation 
     *  functions for toggling on functions at runtime. This 
     *  method expects an integer id for uniquely identifying
     *  the function instaed of a string representation for efficency resasons. 
     *  The mapping from human readable function representation (e.g: name)
     *  to function is maintained at Instrumentor level. Profiler layer just 
     *  passes through the function id argument it receives via instrumentation 
     *  function parameter to this call.
     *
     *  \param id 16bit integer id uniquely identifying the function
     */
    virtual int activateFunction(uint16_t func_id);

    /// Activates profiling for the function with given name 
    /** This is the method which should be used for programatically
     *  toggling methods using Profiler API since access to function ids 
     *  is not available outside instrumentation functions. 
     *
     *  \param name Mangled function name (according to C++ mangling rules) 
     */
    virtual int activateFunction(std::string name);

    /// Dectivates profiling for the function with given id
    /** This method is mainly for use within profiler instrumentation 
     *  functions for toggling off functions at runtime. This 
     *  method expects an integer id for uniquely identifying
     *  the function instaed of a string representation for efficency resasons. 
     *  The mapping from human readable function representation (e.g: name)
     *  to function is maintained at Instrumentor level. Profiler layer just 
     *  passes through the function id argument it receives via instrumentation 
     *  function parameter to this call.
     *
     *  \param id 16bit integer id uniquely identifying the function
     */
    virtual int deactivateFunction(uint16_t id);

    /// Dectivates profiling for the function with given name 
    /** This is the method which should be used for programatically
     *  toggling methods using Profiler API since access to function ids 
     *  is not available outside instrumentation functions. 
     *
     *  \param id Mangled function name (according to C++ mangling rules) 
     */
    virtual int deactivateFunction(std::string name);

    /// Starts profiling all functions
    virtual void startProfiler();

    /// Stops profiling all functions
    virtual void stopProfiler();

    /// Flush the statistics to its intended destination 
    /** This is guaranteed to be called at application termination
     */
    virtual void dumpStatistics() = 0;

    /// Tear down profiler instance
    /** cleanupInstrumentor method should be called with this destructor
     *  at a minimum. Typically all the profiler allocated data 
     *  structures is deallocated here as well.
     */
    virtual ~Profiler();

  protected:
    
    /// Initializes the Instrumentor instance associated with the Profiler
    /** Prolog and epilog functions implements the profiler specific 
     *  instrumentation for capturing data from each function entry and 
     *  exit. These callback methods will be called with function id 
     *  integer arguments at runtime
     */
    virtual void initInstrumentor(InstrumentationFunc prologFunc, InstrumentationFunc epilogFunc);

    /// Tear down the Instrumentor instance associated with the Profiler
    virtual void cleanupInstrumentor();

    Instrumentor* ins; ///< Instrumentor instance handle

};

extern Profiler* PROFILER_INSTANCE; ///< Global Profiler instance handle
extern void* g_ubiprof_stats; ///< Global handle for accessing profiler statistics. Need to be cast
                              ///< profiler specific type before accessing.
extern volatile uint64_t g_deactivation_count;
extern uint64_t g_cache_miss_overhead_upper_bound; ///< Global cache miss overhead estimate 
                                                   ///<induced by a single call to an 
                                                   ///<instrumentation function 
extern uint64_t g_init_overhead; /// <Global for storing initialization overhead for Ubiprof 

#endif /* _UBIPROF_HPP_ */
