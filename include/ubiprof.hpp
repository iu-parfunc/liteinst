
#ifndef _UBIPROF_HPP_
#define _UBIPROF_HPP_

#include "fastinst.hpp"
#include "lock.hpp"

#include <unordered_map>
#include <vector>

/// This is used to specify which profiler needs to be selected at 
/// intialization time.
enum class ProfilerType {
  BACKOFF, SAMPLING, EMPTY, ADAPTIVE, MINIMAL_ADAPTIVE, MINIMAL_BACKOFF,
  MINIMAL_SAMPLING 
};

/// An opaque, unique function identifier. Do not depend on the representation 
/// of this value.
typedef uint64_t FuncId;

/// Ensure parameter type to the instrumentation function is 
/// the same as function id
typedef ProbeArg FuncId;

/// Mapping of function to probe meta data by function id
typedef 
  std::unordered_map<FuncId, std::vector<const ProbeMetaData*>*> FuncMetaDataById;

/// Mapping of function to probe meta data by function name
typedef 
  std::unordered_map<std::string, std::vector<const ProbeMetaData*>*> 
  FuncMetaDataByName;


class Profiler {

  public:
    static Profiler* profiler_;
    FuncMetaDataById meta_data_by_id_;
    FuncMetaDataByName meta_data_by_name_;  
    InstrumentationFunc prolog_;
    InstrumentationFunc epilog_;
    lock::CASLock func_lock_;

    static Profiler* newInstance(ProfilerType type);

    /// Initializes the profiler. It is an error to call this repeatedly since
    /// Profiler is meant to be a singleton.  
    Profiler(InstrumentationFunc prolog, InstrumentationFunc epilog);


    /// This can be used to initialize internal statistics related data 
    /// structures. At the point of calling of this function Profiler and the 
    /// related ProbeProvider are guaranteed to have been initialized so using
    /// any methods on those would be valid.
    virtual void initialize() = 0;

    // Callback function to be passed to probe layer so that 
    // profiler can take any action necssary when a probe is discovered.
    // This needs to be static so that it could be passed as a C
    // function pointer to the probe API.
    /* \param pmd Probe meta data returned from the probe layer
     */
    static void callback(const ProbeMetaData* pmd);

    // Enable profiling for the given function 
    /* \param name Mangled name of the function
     * */ 
    virtual bool profileFunction(std::string name);

    // Disables profiling for the given function
    /* param name Mangled name of the function
     * \return Whether operation succeeded
     */
    virtual bool unprofileFunction(std::string name);

    /// Gets the function id given its name
    /* \param name Mangled name of the function
     * \return Whether operation succeeded
     */
    virtual FuncId getFunctionId(std::string name);

  protected:

    // Enable profiling for the given function 
    /* This is the version which is being used internally with 
     * instrumentation functions for efficiency reasons.
     * \param func_id The opaque function id
     * \return Whether operation succeeded
     * */
    virtual bool profileFunction(FuncId func_id);

    // Disables profiling for the given function
    /* This is the version which is being used internally
     * with instrumentation functions for efficiency reasons.
     * \param func_id The opaque function id
     * \return Whether operation succeeded
     */
    virtual bool unprofileFunction(FuncId func_id);

};

// ======================================================================
// Global profiler object for profiling

/// Initializes a global Profiler instance of requested type.
/*  This implements one possible policy for creating a profiler 
 *  at application startup time.  This approach is limited because it
 *  recognized only a closed set of profiler implementations.
 *  
 *  It's an error to call this method more than once.
 * 
 *  \param type The type of the profiler needed.
 *  \return A reference to initialized Profiler instance.
 */
extern Profiler* initializeGlobalProfiler(ProfilerType type);

/// Global Profiler instance. Allows plain C functions to accesss Profiler 
/// functions at runtime.
extern Profiler* PROFILER;


#endif /* _UBIPROF_HPP_ */
