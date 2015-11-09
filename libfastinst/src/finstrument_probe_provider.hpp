
#ifndef _FINSTRUMENT_PROBE_PROVIDER_HPP_
#define _FINSTRUMENT_PROBE_PROVIDER_HPP_

#include <unordered_map>

#include "fastinst.hpp"
#include "lock.hpp"
#include "calibrate.hpp"

typedef std::unordered_map<Address, std::string> FuncAddrMapping;
typedef std::unordered_map<Address, uint32_t> ProbeLookupMap; 
// typedef std::unordered_map<uint64_t, lock::CASLock> FuncRWLocks;

class FinstrumentProbeProvider : public ProbeProvider {

  private:
    FuncAddrMapping func_addr_mappings; //!< Function adress to name mapping.
    lock::CASLock probe_lock; //!< Global lock protecting to updates to probe
                               //!<  meta data vector.
    ProbeLookupMap probe_lookup; //!< Looks up currently if a probe with given 
                                 //!< address has already been discovered. Value
                                 //!< is not really important. 

    /// Reads the function related meta data from debug tables.
    /*  The information gathered are function addresses and their names.
     *  Call at initialization time.
     */
    void readFunctionInfo();

    /// Estimates the overhead induced by the mechanism used by this probe 
    /// provider (via cyg_* calls) to get to  and return
    //  from the actual instrumentation function. 
    void calibrateInstrumentationOverhead();

  public:

    /// Finstrument probe provider constructor
    /*  Initializes with the probe discovery callback
     *  and do some data structure initializations related to function 
     *  meta data.
     *  \param callback Probe discovery callback
     */
    FinstrumentProbeProvider(Callback cb) : ProbeProvider(cb) {
      // Printing arg patching method
#if defined(ARG_PATCH_SYNC)
      fprintf(stderr, "ARG_PATCH_METHOD: ARG_PATCH_SYNC\n");
#elif defined(ARG_PATCH_ASYNC)
      fprintf(stderr, "ARG_PATCH_METHOD: ARG_PATCH_ASYNC\n");
#else
      fprintf(stderr, "ARG_PATCH_METHOD: ARG_PATCH_ASYNC\n");
#endif
     
      // Printing invoke patching method
#if defined(INVOKE_PATCH_SYNC)
      fprintf(stderr, "INVOKE_PATCH_METHOD: INVOKE_PATCH_SYNC\n");
#elif defined(INVOKE_PATCH_CALL)
      fprintf(stderr, "INVOKE_PATCH_METHOD: INVOKE_PATCH_CALL\n"); 
#else
      fprintf(stderr, "INVOKE_PATCH_METHOD: INVOKE_PATCH_CALL\n"); 
#endif

      readFunctionInfo();
      // calibrateInstrumentationOverhead();
    }

    void initializeProvider();

    /// Get the number of functions 
    uint64_t getNumberOfFunctions();

    /// Overriden initialize method from ProbeProvider
    void initialize(ProbeId probe_id, ProbeArg probe_arg);

    /// Overridden activate method from ProbeProvider
    bool activate(ProbeId probe_id, InstrumentationFunc func);

    /// Overridden activate method from ProbeProvider
    bool activate_async(ProbeId probe_id, InstrumentationFunc func);

    /// Overridden activate method from ProbeProvider
    void activate_async_finish(ProbeId probe_id);

    /// Overridden deactivate method from ProbeProvider
    bool deactivate(ProbeId probe_id);

    /// Overridden activate method from ProbeProvider
    bool deactivate_async(ProbeId probe_id);

    /// Overridden activate method from ProbeProvider
    void deactivate_async_finish(ProbeId probe_id);

    /// Generates a new ProbeMetaData entry initialized with a sequentilal, 
    //  monotonically increasing (starting from zero) unique probe id filled in.
    /*  \return New ProbeMetaData entry with probe id initialized. 
     */
    ProbeMetaData* getNewProbeMetaDataContainer(Address probe_addr);

    /// Registers the probe with the probe provider.
    /*  This would call the registered callback.
     *  \param pmd ProbeMetaData entry for the probe.
     */
    void registerProbe(ProbeMetaData* pmd);

    /// Gets ProbeMetaData entry for the given probe
    /*  \param probe_id The probe id of the probe
     */
    ProbeMetaData* getProbeMetaData(ProbeId probe_id); 

    /// Gets function name given the function address
    /*  /param func_addr Address of the function
     *  /return mangled name of the function
     */
    std::string getFunctionName(Address func_addr);

    ~FinstrumentProbeProvider() {
    }

};

#endif /* _FINSTRUMENT_PROBE_PROVIDER_HPP_ */
