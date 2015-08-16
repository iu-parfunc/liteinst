
#include <unordered_map>

#include "fastinst.hpp"
#include "lock.hpp"

typedef std::unordered_map<Address, std::string> FuncAddrMapping;
typedef std::unordered_map<Address, uint32_t> ProbeLookupMap; 
// typedef std::unordered_map<uint64_t, lock::CASLock> FuncRWLocks;

class FinstrumentProbeProvider : ProbeProvider {

  private:
    FuncAddrMapping func_addr_mappings; //!< Function adress to name mapping.
    lock::CASLock* probe_lock; //!< Global lock protecting to updates to probe
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
    /// provider (via indirect jumps, trampolines etc.) to get to  and return
    //  from the actual instrumentation function. 
    uint64_t estimateInstrumentationOverhead();

  public:

    /// Finstrument probe provider constructor
    /*  Initializes with the probe discovery callback
     *  and do some data structure initializations related to function 
     *  meta data.
     *  \param callback Probe discovery callback
     */
    FinstrumentProbeProvider(Callback cb):ProbeProvider(cb) {
      readFunctionInfo();
      PROBE_PROVIDER = this;
      probe_lock = new lock::CASLock;
    }

    /// Overriden initialize method from ProbeProvider
    void initialize(ProbeId probe_id, ProbeArg probe_arg);

    /// Overridden activate method from ProbeProvider
    bool activate(ProbeId probe_id, InstrumentationFunc func);

    /// Overridden deactivate method from ProbeProvider
    bool deactivate(ProbeId probe_id);

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

};
