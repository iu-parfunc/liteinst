
#ifndef _FINSTRUMENT_PROBE_PROVIDER_HPP_
#define _FINSTRUMENT_PROBE_PROVIDER_HPP_

#include <unordered_map>

#include "fastinst.hpp"
#include "lock.hpp"
#include "calibrate.hpp"
#include "utils.hpp"

typedef std::unordered_map<Address, std::string> FuncAddrMapping;
typedef std::unordered_map<Address, uint32_t> ProbeLookupMap; 
// typedef std::unordered_map<uint64_t, lock::CASLock> FuncRWLocks;

#ifdef AUDIT_INIT_COST 
extern ticks** init_costs;
extern volatile int g_thread_counter;
#endif

class FinstrumentProbeProvider : public ProbeProvider {

  private:
    FuncAddrMapping func_addr_mappings; //!< Function adress to name mapping.
    lock::CASLock probe_lock; //!< Global lock protecting to updates to probe
                               //!<  meta data vector.
    ProbeLookupMap probe_lookup; //!< Looks up currently if a probe with given 
                                 //!< address has already been discovered. Value
                                 //!< is not really important. 

#ifdef AUDIT_PROBES 
    // Auditing
    ToggleStatistics** toggle_stats;    
#endif
    int thread_counter;

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

    void initializeProvider(InstrumentationFunc prolog,
        InstrumentationFunc epilog);

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
    // void registerProbe(ProbeMetaData* pmd);

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

#ifdef AUDIT_INIT_COST 
      ticks cost = 0;
      for (int i=0; i < g_thread_counter; i++) {
        cost += *init_costs[i];
      }

      double total_init_cost = utils::getSecondsFromTicks(cost); 
      fprintf(stderr, "INIT_COST: %lf\n", total_init_cost);

      delete(init_costs);
#endif

#ifdef AUDIT_PROBES
      uint64_t deactivation_count = 0;
      uint64_t activation_count = 0;
      ticks deactivation_costs = 0;
      ticks activation_costs = 0;

      for (int i=0; i < thread_counter; i++) {
        ToggleStatistics* stats = toggle_stats[i];
        activation_count += stats->activation_count;
        deactivation_count += stats->deactivation_count;
        activation_costs += stats->activation_costs;
        deactivation_costs+= stats->deactivation_costs;

        free(stats);
      }

      free(toggle_stats);

      double total_activation_cost = 
        utils::getSecondsFromTicks(activation_costs); 
      double total_deactivation_cost = 
        utils::getSecondsFromTicks(deactivation_costs); 

      fprintf(stderr, "ACTIVATION_COUNT: %lu\n", activation_count);
      fprintf(stderr, "DEACTIVATION_COUNT: %lu\n", deactivation_count);
      fprintf(stderr, "TOTAL_TOGGLE_COUNT: %lu\n", activation_count + deactivation_count);
      fprintf(stderr, "ACTIVATION_COST: %.6lf\n", total_activation_cost);
      fprintf(stderr, "DEACTIVATION_COST: %.6lf\n", total_deactivation_cost); 
      fprintf(stderr, "TOTAL_TOGGLE_COST: %.6lf\n", 
          total_activation_cost + total_deactivation_cost);
#endif

    }

};

#endif /* _FINSTRUMENT_PROBE_PROVIDER_HPP_ */
