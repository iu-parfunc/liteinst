
#ifndef _ZCA_PROBE_PROVIDER_HPP_
#define _ZCA_PROBE_PROVIDER_HPP_

#include <unordered_map>

#include "fastinst.hpp"
#include "lock.hpp"
// #include "calibrate.hpp"
#include "utils.hpp"

#ifdef AUDIT_INIT_COST 
extern ticks** init_costs;
extern volatile int g_thread_counter;
#endif

typedef struct ZCAProbeMetaData : ProbeMetaData {

  Address stub_address; // Address of the stub related to this probe
  uint8_t call_addr_offset;  // Offset within the stub at which the call address to 
                         // instrumentation function be found
  const char* expr;      // Decoded annotation string
  uint8_t tramp_call_size; // CALL instruction size of the trampoline call
                           // within the stub

} ZCAProbeMetaData;

class ZCAProbeProvider : public ProbeProvider {

  private:
    int thread_counter;
    int func_count;
#ifdef AUDIT_PROBES
    // Auditing
    ToggleStatistics** toggle_stats;    
#endif

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
    ZCAProbeProvider(Callback cb) : ProbeProvider(cb) {
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
    void registerProbe(ProbeMetaData* pmd);

    /// Gets ProbeMetaData entry for the given probe
    /*  \param probe_id The probe id of the probe
     */
    ProbeMetaData* getProbeMetaData(ProbeId probe_id); 

    ~ZCAProbeProvider() {

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

#endif /* _ZCA_PROBE_PROVIDER_HPP_ */
