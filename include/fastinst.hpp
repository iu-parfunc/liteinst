
#ifndef _PROBE_PROVIDER_HPP_
#define _PROBE_PROVIDER_HPP_

#include <cstdint>
#include <vector>
#include <atomic>
#include <string>
#include <cstdio>

/// This is used to specify which provider needs to be selected
/// at initialization time.
enum class ProviderType { ZCA, FINSTRUMENT, DTRACE, DYNINST };

/// In the future we will aim to support an open universe of probe
/// providers.  In the short term, we explicitly enumerate the probe
/// providers.  Any use of this field violates the abstraction of the
/// ProbeProvider by depending on implementation details.
enum class ProbeType { ZCA, FINSTRUMENT, DTRACE, DYNINST };

/// What is context of a particular probe location?  This is used by
/// profilers to assign some semantic meaning to probe.
enum class ProbeContext { ENTRY, EXIT, LINE_NUM, OTHER };

/// Currently ProbLoc is set to a line number (if context=LINE_NUM)
/// and to null otherwise.
typedef uint64_t ProbeLoc;

/// Probes are a four-state finite automata, and all transitions
/// between states must be atomic (i.e. have serialization points in
/// the code).
enum class ProbeState { UNINITIALIZED, INITIALIZING, ACTIVE, DEACTIVATED ,
  ACTIVATING, DEACTIVATING};

/// An opaque, unique probe identifier.  Do not depend on the
/// representation of this value.
typedef uint64_t ProbeId;

/// An opaque, unique function identifier.  Do not depend on the
/// representation of this value.
typedef uint64_t FuncId;

/// A byte addressible data type
typedef uint8_t* Address;

/// The argument passed at runtime to the Instrumentation_func.
/// Very often, this is the FuncId.
typedef uint64_t ProbeArg;

/// A byte sequence of 8 bytes
typedef uint64_t Sequence;

/// This is the type of function pointers for dynamically injected
/// function calls.  The job of the ProbeProvider
typedef void (*InstrumentationFunc) (ProbeArg context_id);

/// Atomic reference to an InstrumentationFunc function pointer
typedef std::atomic<InstrumentationFunc> InstrumentationFuncAtomic;

// Forward declare ProbeProvider class to be used inside ProbeMetaData
class ProbeProvider;

/// Everything we need to know about a newly discovered probe.
typedef struct ProbeMetaData {
  FuncId func_id;
  std::string func_name;
  ProbeId probe_id;
  std::string probe_name;
  Address probe_addr;
  ProbeType type;
  ProbeState state;
  ProbeContext probe_context;
  ProbeLoc     probe_loc;
  ProbeArg     probe_arg;
  Sequence active_seq;
  Sequence inactive_seq;
  InstrumentationFuncAtomic instrumentation_func;
  bool is_straddler;
  int  straddle_point;

  // The provider that "owns" this probe:
  ProbeProvider* provider;
} ProbeMetaData;

/// Probe meta data vector data type
typedef std::vector<ProbeMetaData*> ProbeVec;

/// The signature for a callback that registers a newly discovered
/// probe.  The ProbeProvider owns the ProbeMetadata record, so the
/// callback may read it, but should not free it.
///
/// This probe-discovery callback has two main obligations:
///
///  (1) Call initialize() to set up the constant argument to future
///      probe invocations.
///
///  (2) Call either activate() or deactivate() any number of times,
///      to leave the probe in a valid state by callback completion.
///
/// These methods can be called by accessing the owning ProbeProvider
/// through the ProbeMetadata pointer itself.
typedef void (*Callback) (const ProbeMetaData* pmd);


/// Implements an object that discovers probes and subsequently
/// provides the ability to toggle those probes.  When active, a
/// client-provided function-pointer is called from the probe site.
class ProbeProvider {

  protected:
    Callback callback;
    ProbeVec* probe_meta_data;

  public:

    /// Probe provider constructor
    /* Initialises with the callback
     * \param callback The callback function invoked at each probe discovery
     */
    ProbeProvider(Callback cb) : callback(cb) {
      probe_meta_data = new ProbeVec;
    }

    /// This method can be used to do probe provider specific further
    /// intializations.
    /* Specially any initialization which might require the
     * ProbeProvider instance to have been fully constructed. The default
     * implementation is empty.
     *
     */
    virtual void initializeProvider() {
    };

    /// This method registers a newly discovered probe with the proider.
    /* This is typically done by calling the registered probe callback.
     * The probe initialzation call will be done inside the callback.
     * \param pmd The probe metadata entry
     */
    virtual void registerProbe(ProbeMetaData* pmd) {
      (*callback)(pmd);
    };

    /// Move from UNINITIALIZED to INITIALIZING state.
    /* Begin initializing the probe, moving from UNINITIALIZED to
     * INITIALIZING state.  This function should not be called
     * concurrently, it should only be called on a single thread, and
     * specifically should be called from the probe discovery
     * Callback.
     *
     * This function cannot fail, but it may raise an exception if
     * preconditions are violated either in terms of the wrong
     * starting state or detection of contention.
     *
     * \param probe_arg A constant argument that is passed to EVERY
     * subsequent probe invocation, that is, every
     * Instrumentation_func which the probe is activated with.
     *
     */
    virtual void initialize(ProbeId probe_id, ProbeArg probe_arg) = 0;

    /// Activates the given probe.
    /* Activates the probe with given instrumentation function.
     *
     * Returns a code indicating success or failure.  If true is
     * returned, then the probe was successfully activated with the
     * given function.
     *
     * Failure can happen for a number of reasons.  Another thread may
     * have beat us to modifying the probe.  Also, this revision of
     * the API does not allow transitions *between* active states.
     * The probe must first be deactivated, before it may be activated
     * with a different instrumentation function.  This may be relaxed
     * in the future.
     *
     * It is an error to call this function before initialize
     * (UNINITIALIZED state).  INITIALIZING or DEACTIVATED states are
     * fine.
     *
     * \param probe_id The probe id opaque identifier
     * \param func The instrumentation function
     *
     * \return A boolean indicating success or failure.  True implies
     * that the probe was successfully activated.
     */
    virtual bool activate(ProbeId probe_id, InstrumentationFunc func) = 0;

    /// Activates the given probe asynchronously
    /* Activates the probe with given instrumentation function.
     *
     * \param probe_id The probe id opaque identifier
     * \param func The instrumentation function
     *
     * \return TODO
     * 
     */
    virtual bool activate_async(ProbeId probe_id, 
        InstrumentationFunc func) = 0;

    /// Force asynchronous probe activation to finish before returning
    /* Wait until the asynchronous protocol gets finished
     *
     * \param  The probe id: an opaque identifier.
     *
     * \return TODO
     */
    virtual void activate_async_finish(ProbeId probe_id) = 0;

    /// Deactivates the given probe
    /* Deactivate the probe, restoring the original functioality of
     * the machine code at the probe point.
     *
     * It is an error to call this function before initialize
     * (UNINITIALIZED state).  INITIALIZING or ACTIVE states are fine.
     *
     * \param  The probe id: an opaque identifier.
     *
     * \return A boolean indicating success or failure.  True implies
     * that the probe was successfully deactivated.
     */
    virtual bool deactivate(ProbeId probe_id) = 0;

    /// Deactivates the given probe
    /* Deactivate the probe, restoring the original functioality of
     * the machine code at the probe point.
     *
     * The deactivation protocol completion happens asyncronously.
     * Once the top half of the protocol is completed probe would transition to
     * DEACTIVATING. Once the bottom half is run probe will be in DEACTIVATED
     * state.
     *
     * \param  The probe id: an opaque identifier.
     *
     * \return TODO
     */
    virtual bool deactivate_async(ProbeId probe_id) = 0;

    /// Force asynchronous probe deactivation to finish before returning
    /* Wait until the asynchronous protocol gets finished
     *
     * \param  The probe id: an opaque identifier.
     *
     * \return TODO
     */
    virtual void deactivate_async_finish(ProbeId probe_id) = 0;

    /// Gets the number of functions in the application
    /*  Usually probe providers are privy to this information by reading
     *  ELF tables during the processing of probe sites. This method
     *  exposes the number of functions found during such initialization
     *  to the client. This can be really useful for initialization of
     *  client data structures for gathering statistics related to each
     *  function in more efficient manner (e.g: array instead
     *  of list) since the size of the data structure is known before hand.
     *  \return The number of functions discovered durig probe provider
     *  initialization. Returns -1 if the particular provider isn't capable
     *  of enumerating functions.
     */
    virtual uint64_t getNumberOfFunctions() = 0;

    virtual ~ProbeProvider() {
#ifdef AUDIT_PROBES
      uint64_t num_straddlers = 0;
      uint64_t num_probes     = probe_meta_data->size();
      uint64_t straddler_hist[8] = {0};
      uint64_t entry_straddlers = 0;
      uint64_t exit_straddlers  = 0;
      for (auto it = probe_meta_data->begin(); it != probe_meta_data->end();
          ++it) {
        ProbeMetaData* pmd = *it;
        if (pmd->is_straddler) {
          num_straddlers++;
          straddler_hist[pmd->straddle_point]++;

          if (pmd->probe_context == ProbeContext::ENTRY) {
            entry_straddlers++;
          } else if (pmd->probe_context == ProbeContext::EXIT) {
            exit_straddlers++;
          }
        }
      }

      FILE* fp = fopen("probe.audit", "a");

      fprintf(fp, "Straddler Report \n");
      fprintf(fp, "================ \n");
      fprintf(fp, "Straddlers (out of total probes)  : %lu/%lu\n",
          num_straddlers, num_probes);
      fprintf(fp, "Straddlers % (out of total probes) : %.2lf\n",
          (double) num_straddlers * 100/ num_probes);
      fprintf(fp, "Entry straddlers : %lu\n", entry_straddlers);
      fprintf(fp, "Exit straddlers : %lu\n", exit_straddlers);
      fprintf(fp, "Straddler histogram by straddle point\n");
      fprintf(fp, "-------------------------------------\n");
      for (int i=1; i < 8; i++) {
        fprintf(fp, "%d  :  %lu\n", i, straddler_hist[i]);
      }

      fclose(fp);
#endif

      // Release probe meta data entries
      for (auto it = probe_meta_data->begin(); it != probe_meta_data->end();
          ++it) {
        ProbeMetaData* pmd = *it;
        delete pmd;
      }

      delete probe_meta_data;
    };

    /// Gets the estimate of overhead induced by the instrumentation mechanism.
    /* This represents a current estimate of the time (in cycles)
     * required to get to and from the Instrumentation_func from the
     * application thread in which the probe is embedded.  Thus total
     * overhead to the application thread should be the time consumed
     * by the Instrumentation_func itself, plus this additional
     * (estimated) overhead.
     *
     */
    uint64_t instrumentationOverheadEstimate;
    // virtual uint64_t getInstrumentationOverheadEstimate(uint64_t num_invocations) = 0;
    /* This still needs some thinking over. Tricky to expose overhead of
     * probe providers such as ZCA since we don't have a hook function to account
     * for that before the control transfers to the instrumentation function.
     */

};

// ================================================================================
// Global probe provider object for profiling libraries and other use cases.

/// Initializes a global ProbeProvider instance of requested type.
/*  This implements one possible policy for creating a probe provider
 *  at application startup time.  This approach is limited because it
 *  recognized only a closed set of probe provider implementations.
 *
 *  It's an error to call this method more than once.
 *
 *  \param type The type of the ProbeProvider needed.
 *  \param callback The probe discovery callback.
 *  \return A reference to initialized ProbeProvider instance.
 */
extern ProbeProvider* initializeGlobalProbeProvider(ProviderType type, Callback callback);

/// Accessor function
/*  Gets the reference to ProbeProvider instance.
 *  \return A reference to ProbeProvider instance.
 *          The reference will be NULL if initializeGlobalProbeProvider has not been called.
 */
extern ProbeProvider* getGlobalProbeProvider();


/// Global probe provider instance. Allows plain C functions to accesss instrumentor
/// functions at runtime.
extern ProbeProvider* PROBE_PROVIDER;

#endif /* _PROBE_PROVIDER_HPP_ */
