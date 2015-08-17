
#ifndef _PROBE_PROVIDER_HPP_
#define _PROBE_PROVIDER_HPP_

#include <cstdint>
#include <vector>
#include <atomic>
#include <string>

/// This is used to specify whichprovider needs to be selected
/// at initialization time.  
enum class ProviderType { ZCA, FINSTRUMENT, DTRACE, DYNINST };

/// In the future we will aim to support an open universe of probe
/// providers.  In the short term, we explicitly enumerate the probe
/// providers.  Any use of this field violates the abstraction of the
/// ProbeProvider by depending on implementation details.
enum class ProbeType { ZCA, FINSTRUMENT, DTRACE, DYNINST };

/// What is context of a particular probe location?  This is used by
/// profilers to assign some semantic meaning to probe.
enum class ProbeContext { ENTRY, EXIT, LINE_NUM };

/// Currently ProbLoc is set to a line number (if context=LINE_NUM)
/// and to null otherwise.
typedef uint64_t ProbeLoc;

/// Probes are a four-state finite automata, and all transitions
/// between states must be atomic (i.e. have serialization points in
/// the code).
enum class ProbeState { UNINITIALIZED, INITIALIZING, ACTIVE, DEACTIVATED };

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
typedef void (*Callback) (const ProbeMetaData* pmd);


/// Implements an object which discovers probes and subsequently
/// provides the ability to toggle those probes.
class ProbeProvider {

  protected:
    Callback callback;
    ProbeVec probe_meta_data;

  public:
    /// Probe provider constructor
    /* Initialises with the callback
     * \param callback The callback function invoked at each probe discovery
     */
    ProbeProvider(Callback callback);

    /// Initializes the ProbeProvider instance of requested type
    /*  It's an error to call this method more than once. 
     *  \param type The type of the ProbeProvider needed.
     *  \param callback The probe discovery callback.
     *  \return A reference to initialized ProbeProvider instance.
     */
    static ProbeProvider* initializeProbeProvider(ProviderType type, Callback callback);
    
    /// Gets the reference to ProbeProvider instance.
    /*  \return A reference to ProbeProvider instance.
     */
    static ProbeProvider* getProbeProvider();

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
    virtual void initialize(ProbeId probe_id, ProbeArg probe_arg);

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
     */
    virtual bool activate(ProbeId probe_id, InstrumentationFunc func) = 0;

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

/// Global probe provider instance. Allows plain C functions to accesss instrumentor
/// functions at runtime.
extern ProbeProvider* PROBE_PROVIDER;

#endif /* _PROBE_PROVIDER_HPP_ */
