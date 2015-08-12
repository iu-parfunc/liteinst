
#ifndef _PROBE_PROVIDER_HPP_
#define _PROBE_PROVIDER_HPP_

#include <cstdint>

/// In the future we will aim to support an open universe of probe
/// providers.  In the short term, we explicitly enumerate the probe
/// providers.  Any use of this field violates the abstraction of the
/// ProbeProvider by depending on implementation details.
enum ProbeType { ZCA, FINSTRUMENT, DTRACE, DYNINST };

/// What is context of a particular probe location?  This is used by
/// profilers to assign some semantic meaning to probe.
enum ProbeContext { ENTRY, EXIT, LINE_NUM };

/// Currently ProbLoc is set to a line number (if context=LINE_NUM)
/// and to null otherwise.
typedef uint64_t ProbeLoc;

/// Probes are a four-state finite automata, and all transitions
/// between states must be atomic (i.e. have serialization points in
/// the code).
enum ProbeState { UNINITIALIZED, INITIALIZING, ACTIVE, DEACTIVATED };

/// An opaque, unique probe identifier.  Do not depend on the
/// representation of this value.
typedef uint64_t ProbeId;

/// An opaque, unique function identifier.  Do not depend on the
/// representation of this value.
typedef uint64_t FuncId;

/// Everything we need to know about a newly discovered probe.
typedef struct ProbeMetaData {
  FuncId func_id;
  ProbeId probe_id;
  uint8_t* probe_addr;
  ProbeType type;
  ProbeState state;
  ProbeContext probe_context;
  ProbeLoc     probe_loc;
} ProbeMetaData;


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

/// This is the type of function pointers for dynamically injected
/// function calls.  The job of the ProbeProvider
typedef void (*Instrumentation_func) (FuncId func_id);

/// The argument passed at runtime to the Instrumentation_func.
/// Very often, this is the FuncId.
typedef uint64_t ProbeArg;

/// Implements an object which discovers probes and subsequently
/// provides the ability to toggle those probes.
class ProbeProvider {

  private:
    Callback* callback;

  public:
    /// Probe provider constructor
    /* Initialises with the callback
     * \param callback The callback function invoked at each probe discovery
     */
    ProbeProvider(Callback callback);


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
    virtual void initialize(ProbeArg probe_arg);

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
    virtual bool activate(ProbeId probe_id, Instrumentation_func func) = 0;

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


#endif /* _PROBE_PROVIDER_HPP_ */
