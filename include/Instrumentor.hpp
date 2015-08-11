
#ifndef _INSTRUMENTOR_HPP_
#define _INSTRUMENTOR_HPP_

#include <cstdint>
#include <list>

#include "ProbeProvider.hpp"

class Instrumentor {

  private:
    Callback callback;
    std::list<ProbeMetaData*> pmd;

  public :

    /// Instrumentor constructor.
    /* Initialises with the callback
     * \param callback The callback function invoked at each probe discovery
     */
    Instrumentor(Callback callback);

    /// Activates profiling for the probe with given probe id.
    /** Opaque probe id obtained via the callback registered at init time
     *  or by doing a getProbes() call needs to be passed here.
     *  \param probe_id Probe id of the given probe 
     *  \param func Instrumentation function to be associated with this probe site.
     *              If one is already associated with the current site returns false
     *              to indicate that modification was skipped.
     */
    bool activateProbe(ProbeId probe_id, Instrumentation_func func);

    /// Deactivates profiling for the probe with given probe id.
    /** Opaque probe id obtained via the callback registered at init time
     *  or by doing a getProbes() call needs to be passed here.
     *  \param probe_id Probe id of the given probe 
     */
    bool deactivateProbe(ProbeId probe_id);

    /// Gets the estimate of overhead induced by the instrumentation mechanism.
    /* This still needs some thinking over.. Tricky to expose overhead of various
     * mechanisms specially now we have to return it as an aggregated value in the
     * case where there can be multiple probe provider backends at same time.
     */
    uint64_t getInstrumentationOverheadEstimate(uint64_t num_invocations);

    /// Registers probe with the instrumentor 
    /* Probe providers would use this function to register the probes discovered
     * with the Instrumentor
     * \param type Type of the probe. Currently one of ZCA| FINSTRUMENT.
     * \param pmd  Meta data about the probe
     * */
    void registerProbe(ProbeType type, ProbeMetaData* pmd);

    /// Gets the registered probes 
    /* Returns the registered probe up to now. May only be a partial list of available 
     * probe sites in the application.
     * (e.g: In the case of instrumentors which dynamically discover probes at runtime)
     */
    std::list<ProbeMetaData*> getProbes();

    /// Gets the discovered function count
    /* This is useful for efficient initialization of stat collection data structures
     * at potential consumers of the Instrumentor API such as profilers. 
     */
    uint64_t getFunctionCount();

    ~Instrumentor();

  protected:
    long func_count = 0; ///< Total number of functions being profiled

};

#endif /* _INSTRUMENTOR_HPP_ */
