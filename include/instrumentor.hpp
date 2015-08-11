
#ifndef _INSTRUMENTOR_HPP_
#define _INSTRUMENTOR_HPP_

#include <cstdint>
#include <list>

enum ProbeType { ZCA, FINSTRUMENT, DTRACE };
enum ProbeLoc { ENTRY, EXIT, LINE_NUM };

typedef uint64_t ProbeId;

typedef uint64_t FuncId;

typedef struct ProbeMetaData {
  ProbeId probe_id;
  uint8_t* probe_addr;
  ProbeType type;
  ProbeLoc probe_loc;
} ProbeMetaData;

typedef void (*Callback) (ProbeMetaData* pmd);

typedef void (*Instrumentation_func) (FuncId func_id);

class Instrumentor {

  private:
    Callback callback;
    std::list<ProbeMetaData*> pmd;

  public :

    /// Activates profiling for the probe with given probe id.
    /** Opaque probe id obtained via the callback registered at init time
     *  or by doing a getProbes() call needs to be passed here.
     *  \param probe_id Probe id of the given probe 
     */
    virtual bool activateProbe(ProbeId probe_id, ) = 0;

    /// Deactivates profiling for the probe with given probe id.
    /** Opaque probe id obtained via the callback registered at init time
     *  or by doing a getProbes() call needs to be passed here.
     *  \param probe_id Probe id of the given probe 
     */
    virtual bool deactivateProbe(ProbeId probe_id) = 0;

    /// Gets the estimate of overhead induced by the instrumentation mechanism.
    /* This still needs some thinking over.. Tricky to expose overhead of various
     * mechanisms specially now we have to return it as an aggregated value in the
     * case where there can be multiple probe provider backends at same time.
     */
    virtual uint64_t getInstrumentationOverheadEstimate(uint64_t num_invocations) = 0;

    /// Registers probe with the instrumentor 
    virtual void registerProbe(ProbeType type, ProbeMetaData* pmd) = 0;

    /// Gets the registered probes 
    /* Returns the registered probe up to now. May only be a partial list of available 
     * probe sites in the application.
     * (e.g: In the case of instrumentors which dynamically discover probes at runtime)
     */
    virtual std::list<ProbeMetaData*> getProbes();

    /// Gets the discovered function count
    /* This is useful for efficient initialization of stat collection data structures
     * at potential consumers of the Instrumentor API such as profilers. 
     */
    virtual uint64_t getFunctionCount();

    virtual ~Instrumentor() {}

  protected:
    long func_count = 0; ///< Total number of functions being profiled

};

#endif /* _INSTRUMENTOR_HPP_ */
