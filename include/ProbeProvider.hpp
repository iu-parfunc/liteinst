
#ifndef _PROBE_PROVIDER_HPP_
#define _PROBE_PROVIDER_HPP_

#include <cstdint>

enum ProbeType { ZCA, FINSTRUMENT, DTRACE };
enum ProbeLoc { ENTRY, EXIT, LINE_NUM };
enum ProbeState { ACTIVE, DEACTIVE, INITIALIZING }

typedef uint64_t ProbeId;

typedef uint64_t FuncId;

typedef struct ProbeMetaData {
  FuncId func_id
  ProbeId probe_id;
  uint8_t* probe_addr;
  ProbeType type;
  ProbeLoc probe_loc;
  ProbeState state;
} ProbeMetaData;

typedef void (*Callback) (ProbeMetaData* pmd);

typedef void (*Instrumentation_func) (FuncId func_id);


class ProbeProvider {

  private:
    Callback* callback;

  public:
    // Probe provider constructor
    /* Initialises with the callback
     * \param callback The callback function invoked at each probe discovery
     */
    ProbeProvider(Callback callback);

    // Activates the given probe
    /* Activates the probe with given instrumentation function.
     * If it is already set returns false indicating the activation was skipped
     * \param probe_id The probe id opaque identifier
     * \param func The instrumentation function
     */
    virtual bool activate(ProbeId probe_id, Instrumentation_func func) = 0;

    // Deactivates the given probe
    /* \param The probe ide opaque identifier
     */ 
    virtual bool deactivate(ProbeId probe_id) = 0;

    /// Gets the estimate of overhead induced by the instrumentation mechanism.
    /* This still needs some thinking over.. Tricky to expose overhead of  
     * probe providers such as ZCA since we don't have a hook function to account
     * for that before the control transfers to the instrumentation function.     
     */
    virtual uint64_t getInstrumentationOverheadEstimate(uint64_t num_invocations) = 0;

}

#endif /* _PROBE_PROVIDER_HPP_ */
