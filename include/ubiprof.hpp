
#ifndef _PROFILER_HPP_
#define _PROFILER_HPP_

#include "instrumentor.hpp"

class Profiler {

  private:
    static Profiler instance;

  public:

    Profiler();

    // Callback function to be passed to probe layer so that 
    // profiler can take any action necssary when a probe is discovered.
    // This needs to be static so that it could be passed as a C
    // function pointer to the probe API.
    /* \param pmd Probe meta data returned from the probe layer
     */
    static void callback(ProbeMetaData* pmd);

    // Prolog instrumentation function
    /* Calls the prolog for given function with respective func_id
     * \param func_id
     */
    virtual void prolog(FuncId func_id) = 0;

    // Epilog instrumentation function
    /* Calls the prolog for given function with respective func_id
     * \param func_id
     */
    virtual void epilog(FuncId func_id) = 0;

    // Enable profiling for the given function 
    /* \param name Mangled name of the function
     * */ 
    virtual void profileFunction(std::string name);

    // Disables profiling for the given function
    /* param name Mangled name of the function
     */
    virtual void unprofileFunction(std::string name);

  protected:

    // Enable profiling for the given function 
    /* This is the version which is being used internally with 
     * instrumentation functions for efficiency reasons.
     * \param func_id The opaque function id
     * */
    virtual void profileFunction(FuncId func_id);

    // Disables profiling for the given function
    /* This is the version which is being used internally
     * with instrumentation functions for efficiency reasons.
     * \param func_id The opaque function id
     */
    virtual void unprofileFunction(FuncId func_id);

}

#endif /* _PROFILER_HPP_ */
