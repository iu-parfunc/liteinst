
#ifndef _PROBE_API_H_
#define _PROBE_API_H_

#include "constants.h"

/*
 * C API for probe activation and deactivation 
 */

#ifdef __cplusplus
extern "C"  
{
#endif

/** 
 *  Description : Inject a function call at a probe point. Note that multiple consecutive calls to
 *  `activateProbe` on the *same* probe will return without any modifications being done if the passed function
 *  is the same as the already existing one. Otherwise probe callsite would be switched to the new function.
 *
 *  Parameters : 
 *    name - Probe name. For block level probes the convention is "block_name:start" or "block_name:end" to signify probes at beginning and 
 *           end of instrumented blocks.  
 * 
 *  Returns :  The success code 0 or negative values for faliures.  
 **/
extern int activate_probe(char* name);

/** 
 *  Description : Deactivate probe point for profiling. Usually this is done by overwriting NO-OPs to the probe call site or
 *  restoring probe point to the original condition before probe activation.
 *
 *  Parameters : 
 *    name - Probe name. For block level probes the convention is "block_name:start" or "block_name:end" to signify probes at beginning and 
 *           end of instrumented blocks.  
 *  
 *  Returns : success code 0 or negative values for faliures.
 * 
 **/
extern int deactivate_probe(char* name);

#ifdef __cplusplus
}
#endif
#endif 
