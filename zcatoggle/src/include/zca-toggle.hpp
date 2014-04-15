
/** 
 * 
 * @package ZCAtoggle
 * @brief Public interface to the ZCA Toggle library.
 * 
 * @details
 * Please see the whitepaper for more information on the underlying
 * zero-cost/low-cost annotation (ZCA) intrinsics:
 *
 *   http://www.cilkplus.org/sites/default/files/open_specifications/LowOverheadAnnotations.pdf
 * 
 * This library complements the compiler-provided
 * "__notify_intrinsic"; it provides the ability to read a ZCA
 * metadata table (.itt_notify section) from an ELF binary, and then
 * the to use such a table to dynamically inject and remove function
 * calls at preestablished probe points.
 * 
 * All exported functions are threadsafe.
 * 
 */

#ifndef _ZCA_TOGGLE_H_
#define _ZCA_TOGGLE_H_

#include <string>
#include <functional>

#ifdef __cplusplus
extern "C"
{
#endif

#include "zca-types.hpp"

extern const char *__progname; // Get the program name before the main. No argv[0] at this stage.

// extern ann_data* annotations;

// @brief  @details

/** A probe-able location in the instruction stream.
 *
 *  This is a a datatype representing a point in the instruction
 *  stream that can have a function call attached via `activateProbe`.
 */
typedef struct probe {
  const char* label;  // the label for this probe point, may be shared by many probe points
  char* addr;         // instruction address  
} probe_t;

/** A function that can be called from a `probe_t` site.
 * 
 *  A function called from a dynamically inserted probesite call
 *  receives two arguments: (1) the name or label of the probe, and
 *  (2) the dynamic argument received at the probe site.  These
 *  correspond to the two arguments provided to the
 *  `__notify_intrinsic` itself.  
 */
typedef void (*probe_callable_t)(const char* label, void* arg);

/** Inject a function call at a probe point.
 *
 *  Returns the success code.  Note that multiple consecutive calls to
 *  `activateProbe` on the *same* probe will result in the new
 *  callback function atomically replacing the old.
 */
extern int activateProbe(std::string label, void (*fun)());

/** Deactivate a probe.
 *  Returns the sucess code.
 */
extern int deactivateProbe(std::string label);

/** Runs at startup-time and reads the current binary's ELF headers.
 */
extern void initZCAService(); //__attribute__((constructor));

// TODO: the basic toggling functionality should be separated from the
// retriev-own-ELF-headers hack.

#ifdef __cplusplus
}
#endif
#endif  // _ZCA_TOGGLE_H_
