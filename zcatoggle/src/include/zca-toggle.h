
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
#ifdef __cplusplus
extern "C"
{
#endif


//! @brief Architure definition of the MUX
//! @details More details about this mux element.



/** Inject a function call at a probe point.
 *  Returns the success code.
 */
extern int activateProbe(const char* ann, void (*fptr)(const char*, void*));

/** Runs at startup-time and reads the current binary's ELF headers.
 */
extern void initZCAService() __attribute__((constructor));

// TODO: the basic toggling functionality should be separated from the
// retriev-own-ELF-headers hack.

#ifdef __cplusplus
}
#endif
#endif  // _ZCA_TOGGLE_H_
