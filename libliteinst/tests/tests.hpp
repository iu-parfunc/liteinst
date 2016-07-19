
#ifndef _LITEPROBE_INJECTOR_HPP_
#define _LITEPROBE_INJECTOR_HPP_

#include <signal.h>
#include <stdint.h>

extern volatile sig_atomic_t flag;
extern volatile int32_t entry_counter;
extern volatile int32_t exit_counter;

#endif /* _LITEPROBE_INJECTOR_HPP_ */
