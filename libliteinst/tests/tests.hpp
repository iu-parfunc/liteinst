
#ifndef _LITEPROBE_TESTS_HPP_
#define _LITEPROBE_TESTS_HPP_

#include <signal.h>
#include <stdint.h>

extern "C" void dummy();

extern volatile sig_atomic_t tear_down;
extern volatile int32_t entry_counter;
extern volatile int32_t exit_counter;

#endif /* _LITEPROBE_TESTS_HPP_ */
