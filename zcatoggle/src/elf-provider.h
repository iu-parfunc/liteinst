
/**
 * @brief Inspect an ELF binary and recover the probe locations from its headers.
 *
 *
 */

#ifndef _ELF_PROVIDER_H_
#define _ELF_PROVIDER_H_

#include <string>
#include <unordered_map>
#include <map>
#include <list>
#include "zca-types.hpp"

//#ifdef __cplusplus
//extern "C"
//{
//#endif

// #include "zca-types.h"

extern const char *__progname;

extern unsigned long probe_start; // Address of the first probe

extern unsigned long probe_end; // Address of the last probe

typedef std::unordered_map<std::string, ann_data*> ann_table;

extern ann_table annotations;

typedef std::map<uint32_t, std::list<mem_island*>*> mem_alloc_table;

extern mem_alloc_table mem_allocations;

/** Read the probes available in an ELF binary.
 *
 * This examines data section headers to retrieve the ZCA probe
 * information (section .itt_notify_tab).
 *
 * return Number of probes discovered
 */
int read_zca_probes(const char* filename);

/** Read the probes available in the current process.
 *
 * This examines data section headers to retrieve the ZCA probe
 * information (section .itt_notify_tab).
 *
 * return Number of probes discovered
 */
int read_self_zca_probes();

//#ifdef __cplusplus
//}
//#endif

/** Read the probes available in the CURRENT process.
 *
 * This attempts to locate the currently executing program on disk and
 * read its ELF headers, looking for a compiler-inserted section
 * containing a ZCA table.
 *
 * return Number of probes discovered
 */
// int read_self_zca_probes();

/** Return the current working path or an empty string upon failure.
 *
 */
void get_working_path(char* buf);

void print_fn();

void print_fn2();

#endif // _ELF_PROVIDER_H_
