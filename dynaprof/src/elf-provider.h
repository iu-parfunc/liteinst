
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
#include "cycle.h"

//#ifdef __cplusplus
//extern "C"
//{
//#endif

// #include "zca-types.h"

extern const char *__progname;

extern unsigned long probe_start; // Address of the first probe

extern unsigned long probe_end; // Address of the last probe

typedef std::unordered_map<std::string, std::list<ann_data*>*> ann_table;

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

// void print_fn(char* annotation);

void print_fn2();

typedef struct prof_data {
	ticks start;
	int count;
	uint64_t min;
	uint64_t max;
	uint64_t sum;
} prof_data;

typedef std::map<std::string, prof_data*> function_stats;

typedef std::map<std::string, prof_data*> global_stats;

class Statistics {
public:
	function_stats f_stats;
	Statistics() {}
	~Statistics() {
		for (auto iter = f_stats.begin(); iter != f_stats.end(); iter++) {
			prof_data* data = iter->second;
			if (data != NULL) {
				free(data);
			}
		}
	}
};

// typedef typename std::aligned_storage<sizeof(function_stats*), std::alignment_of<function_stats*>::value>::type Storage;

#endif // _ELF_PROVIDER_H_
