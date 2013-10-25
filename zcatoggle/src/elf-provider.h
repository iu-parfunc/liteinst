
#include <string>

/** 
 * @brief Inspect an ELF binary and recover the probe locations from its headers.
 * 
 * 
 */


/** Read the probes available in an ELF binary.
 *
 * This examines data section headers to retrieve the ZCA probe
 * information (section .itt_notify_tab).
 */
void read_zca_probes(const char* filename);


/** Read the probes available in the CURRENT process.
 *
 * This attempts to locate the currently executing program on disk and
 * read its ELF headers, looking for a compiler-inserted section
 * containing a ZCA table.
 */
void read_self_zca_probes();

/** Return the current working path or an empty string upon failure.
 *
 */
std::string get_working_path(); 
