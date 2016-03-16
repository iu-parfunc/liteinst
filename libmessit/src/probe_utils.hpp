
#include <list>
#include <unordered_map>

#include "patcher.h"
#include "distorm.h"
#include "mnemonics.h"

typedef uint8_t* Address;

typedef struct MemIsland {
	uint8_t* start_addr;    // Memory island start address
	int32_t mem_chunk;            // 2^32 memory chunk this island belongs to
  //	bool allocated; 			      // Has this island been actually allocated
	uint8_t* insertion_ptr;       // Next stub insertion pointer
	uint64_t size;                // Size required for this memory island in bytes
	uint64_t unallocated_size;    // The size that was left unallocated due to memory constraints.
	                              // (e.g: due to fragmentation of vm address space)
	                              // Should be allocated to another mem_island at a different location a lazy manner
	int64_t remaining_size;      // Remaining size available
	uint8_t* last_probe_address;  // Address of the last probe serviced by this stub island.
} MemIsland;

typedef struct {
  Address addr;
  uint8_t size;
  uint64_t original;
  uint64_t patched;
} PICInstruction;

typedef struct {
  Address fn_addr;
  PICInstruction ins;
} Probe;

typedef std::unordered_map<uint32_t, std::list<MemIsland*>*> MemAllocTable;

Probe setupProbe(Address fn_addr, Address trampoline_fn_addr);
