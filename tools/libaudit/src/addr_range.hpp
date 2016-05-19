
#ifndef ADDR_RANGE_H 
#define ADDR_RANGE_H 

#include <vector>

#include "defs.hpp"

namespace range {

  /// Describes a range in virtual memory address space.
  class Range {
    public:
      defs::Address start;
      defs::Address end;

      Range();
      Range(defs::Address start_addr, defs::Address end_addr);

      /// Get the range partitioned to blocks of given block_size.
      /* \param block_size The size of the block (typically page length)
       * \param aligned    If the returned ranges are to be aligned to the block 
       *                   boundaries (i.e: The first block would start at a 
       *                   block aligned address instead of the start of the 
       *                   range if it happens to be lying inside a block). Same
       *                   for the range end (i.e last range would end at a 
       *                   block boundary).
       *                   Make it false to start the first range from the 
       *                   actual range start irrespective of the start address 
       *                   placement. Also the last range will not be aligned 
       *                   to the last block boundary. 
       */
      std::vector<Range> getBlockedRange(int32_t block_size, bool aligned);

      /// Checks if the given address is within the current range
      /* \param addr      The virtual address to check
       * \param inclusive If the the range check is inclusive of range 
       *                  boundaries
       */
      bool withinRange(defs::Address addr, bool inclusive);

      /// Gets the distance from the current range. A negative value indicates
      /// that the address is lower in address space than the range.  
      /* \param addr  The address to get the distance from
       */
      int64_t distanceFromRange(defs::Address addr);

      /// Checks if the given range overlaps with the current range. Range 
      /// boundaries are included in the check.
      /* \param r The range to check
       */
      bool overlapsWith(Range r);

      /// Overloaded comparison operators for ranges
      bool operator<(const Range& r);

      bool operator>(const Range& r);

      bool operator==(const Range& r);

      bool operator!=(const Range& r);

      bool operator<=(const Range& r);

      bool operator>=(const Range& r);
  };

}

#endif /*ADDR_RANGE_H*/
