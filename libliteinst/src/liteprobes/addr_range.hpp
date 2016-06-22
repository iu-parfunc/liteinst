
#ifndef ADDR_RANGE_H 
#define ADDR_RANGE_H 

#include <vector>

#include "utils.hpp"

namespace liteinst {
namespace liteprobes {

/// Describes a range in virtual memory address space.
class Range {
  public:
    utils::Address start;   ///< Start address of the range
    utils::Address end;     ///< Ending address of the range

    Range();
    Range(utils::Address start_addr, utils::Address end_addr);

    /** \brief Get the range partitioned to blocks of given block_size.
     *  \param block_size The size of the block (typically page length)
     *  \param aligned    If the returned ranges are to be aligned to the block 
     *                    boundaries (i.e: The first block would start at a 
     *                    block aligned address instead of the start of the 
     *                    range if it happens to be lying inside a block). Same
     *                    for the range end (i.e last range would end at a 
     *                    block boundary).
     *                    Make it false to start the first range from the 
     *                    actual range start irrespective of the start address 
     *                    placement. Also the last range will not be aligned 
     *                    to the last block boundary. 
     *  \return Partitions of the range blocked to given block size.
     */
    std::vector<Range> getBlockedRange(int32_t block_size, bool aligned);

    /** \brief Checks if the given address is within the current range
     *  \param addr      The virtual address to check
     *  \param inclusive If the the range check is inclusive of range 
     *                  boundaries
     *  \return If the given address is within the range
     */
    bool withinRange(utils::Address addr, bool inclusive);

    /** \brief Gets the distance from the current range. 
     *  \param addr  The address to get the distance from
     *  \return The distance from the current range. A negative value indicates
     *    that the address is lower in address space than the range.    
     */
    int64_t distanceFromRange(utils::Address addr);

    /** \brief Checks if the given range overlaps with the current range.
     *  \param r The range to check
     *
     *  Range boundaries are included in the check.
     */
    bool overlapsWith(const Range r);

    /// Overloaded comparison operators for ranges
    bool operator<(const Range& r);

    bool operator>(const Range& r);

    bool operator==(const Range& r);

    bool operator!=(const Range& r);

    bool operator<=(const Range& r);

    bool operator>=(const Range& r);
};

} // End liteprobes 
} // End liteinst 

#endif /*ADDR_RANGE_H*/
