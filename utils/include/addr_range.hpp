
#ifndef ADDR_RANGE_H 
#define ADDR_RANGE_H 

#include <vector>

#include "defs.hpp"

namespace utils {
namespace range {

/// Describes a range in virtual memory address space.
class Range {
  public:
    utils::Address start;   ///< Start address of the range
    utils::Address end;     ///< Ending address of the range

    static const uint8_t START;
    static const uint8_t END;
    static const uint8_t EXCLUSIVE;
    static const uint8_t INCLUSIVE;

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
     *  \param inclusive If the the range check is inclusive of current range 
     *                  boundaries. INCLUSIVE, EXCLUSIVE, START and END flags or 
     *                  combination thereof are valid inputs.
     *
     *  \return If the given address is within the range
     */
    bool withinRange(utils::Address addr, uint8_t inclusive);

    /** \brief Gets the distance from the current range. 
     *  \param addr  The address to get the distance from
     *  \return The distance from the current range. A negative value indicates
     *    that the address is lower in address space than the range.    
     */
    int64_t distanceFromRange(utils::Address addr);

    /** \brief Checks if the given range overlaps with the current range.
     *  \param r         The range to check
     *  \param inclusive If the the range check is inclusive of current range 
     *                  boundaries. INCLUSIVE, EXCLUSIVE, START and END flags or 
     *                  combination thereof are valid inputs.
     *
     *  \return If the given range overlaps with the current range
     */
    bool overlapsWith(const Range r, uint8_t inclusive);

    bool includes(utils::Address from, utils::Address to);

    /** \brief Unions the given range with the current one. 
     *  \param r The range to be unioned to the current one.
     *
     *  Current range boundaries are modified in order contain the union of 
     *  given range and the existing range.
     */ 
    void unionRange(const Range r);

    /** \brief Gets the size of the range.
     */
    int64_t size();

    /// Overloaded comparison operators for ranges
    bool operator<(const Range& r);

    bool operator>(const Range& r);

    bool operator==(const Range& r) const;

    bool operator!=(const Range& r);

    bool operator<=(const Range& r);

    bool operator>=(const Range& r);
};

class RangeHash {
  public:
    std::size_t operator()(const Range& r) const {
      // From : http://goo.gl/VcV6w0
      return ((std::hash<utils::Address>()(r.start) ^
              (std::hash<utils::Address>()(r.end) << 1)) >> 1);
    }
};

} // End range 
} // End utils 

#endif /*ADDR_RANGE_H*/
