
#ifndef RANGE_MAP_H
#define RANGE_MAP_H 

#include <map>
#include <vector>
#include <functional>

#include "addr_range.hpp"
#include "lock.hpp"
#include "defs.hpp"

namespace range { 

  /// Range information corresponding to a block with some additional meta data
  /// added for RangeMap internal use. 
  class BlockRange {
    friend class RangeMap;

    private:
      Range range;
      bool entry_present = false; ///< If the corresponding block meta data 
                                  ///< entry is present
  };

  /// Meta data about a block 
  /* This is a marker interface which is to implemented by clients to 
   * incoporate specific meta data as required.
   */
  class BlockMetaData {
  };

  /// Meta data container for a given block 
  class BlockEntry {
    friend class RangeMap;

    public:
      Range entry_range;
      BlockMetaData* metadata = NULL;

      BlockEntry(){}
      ~BlockEntry(){
        if (metadata != NULL) {
          delete metadata;
        }
      }

    /* Lock should be inaccessible to clients of RangeMap. Otherwise they can
     * monkey with it by directly calling lock operations on RangeEntry which 
     * would mess things up since we assume all the lock related operations are
     * happening under the control of RangeMap. Hence the callback mechanism. */
    private:
      lock::CASLock lock;
  };

  /// Mapping from block start addresses to block metadata. 
  /// RangeEntry is a pointer type since we need it to be downcastable. 
  typedef std::map<defs::Address, BlockEntry*> BlockEntries;

  /// Callback type for updating block entries corresponding to a given range.
  typedef std::function<bool(std::vector<BlockEntry*>, Range range)> 
    UpdateEntriesCallback;

  /// RangeMap is a concurrent map holding information about ranges partitioned 
  /// according to a given block size.   
  class RangeMap {
    public:
      RangeMap(int32_t block_size);
      
      /// Updates the block entries corresponding to the given range. 
      /*  Guanrantees that block entries will be updated atomically with the 
       *  given callback.
       * \param r  The range to be updated.
       * \param cb Callback which will be invoked to update the range entries. 
       */
      bool updateRangeEntries(Range r, UpdateEntriesCallback cb);   
      ~RangeMap();

    private:
      BlockEntries entries; ///< Block entry mappings
      int32_t block_size;  
      lock::CASLock global_lock; ///< Lock to ensure mutual exclusive access to
                                 ///< entries map

      /// Gets meta data for the blocked range    
      std::vector<BlockRange> getBlockedRangeMetaData(Range r);

      /// Locks the address range for mutual exclusive updates  
      /* \param r Range to be locked
       */
      std::vector<BlockEntry*> lockRange(Range r);

      /// Unlocks the address range.
      /* \param r Range to be unlocked
       */
      bool unlockRange(Range r);
  };

}

#endif /*RANGE_MAP_H*/
