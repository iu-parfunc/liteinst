
#ifndef RANGE_MAP_H
#define RANGE_MAP_H 

#include <map>
#include <vector>
#include <functional>

#include "addr_range.hpp"
#include "lock.hpp"
#include "defs.hpp"

namespace range { 

  class RangeBlock {
    friend class RangeMap;

    private:
      Range range;
      bool entry_present = false;

  };

  class EntryMetaData {
  };

  class RangeEntry {
    friend class RangeMap;

    public:
      Range entry_range;
      EntryMetaData* meta;

      RangeEntry(){}
      ~RangeEntry(){
        if (meta != NULL) {
          delete meta;
        }
      }

    /* Lock should be inaccessible to clients of RangeMap. Otherwise they can
     * monkey with it by directly calling lock operations on RangeEntry which 
     * would mess things up since we assume all the lock related operations are
     * happening under the control of RangeMap. Hence the callback mechanism. */
    private:
      lock::CASLock lock;
  };

  // RangeEntry is a pointer type since we need it to be downcastable. 
  typedef std::map<defs::Address, RangeEntry*> RangeEntries;
  typedef std::function<bool(std::vector<RangeEntry*>, Range range)> 
    UpdateEntriesCallback;

  class RangeMap {
    public:
      RangeMap(int32_t block_size);
      bool updateRangeEntries(Range r, UpdateEntriesCallback cb);   
      std::vector<Range> getBlockedRanges(Range r);
      ~RangeMap();

    private:
      RangeEntries entries;
      int32_t block_size;
      lock::CASLock global_lock;

      std::vector<RangeBlock> getRangeBlocks(Range r);
      std::vector<RangeEntry*> lockRange(Range r);
      bool unlockRange(Range r);
  };

}

#endif /*RANGE_MAP_H*/
