
#include "range_lock.hpp"
#include "lock.hpp"

#include <cassert>
#include <map>

namespace fastinst {
namespace rprobes {

using namespace lock;
using std::pair;
using std::unique_ptr;

void RangeLock::lockRange(Range r) {
  auto it = range_map.find(r);
  if (it != range_map.end()) {
    it->second.get()->lock();
  } else {
    range_map_lock.lock();
    unique_ptr<CASLock> lock(new CASLock());
    range_map.insert(pair<Range, unique_ptr<CASLock>>(r, move(lock)));
    range_map_lock.unlock();
  }
}

void RangeLock::unlockRange(Range r) {
  auto it = range_map.find(r);
  if (it != range_map.end()) {
    it->second.get()->unlock();
  } else {
    assert(false); // lockRange needs to be called before unlockRange
  }
}

} /* End rprobes */
} /* End fastinst */
