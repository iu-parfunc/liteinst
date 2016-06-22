
#include "range_lock.hpp"
#include "concurrency.hpp"

#include <cassert>
#include <map>

namespace liteinst {
namespace liteprobes {

using std::pair;
using std::unique_ptr;
using utils::concurrency::SpinLock;

void RangeLock::lockRange(Range r) {
  auto it = range_map.find(r);
  if (it != range_map.end()) {
    it->second.get()->lock();
  } else {
    range_map_lock.lock();
    unique_ptr<SpinLock> lock(new SpinLock(true));
    range_map.insert(pair<Range, unique_ptr<SpinLock>>(r, move(lock)));
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

} /* End liteprobes */
} /* End liteinst */
