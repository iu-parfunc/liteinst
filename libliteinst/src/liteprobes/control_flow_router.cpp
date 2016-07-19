
#include "control_flow_router.hpp"

using std::map;
using std::list;
using std::unique_ptr;
using utils::Address;
using utils::range::Range;

// A global function with C linkage
void liteprobes_sigill_handler(int signum, siginfo_t* siginfo, void* context) {
  // Get the interrupted instruction
  ucontext_t *ucontext = (ucontext_t*)context;
  Address interrupted_addr = (Address)(ucontext->uc_mcontext.gregs[REG_RIP]);

  // Reroute to the new address within a springboard
  Address reroute_addr = liteinst::liteprobes::
    ControlFlowRouter::getRerouteAddress(interrupted_addr);
  ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)reroute_addr;
}

namespace liteinst {
namespace liteprobes { 

utils::concurrency::ReadWriteLock ControlFlowRouter::lock;
map<Address, unique_ptr<Springboard>> ControlFlowRouter::route_map;

void ControlFlowRouter::addSpringboard(unique_ptr<Springboard> sb) {
  lock.writeLock();
  route_map.emplace(sb->base, move(sb));
  lock.writeUnlock();
}

void ControlFlowRouter::removeSpringboard(Springboard* sb) {
  lock.writeLock();
  route_map.erase(sb->base);
  lock.writeUnlock();
}

Springboard* ControlFlowRouter::getSpringboard(Address address) {
  lock.readLock();
  auto it = route_map.find(address);
  if (it != route_map.end()) {
    lock.readUnlock();
    return it->second.get();
  }

  lock.readUnlock();
  return nullptr;
}

Springboard* ControlFlowRouter::getContainingSpringboard(Address address) {
  lock.readLock();

  Springboard* sb = nullptr;
  if (route_map.size() == 0) {
    lock.readUnlock();
    return sb;
  }

  auto it = route_map.lower_bound(address);

  if (it == route_map.end() ||
      !(it == route_map.begin() || it->second->base == address)) {
    --it;
  }

  if (it->second->displaced.withinRange(address, Range::START)) {
    sb = it->second.get();
  }

  lock.readUnlock();
  return sb;
}

list<Springboard*> ControlFlowRouter::getOverlappingSpringboards(Range r) {
  list<Springboard*> sbs;
  lock.readLock();

  if (route_map.size() == 0) {
    lock.readUnlock();
    return sbs;
  }

  auto it = route_map.lower_bound(r.start);

  if (it == route_map.end() ||
      !(it == route_map.begin() || it->second->base == r.start)) {
    --it;
  }

  // Now iterate until we get all springboards overlapping the range 
  while (it != route_map.end() && it->second->displaced.overlapsWith(r, 
        Range::EXCLUSIVE)) {
    sbs.push_back(it->second.get());
    ++it; 
  }

  lock.readUnlock();
  return sbs;
}

Address ControlFlowRouter::getRerouteAddress(Address address) {
  Springboard* sb = getContainingSpringboard(address);
  assert(sb != nullptr);

  int offset = address - sb->base;

  int index = 0;
  do {
    if (offset == sb->instruction_offsets[index]) {
      break;
    }
  } while (++index < sb->n_relocated);
      
  int relocation_offset = sb->relocation_offsets[index];
  return sb->range.start + relocation_offset;
}

} // End liteprobes
} // End liteinst
