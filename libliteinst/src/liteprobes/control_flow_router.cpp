
#include "control_flow_router.hpp"

using std::list;
using std::unique_ptr;
using utils::Address;
using utils::range::Range;

// A global function with C linkage
void liteprobes_sigill_handler(int signum, siginfo_t* siginfo, void* context) {
  // Get the interrupted instruction
  ucontext_t *ucontext = (ucontext_t*)context;
  Address interrupted_addr = (Address)(ucontext->uc_mcontext.gregs[REG_RIP]-1);

  // Reroute to the new address within a springboard
  Address reroute_addr = liteinst::liteprobes::
    ControlFlowRouter::getRerouteAddress(interrupted_addr);
  ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)reroute_addr;
}

namespace liteinst {
namespace liteprobes { 

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
  if (it == route_map.begin()) {
    ; // Fall through
  } else if (it->second->base == address) {
    sb = it->second.get();
  } else {
    --it;
    if (it->second->range.withinRange(address, true)) {
      sb = it->second.get();
    } 
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
  if (it == route_map.begin() || it->second->base == r.start) {
    ; // Fall through
  } else {
    --it; // Backup one time to get to the springboard containing range start
  }

  // Now iterate until we get all springboards overlapping the range 
  while (it->second->range.overlapsWith(r) && it != route_map.end()) {
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
  int relocation_offset = sb->relocation_offsets[offset];
  return sb->range.start + relocation_offset;
}

} // End liteprobes
} // End liteinst
