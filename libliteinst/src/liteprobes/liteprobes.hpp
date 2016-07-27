
#ifndef _LITEPROBES_HPP_
#define _LITEPROBES_HPP_

#include <string>
#include <list>
#include <cstdint>
#include <map>
#include <memory>

#include "defs.hpp"
#include "process.hpp"
#include "addr_range.hpp"
#include "liteinst.hpp"

namespace liteinst {
namespace liteprobes {

typedef int offset_t;

struct ShortCircuit {
  utils::Address start;
  int size;
  uint64_t off_state;
  uint64_t on_state;
};

struct ContextSave {
  utils::Address start;
  int size;
};

struct Args {
  utils::Address start;
  offset_t pg_id_offset;
  offset_t p_id_offset;
  offset_t i_id_offset;
  offset_t placement_offset;
  offset_t u_regs_offset;
  offset_t address_offset;
  int size;
};

struct Call {
  utils::Address start;
  utils::Address target;
  int size;
};

struct ContextRestore {
  utils::Address start;
  int size;
};

struct Return {
  utils::Address start;
  utils::Address target;
  int size;
};

struct Callout {
  utils::Address start;
  ShortCircuit short_circuit;
  ContextSave ctx_save;
  Args args;
  Call call;
  ContextRestore ctx_restore;
};

class Springboard; // Forward declaration 

class Probe {
  public:
    ProbeId p_id;
    utils::Address address;
    ProbeContext context;
    Springboard* trampoline;
};

class ProbeGroup {
  public:
    std::string name;
    ProbeGroupId pg_id;
    utils::process::Function* fn;
    std::map<utils::Address, ProbePlacement> probe_sites;
    std::map<utils::Address, Probe*> probes;
    utils::Address start;

    ProbeGroup(std::string name) : name(name) {
    }
};

class CoalescedProbes {
  public:
    std::map<utils::Address, Probe*> probes;
    utils::range::Range range;
    std::list<Springboard*> springboards;
    bool is_end_a_control_transfer;
};

enum class SpringboardType {
  TRAMPOLINE,
  SUPER_TRAMPOLINE
};

class Springboard {
  public:
    SpringboardType type;
    utils::Address base;
    int32_t probe_length;    
    bool is_probe_ready;
    std::map<utils::Address, Probe*> probes;
    std::map<utils::Address, uint8_t> saved_probe_heads;
    utils::range::Range displaced;
    utils::range::Range range;
    uint64_t punned;
    uint64_t original;
    uint64_t marked_probe_heads;
    int n_relocated;
    int* relocation_offsets;
    int* instruction_offsets;
    std::map<utils::Address, std::unique_ptr<Callout>> callouts;
    Return control_return;
    // PatchPoint patch_point;
    int n_probes;
    int active_probes;
    utils::concurrency::SpinLock lock;

    Callout* getCalloutForProbe(utils::Address addr) {
      auto it = callouts.find(addr);
      if (it != callouts.end()) {
        return it->second.get();
      } 
      return nullptr;
    }

    Springboard() {
    }

    ~Springboard() {
    }
};

} // End liteprobes
} // End liteinst

#endif /* _LITEPROBES_HPP_ */
