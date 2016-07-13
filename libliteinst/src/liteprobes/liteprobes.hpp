
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
  int jump_length;
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

struct Callout {
  utils::Address start;
  ShortCircuit short_circuit;
  ContextSave ctx_save;
  Args args;
  Call call;
  ContextRestore ctx_restore;
};

class ProbeGroup {
  public:
    std::string name;
    ProbeGroupId pg_id;
    utils::process::Function* fn;
    std::list<utils::Address> probes;
    utils::Address start;

    ProbeGroup(std::string name) : name(name) {
    }
};

class Trampoline {
};

enum class SpringboardType {
  TRAMPOLINE,
  SUPER_TRAMPOLINE
};

class Springboard {
  public:
    SpringboardType type;
    utils::Address base;
    int32_t relative_jump;
    int32_t probe_length;    
    bool is_probe_ready;
    std::list<utils::Address> probed_addrs;
    utils::range::Range displaced;
    utils::range::Range range;
    uint8_t punned_bytes[8];
    uint8_t original_bytes[8];
    uint8_t instruction_marked_bytes[8];
    int* relocation_offsets;
    std::map<utils::Address, std::unique_ptr<Callout>> callouts;
    // PatchPoint patch_point;
};


class Probe {
  public:
    ProbeId p_id;
    ProbeContext context;
    Trampoline* trampoline;
};

class CoalescedProbes {
  public:
    std::list<utils::Address> probes;
    utils::range::Range range;
    std::list<Springboard*> springboards;
};


} // End liteprobes
} // End liteinst

#endif /* _LITEPROBES_HPP_ */
