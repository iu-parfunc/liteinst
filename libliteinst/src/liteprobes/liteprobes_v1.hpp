
#include <string>
#include <list>

#include "defs.hpp"
#include "process.hpp"
#include "range.hpp"
#include "liteinst.hpp"

namespace liteinst {
namespace liteprobes {

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

class Probe {

};

class ControlTransfer {
  public:
    utils::range::Range displaced;
    utils::range::Range relocated;
    // int relocation_offsets[];
    // PatchPoint patch_point;
};

class Trampoline {
  public:
};

} // End liteprobes
} // End liteinst
