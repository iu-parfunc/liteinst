
#ifndef _LITEPROBE_PROVIDER_HPP_
#define _LITEPROBE_PROVIDER_HPP_

#include <atomic>
#include <stack>
#include <unordered_map>

#include "liteprobes.hpp"
#include "addr_range.hpp"

namespace liteinst {
namespace liteprobes {

enum CoordinateType {
  FUNCTION,
  LOOP,
  BASIC_BLOCK,
  OFFSET,
  LINE_NUM,
  INS_TYPE
};

// typedef std::unordered_map<Range, ControlTransfer> ControlTransferMap;

typedef std::unordered_map<std::string, ProbeGroup*> ProbeGroupByName;
typedef std::vector<std::unique_ptr<ProbeGroup>> ProbeGroupVec; 
typedef std::vector<std::unique_ptr<ProbeRegistration>> ProbeRegistrationVec;

class LiteProbeProvider : public ProbeProvider {
  public:
    static std::unordered_map<utils::Address, Probe*> probes;

    LiteProbeProvider(Callback cb) : ProbeProvider(cb) {
      /*
      if (!is_initialized) {
        init_lock.lock();

        if (!is_initialized) {
          is_initialized = true;
        }
      }
      */
    }

    ProbeRegistration registerProbes(Coordinates coords,
        std::string instrumentation_provider);

    bool activate(ProbeInfo ctx);
    bool deactivate(ProbeInfo ctx);

    bool activate(ProbeGroupInfo ctx);
    bool deactivate(ProbeGroupInfo ctx);

    bool activate(ProbeRegistration registration);
    bool deactivate(ProbeRegistration registraiton);

  private:
    static ProbeGroupVec probe_groups;
    static ProbeGroupByName pg_by_name;
    static ProbeRegistrationVec probe_registrations;

    bool is_initialized = false;
    utils::concurrency::SpinLock init_lock;

    utils::concurrency::ReadWriteLock meta_data_lock;

    std::list<ProbeGroup*> generateProbeGroups(Coordinates orginal, 
        Coordinates specific, std::stack<CoordinateType> block_coords,
        std::string probe_group_name);

    ProbeGroup* generateProbeGroupForFunction(utils::process::Function* fn, 
        Coordinates coord, std::string probe_group_name);

    ProbeGroup* generateProbeGroupForBasicBlock(utils::process::Function* fn,
        utils::process::BasicBlock* bb, Coordinates coord, 
        std::string probe_group_name);
 
    // std::map<int, InstrumentationProvider> i_providers;
    // ControlTransferMap control_transfers;

}; 

} /* End liteprobes */
} /* End liteinst */

#endif /* _LITEPROBE_PROVIDER_HPP_ */
