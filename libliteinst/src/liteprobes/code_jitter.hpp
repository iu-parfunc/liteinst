
#ifndef _CODE_JITTER_HPP_
#define _CODE_JITTER_HPP_

#include "liteprobes.hpp"

#include <memory>

namespace liteinst {
namespace liteprobes {

class CodeJitter {
  public:
    std::unique_ptr<Springboard> emitSpringboard(const CoalescedProbes& cp, 
        utils::Address target, ProbeContext& context, InstrumentationFunction fn);
    int64_t getTrampolineSize(int64_t relocation_size);

};

} // End liteprobes
} // End liteinst

#endif /* _CODE_JITTER_HPP_ */
