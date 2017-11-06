
#ifndef _CODE_JITTER_HPP_
#define _CODE_JITTER_HPP_

#include "liteprobes.hpp"

#include <memory>

namespace liteinst {
namespace liteprobes {

class CodeJitter {
  public:
    std::unique_ptr<Springboard> emitSpringboard(const CoalescedProbes& cp, 
        utils::Address target, const InstrumentationProvider& provider);
    int64_t getSpringboardSize(const CoalescedProbes& cp, 
        const InstrumentationProvider& provider);

};

} // End liteprobes
} // End liteinst

#endif /* _CODE_JITTER_HPP_ */
