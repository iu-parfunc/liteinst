
#ifndef _CONTROL_FLOW_ROUTER_HPP_
#define _CONTROL_FLOW_ROUTER_HPP_

#include <list>
#include <map>
#include <memory>
#include <signal.h>
#include "liteprobes.hpp"
#include "concurrency.hpp"

extern "C" void liteprobes_sigill_handler(int signum, siginfo_t* siginfo,
    void* context);

namespace liteinst {
namespace liteprobes {

class ControlFlowRouter {
  public:
    static void addSpringboard(std::unique_ptr<Springboard> sb);
    static void removeSpringboard(Springboard* sb);
    static Springboard* getSpringboard(utils::Address address);
    static Springboard* getContainingSpringboard(utils::Address address);
    static std::list<Springboard*> getOverlappingSpringboards(
        utils::range::Range r);
    static utils::Address getRerouteAddress(utils::Address addr); 

  private:
    static utils::concurrency::ReadWriteLock lock;
    static std::map<utils::Address, std::unique_ptr<Springboard>> route_map;
};

} // End liteprobes
} // End liteinst

#endif /* _CONTROL_FLOW_ROUTER_HPP_ */
