
#ifndef _LITEINST_HPP_
#define _LITEINST_HPP_

#include <cstdint>
#include <vector>
#include <atomic>
#include <string>
#include <list>
#include <memory>
#include <cstdio>
#include <unordered_map>

#include "concurrency.hpp"
#include "defs.hpp"

// TODO: Make this work for x86
#define LITEINST_SET_PROBE_INFO(pi) \
  do { \
    __asm volatile ("movq %%rdi, %0\n" \
         "movq %%rsi, %1\n" \
         "movw %%dx, %2\n" \
         "movb %%r10b, %3\n" \
         "movq %%r8, %4\n" \
         "movq %%r9, %5\n" \
       : "=m"(pi.ctx.pg_id), "=m"(pi.ctx.p_id), "=m"(pi.ctx.i_id), "=m"(pi.ctx.placement), \
         "=m"(pi.ctx.u_regs), "=m"(pi.address)); \
  } while (0)

namespace liteinst {

/// This is used to specify which provider needs to be selected
/// at initialization time.
enum class ProviderType { ZCA, FINSTRUMENT, DTRACE, DYNINST, LITEPROBES };

/// In the future we will aim to support an open universe of probe
/// providers.  In the short term, we explicitly enumerate the probe
/// providers.  Any use of this field violates the abstraction of the
/// ProbeProvider by depending on implementation details.
enum class ProbeType { ZCA, FINSTRUMENT, DTRACE, DYNINST, RPROBES }; 

/// Opaque identifier for a ProbeGroup
typedef uint64_t ProbeGroupId;

/// Opaque identifier for a Probe
typedef uint64_t ProbeId;

/// Opaque identifier for an Instrumentor instance
typedef uint16_t InstrumentationId;

/// Opaque identifier for a probe registration
typedef uint64_t RegistrationId;

/// The probe groupings available. 
enum class ProbeGroupType : uint8_t { 
  FUNCTION,
  LOOP,
  BASIC_BLOCK,
  OFFSET,
  LINE_NUM, 
  INS_TYPE
};

/// The placement of probes within a given probe grouping.
enum class ProbePlacement : uint8_t { 
  ENTRY,
  EXIT,
  NONE,
  BOUNDARY /* ENTRY + EXIT */
};

struct ProbeContext {
  ProbeId p_id;
  ProbeGroupId pg_id;
  InstrumentationId i_id;
  ProbePlacement placement;
  utils::Address u_regs;
};

struct ProbeInfo {
  utils::Address address;
  ProbeContext ctx;
};

class ProbeAxis {
  public:
    ProbeAxis(std::string spec, ProbePlacement placement) : spec(spec),
      placement(placement) {

    }

    virtual void setSpec(std::string ss) {
      spec = ss;
    }

    virtual void setPlacement(ProbePlacement p) {
      placement = p;
    }

    virtual std::string getSpec() {
      return spec;
    }

    ProbePlacement getPlacement() {
      return placement;
    }

  protected:
    std::string spec;
    ProbePlacement placement;
};

class Module : public ProbeAxis {
  public:
    Module(std::string spec = "", ProbePlacement p = ProbePlacement::NONE) : 
      ProbeAxis(spec, p) {
    }
};

class Function : public ProbeAxis {
  public:
    Function(std::string spec = "", ProbePlacement p = ProbePlacement::NONE) : 
      ProbeAxis(spec, p) {
    }
};

class Loop : public ProbeAxis {
  public:
    Loop(std::string spec = "", ProbePlacement p = ProbePlacement::NONE) : 
      ProbeAxis(spec, p) {
    }
};

class BasicBlock : public ProbeAxis {
  public:
    BasicBlock(std::string spec = "", ProbePlacement p = ProbePlacement::NONE) :
      ProbeAxis(spec, p) {
    }
};

class Offset : public ProbeAxis {
  public:
    Offset(std::string spec = "", ProbePlacement p = ProbePlacement::NONE) :
      ProbeAxis(spec, p) {
    }
};

class InstructionType : public ProbeAxis {
  public:
    InstructionType(std::string spec = "", ProbePlacement p = ProbePlacement::NONE) :
      ProbeAxis(spec, p) {
    } 
};

/*
class ProbeGroup {
  public:
    ProbeGroupId id;
    std::string name;
    Module module;
    Function function;
    Loop loop;
    BasicBlock basic_block;
    Offset offset;
    ProbePlacement placement;
};
*/

/// Opaque identifier for uniquely identifying a probe group
typedef uint64_t ProbeGroupId;

/// The type of the instrumentation function.
typedef void (*InstrumentationFunction) ();


class InstrumentationProvider {
  public:
    InstrumentationId id;

    InstrumentationProvider(std::string name, InstrumentationFunction entry, 
        InstrumentationFunction exit) : name(name), entry(entry), exit(exit) {
    }

    InstrumentationFunction getEntryInstrumentation() const {
      return entry;
    }

    InstrumentationFunction getExitInstrumentation() const {
      return exit;
    }

    std::string getName() {
      return name;
    }

  private:
    std::string name;
    InstrumentationFunction entry = NULL;
    InstrumentationFunction exit = NULL;
};

class Coordinates {
  public:

    // Setters
    Coordinates& setModule(Module m) {
      module = m;
      return *this; 
    }

    Coordinates& setFunction(Function f) {
      function = f;
      return *this; 
    }

    Coordinates& setLoop(Loop l) {
      loop = l;
      return *this; 
    }

    Coordinates& setBasicBlock(BasicBlock bb) {
      basic_block = bb;
      return *this; 
    }

    Coordinates& setOffset(Offset o) {
      offset = o;
      return *this; 
    }

    Coordinates& setInstructionType(InstructionType i) {
      ins_type = i;
      return *this;
    }

    Coordinates& setProbePlacement(ProbePlacement placement) {
      if (ins_type.getSpec().compare("")) {
        ins_type.setPlacement(placement);
      } else if (offset.getSpec().compare("")) {
        offset.setPlacement(placement);
      } else if (basic_block.getSpec().compare("")) {
        basic_block.setPlacement(placement);
      } else if (loop.getSpec().compare("")) {
        loop.setPlacement(placement);
      } else if (function.getSpec().compare("")) {
        function.setPlacement(placement);
      } else if (module.getSpec().compare("")) {
        module.setPlacement(placement);
      } else {
        throw std::invalid_argument("At least one probe coordinate must be" 
            " specified");
      }
    }

    // Getters
    Module getModule() {
      return module;
    }

    Function getFunction() {
      return function;
    }

    Loop getLoop() {
      return loop;
    }

    BasicBlock getBasicBlock() {
      return basic_block;
    }

    Offset getOffset() {
      return offset;
    }

    InstructionType getInstructionType() {
      return ins_type;
    }

  private:
    Module module;
    Function function;
    Loop loop; 
    BasicBlock basic_block;
    Offset offset;
    InstructionType ins_type;
};

class ProbeGroupInfo {
  public:
    ProbeGroupId id;
    std::string name;
    utils::Address start;

    ProbeGroupInfo(ProbeGroupId id) : id(id) {

    }

    ProbeGroupInfo(ProbeGroupId id, std::string name, utils::Address start) 
      : id(id), name(name), start(start) {
    }

    bool operator == (const ProbeGroupInfo& pgi) {
      return (id == pgi.id);
    }
};

class ProbeRegistration {
  public:
    RegistrationId reg_id;
    std::map<std::string, std::vector<ProbeGroupInfo>> pg_by_function;
    std::list<ProbeGroupInfo> conflicts;

    std::vector<ProbeGroupInfo> getProbeGroupsForFunction(std::string name) {
      auto it = pg_by_function.find(name);
      if (it != pg_by_function.end()) {
        return it->second;
      }

      return std::vector<ProbeGroupInfo>();
    }

    std::vector<std::string> getProbedFunctions() {
      std::vector<std::string> fns;
      for (auto it = pg_by_function.begin(); it != pg_by_function.end(); it++) {
        fns.push_back(it->first);
      }
      return fns;
    }
};

/// The signature for a callback that registers a newly discovered
/// probe.  The ProbeProvider owns the ProbeMetadata record, so the
/// callback may read it, but should not free it.
///
/// This probe-discovery callback has two main obligations:
///
///  (1) Call initialize() to set up the constant argument to future
///      probe invocations.
///
///  (2) Call either activate() or deactivate() any number of times,
///      to leave the probe in a valid state by callback completion.
///
/// These methods can be called by accessing the owning ProbeProvider
/// through the ProbeMetadata pointer itself.
typedef void (*Callback) (const ProbeInfo* pi);

/// Initialization call back function which willl be called when probe provider
/// gets initialized
typedef void (*InitCallback) (void);

/// probeId -> probeGroupId
///   

/// [Module]:Function:[Loop]:[Basic Block]\([Offset]|[Ins])@Entry
/// Module:Function:Loop:Basic Block:Offset:Ins:*
/// [Module]:Function:[Loop]:[Basic Block]:[Offset]:*
/// /* $exit != offset1&offset2 & $granularity == LOOP*/
class ProbeProvider {
  public:
    ProbeProvider(Callback cb, InitCallback init) : 
      callback(cb), init_callback(init) {
      // probe_meta_data = new ProbeVec;
    }

    InstrumentationId registerInstrumentationProvider(
        InstrumentationProvider instrumentation) {

      ProviderEntry pe(instrumentation.getName());
      auto it = i_providers.find(pe);
      if (it != i_providers.end()) {
        printf("Error adding provider..\n");
        throw std::invalid_argument("Provider with the same name already " 
            "exists");
      } else {
        lock.lock();
        pe.provider_id = i_provider_counter++;
        instrumentation.id = pe.provider_id;
        i_providers.insert(
            std::pair<ProviderEntry, InstrumentationProvider>(pe, 
              instrumentation));
        lock.unlock();
        return pe.provider_id;
      }
    }

    const InstrumentationProvider& getInstrumentationProvider(
        std::string name) {
      ProviderEntry pe(name);
      auto it = i_providers.find(pe);
      if (it != i_providers.end()) {
        return it->second;
      } else {
        throw std::invalid_argument("Provider with the given name does not" 
            "exist");
      }
    }

    virtual ProbeRegistration registerProbes(Coordinates coords, 
        std::string instrumentation_provider) = 0;

    // Per probe operations
    virtual bool activate(ProbeInfo probe) = 0;
    virtual bool deactivate(ProbeInfo probe) = 0;

    // Per probe group operations
    virtual bool activate(ProbeGroupInfo pg) = 0;
    virtual bool deactivate(ProbeGroupInfo pg) = 0;

    // Bulk operations
    virtual bool activate(ProbeRegistration registration) = 0;
    virtual bool deactivate(ProbeRegistration registraiton) = 0;

    static ProbeProvider* getGlobalProbeProvider();

    static ProbeProvider* initializeGlobalProbeProvider(ProviderType type, 
      Callback callback, InitCallback init);

    InitCallback init_callback;

  protected:
    Callback callback;

  private:
    static std::unique_ptr<ProbeProvider> p;
    static utils::concurrency::SpinLock lock;

    class ProviderEntry {
      public:
        int provider_id;
        std::string provider_name;

        ProviderEntry(std::string name) : provider_name(name) {

        }

        bool operator==(const ProviderEntry& other) const { 
          return (provider_name == other.provider_name);
        }
    };

    class ProviderEntryHasher {
      public:
        std::size_t operator()(const ProviderEntry& k) const {
          return (std::hash<std::string>()(k.provider_name));
        }
    };

    std::unordered_map<ProviderEntry, InstrumentationProvider, 
      ProviderEntryHasher> i_providers;
    int32_t i_provider_counter;
};

} /* End liteinst */

#endif /* _LITEINST_HPP_ */
