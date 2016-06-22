
#ifndef _LITEINST_HPP_
#define _LITEINST_HPP_

#include <cstdint>
#include <vector>
#include <atomic>
#include <string>
#include <cstdio>
#include <unordered_map>

#include "concurrency.hpp"

// TODO: Make this work for x86
#define LITEINST_SET_PROBE_CONTEXT (ctx) \
  do { \
    asm ("movq %%rdi, %0\n" \
         "movq %%rsi, %1\n" \
         "movq %%rdx, %2\n" \
         "movq %%r10, %3\n" \
         "movq %%r8, %4\n" \
         "movq %%r9, %5\n" \
       : "=m"(ctx.pg_id), "=m"(ctx.i_id), "=m"(ctx.p_id), "=m"(ctx.placement), \
         "=m"(ctx.u_regs) \
  } while (0) 

namespace liteinst {

/// This is used to specify which provider needs to be selected
/// at initialization time.
enum class ProviderType { ZCA, FINSTRUMENT, DTRACE, DYNINST, RPROBES };

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
typedef uint64_t InstrumentationId;

/// The probe groupings available. 
enum ProbeGroupType { 
  FUNCTION,
  LOOP,
  BASIC_BLOCK,
  OFFSET,
  LINE_NUM, 
  INS_TYPE
};

/// The placement of probes within a given probe grouping.
enum ProbePlacement { 
  ENTRY,
  EXIT,
  BOUNDARY /* ENTRY + EXIT */
};

struct ProbeContext {
  ProbeId p_id;
  ProbeGroupId pg_id;
  InstrumentationId i_id;
  ProbePlacement placement;
};

class ProbeAxis {
  public:
    ProbeAxis(std::string spec) : spec(spec) {

    }

    virtual void setPlacement(ProbePlacement p) {
      placement = p;
    }

    virtual std::string getSpec() {
      return spec;
    }

  protected:
    std::string spec;
    ProbePlacement placement;
};

class Module : public ProbeAxis {
  public:
    Module(std::string spec = "") : ProbeAxis(spec) {
    }
};

class Function : public ProbeAxis {
  public:
    Function(std::string spec = "") : ProbeAxis(spec) {
    }
};

class Loop : public ProbeAxis {
  public:
    Loop(std::string spec = "") : ProbeAxis(spec) {
    }
};

class BasicBlock : public ProbeAxis {
  public:
    BasicBlock(std::string spec = "") : ProbeAxis(spec) {
    }
};

class Offset : public ProbeAxis {
  public:
    Offset(std::string spec = "") : ProbeAxis(spec) {
    }
};

class InstructionType : public ProbeAxis {
  public:
    InstructionType(std::string spec = "") : ProbeAxis(spec) {
    } 
};

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

/// Opaque identifier for uniquely identifying a probe group
typedef uint64_t ProbeGroupId;

/// The type of the instrumentation function.
typedef void (*InstrumentationFunction) ();


class InstrumentationProvider {
  public:
    InstrumentationProvider(std::string name, InstrumentationFunction entry, 
        InstrumentationFunction exit) : name(name), entry(entry), exit(exit) {
    }

    InstrumentationFunction getEntryInstrumentation() {
      return entry;
    }

    InstrumentationFunction getExitInstrumentation() {
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
      if (!ins_type.getSpec().compare("")) {
        ins_type.setPlacement(placement);
      } else if (!offset.getSpec().compare("")) {
        offset.setPlacement(placement);
      } else if (!basic_block.getSpec().compare("")) {
        basic_block.setPlacement(placement);
      } else if (!loop.getSpec().compare("")) {
        loop.setPlacement(placement);
      } else if (!function.getSpec().compare("")) {
        function.setPlacement(placement);
      } else if (!module.getSpec().compare("")) {
        module.setPlacement(placement);
      } else {
        throw std::invalid_argument("At least one probe coordinate must be" 
            " specified");
      }
    }

  private:
    Module module;
    Function function;
    Loop loop; 
    BasicBlock basic_block;
    Offset offset;
    InstructionType ins_type;
};

class ProbeRegistration {

};

class ProbeMetaData {

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
typedef void (*Callback) (const ProbeMetaData* pmd);

/// probeId -> probeGroupId
///   

/// [Module]:Function:[Loop]:[Basic Block]:[Offset]:[Ins]:Entry
/// Module:Function:Loop:Basic Block:Offset:Ins:*
/// [Module]:Function:[Loop]:[Basic Block]:[Offset]:*
/// /* $exit != offset1&offset2 & $granularity == LOOP*/
class ProbeProvider {
  public:
    ProbeProvider(Callback cb) : callback(cb) {
      // probe_meta_data = new ProbeVec;
    }

    InstrumentationId registerInstrumentationProvider(
        InstrumentationProvider instrumentation) {

      ProviderEntry pe(instrumentation.getName());
      auto it = i_providers.find(pe);
      if (it != i_providers.end()) {
        throw std::invalid_argument("Provider with the same name already " 
            "exists");
      } else {
        lock.lock();
        pe.provider_id = i_provider_counter++;
        i_providers.insert(
            std::pair<ProviderEntry, InstrumentationProvider>(pe, 
              instrumentation));
        lock.unlock();
        return pe.provider_id;
      }
    }

    virtual ProbeRegistration configure(Coordinates coords, 
        std::string instrumentation_provider) = 0;

    // Per probe operations
    virtual bool activate(ProbeContext ctx) = 0;
    virtual bool deactivate(ProbeContext ctx) = 0;

    // Bulk operations
    virtual bool activate(ProbeRegistration registration) = 0;
    virtual bool deactivate(ProbeRegistration registraiton) = 0;

  protected:
    Callback callback;

  private:
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
    utils::concurrency::SpinLock lock;

};


} /* End liteinst */

#endif /* _LITEINST_HPP_ */
