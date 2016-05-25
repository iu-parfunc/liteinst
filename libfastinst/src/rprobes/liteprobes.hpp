
typedef uint64_t ProbeContextId;

enum class ProbeContextType {
  FUNCTION,
  LOOP,
  BASIC_BLOCK,
  ADDRESS,
  LINE_NUM
};

enum class ProbePlacement {
  ENTRY,
  EXIT,
  CALL
};

class ProbeContextConfiguration {
  std::vector<InstrumentationFunc> instrumentations;
  std::vector<ProbePlacement> placements;
}

class Probe {
  public:
    ProbeId id;
    ProbeLocation placement;
    Address address;
    Address target;
};

class ProbeContext {
  public:
    ProbeContextId id;
    std::string name;
    ContextType type;
    ProbeContextConfiguration config;
    std::vector<Probe> probes;
};
