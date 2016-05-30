enum ProbeGroupType { 
  FUNCTION,
  LOOP,
  BASIC_BLOCK,
  ADDRESS,
  LINE_NUM
};

enum ProbePlacement { // Probe placements within a ToggleGroup
  ENTRY,
  EXIT,
  CALL
};

typedef uint64_t ProbeGroupId;
typedef void (*InstrumentationFunc) (ProbeGroupType type, ProbeGroupId group_id, ProbeId probe_id);

class ProbeGroupConfig {
  public:
    void setInstrumentation(ProbePlacement placement, InstrumentationFunc func);

  private:
    ProbeGroupType type;  // Type of the context
    ProbeGroupId group_id;  // Monotonically increasing context id for each context type
    map<ProbePlacement, InstrumentationFunc> instrumentations;//Instrumentation for each probe placement within context
};

class ProbeProvider {
  public:
    void configureProbeGroup(ProbeGroupConfig config);
    bool activate(ProbeGroupId id);
    bool deactivate(ProbeGroupId id);
};
