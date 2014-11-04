
#include "../../../api/probe_API.hpp"
#include "finstrumentor.hpp"

// Note: This is not thread safe. But should be probably fine for now
// since we are only invoking this once at application startup
Instrumentor* Instrumentor::getInstance(int type, InstrumentationFunc prolog, InstrumentationFunc epilog) {
  if(!INSTRUMENTOR_INSTANCE) {
    INSTRUMENTOR_INSTANCE = new Finstrumentor(prolog, epilog);
    ((Finstrumentor*)INSTRUMENTOR_INSTANCE)->initialize();
  }

  return INSTRUMENTOR_INSTANCE;
}
