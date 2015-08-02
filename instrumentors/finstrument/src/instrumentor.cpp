
#include "../../../api/probe_API.hpp"
#include "finstrumentor.hpp"

// Note: This is not thread safe. But should be probably fine for now
// since we are only invoking this once at application startup
Instrumentor* Instrumentor::getInstance(int type, InstrumentationFunc prolog, InstrumentationFunc epilog) {
  
  // Delete existing instance possibly with all the accumilated data up to now.
  if (!INSTRUMENTOR_INSTANCE) {
    fprintf(stderr, "WARNING : Deleting existing instrumentor instance. Data may be lost..\n");
    delete INSTRUMENTOR_INSTANCE;
  }

  switch(type) {
    case FINSTRUMENT:
      INSTRUMENTOR_INSTANCE = new Finstrumentor(prolog, epilog);
      ((Finstrumentor*)INSTRUMENTOR_INSTANCE)->initialize();
    default:
      INSTRUMENTOR_INSTANCE = new Finstrumentor(prolog, epilog);
      ((Finstrumentor*)INSTRUMENTOR_INSTANCE)->initialize();
  }

  return INSTRUMENTOR_INSTANCE;
}
