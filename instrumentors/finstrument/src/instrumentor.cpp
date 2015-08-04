
#include "../../../api/probe_API.hpp"
#include "finstrumentor.hpp"

// Note: This is not thread safe. But should be probably fine for now
// since we are only invoking this once at application startup
Instrumentor* Instrumentor::newInstance(int type, InstrumentationFunc prolog, InstrumentationFunc epilog) {
  
  // Delete existing instance possibly with all the accumilated data up to now.
  if (INSTRUMENTOR_INSTANCE) {
    fprintf(stderr, "WARNING: Already initialized. Returning the existing instance..\n");
    return INSTRUMENTOR_INSTANCE;
  }

  switch(type) {
    case FINSTRUMENT:
      INSTRUMENTOR_INSTANCE = new Finstrumentor(prolog, epilog);
      ((Finstrumentor*)INSTRUMENTOR_INSTANCE)->initialize();
      break;
    default:
      INSTRUMENTOR_INSTANCE = new Finstrumentor(prolog, epilog);
      ((Finstrumentor*)INSTRUMENTOR_INSTANCE)->initialize();
  }

  return INSTRUMENTOR_INSTANCE;
}

Instrumentor* Instrumentor::getInstance() {

  return INSTRUMENTOR_INSTANCE;

}

