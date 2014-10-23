
using namespace std;

#include "finstrumentor.hpp"
#include "dynamicarray.h"

Instrumentor* INSTRUMENTOR_INSTANCE;
InstrumentationFunc prologFunc;
InstrumentationFunc epilogFunc;
FinsProbeInfo* probeInfo;

Finstrumentor::Finstrumentor(InstrumentationFunc prolog, InstrumentationFunc epilog) {
  this->prologFunc = prolog; 
  this->epilogFunc = epilog; 
  prologFunc = prolog;
  epilogFunc = epilog;
}

void Finstrumentor::initialize() {
  // Here we are leaking object data to global variables so that we don't incur
  // overhead of object access to get to these data. This should be fine since 
  // Finstrumentor is really a singleton object.
  INSTRUMENTOR_INSTANCE = this;
  probeInfo = this->probeInfo;

  //setting up global data structures
  // this->probeInfo = new DynamicArray<FinsProbeInfo>(DEFAULT_PROBE_COUNT); 
  this->probeInfo = new FinsProbeInfo[DEFAULT_PROBE_COUNT]; // C++ value intialization. Behaves like calloc  
}

int Finstrumentor::activateProbe(string name, void* funcPtr) {
  InstrumentationFunc func = (InstrumentationFunc)funcPtr;

  return 0;

}

int Finstrumentor::deactivateProbe(string name) {

  return 0;

}

Finstrumentor::~Finstrumentor() {

}
