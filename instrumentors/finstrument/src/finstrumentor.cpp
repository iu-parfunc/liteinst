
using namespace std;

#include "finstrumentor.hpp"

Finstrumentor::Finstrumentor(instrumentation_func prolog_function, instrumentation_func epilog_function) {
  this->prolog_func = prolog_function; 
  this->epilog_func = epilog_function; 
}

void Finstrumentor::initialize(Instrumentor* inst) {
  INSTRUMENTOR_INSTANCE = inst;
}

int Finstrumentor::activate_probe(string name, void* func_ptr) {
  instrumentation_func func = (instrumentation_func)func_ptr;


  return 0;

}

int Finstrumentor::deactivate_probe(string name) {

  return 0;

}
