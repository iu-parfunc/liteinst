
#ifndef _FINSTRUMENTOR_HPP_
#define _FINSTRUMENTOR_HPP_

#include "../../../api/probe_API.hpp"
#include <cstdint> 

extern Instrumentor* INSTRUMENTOR_INSTANCE;

typedef void (*instrumentation_func)(uint16_t);

class Finstrumentor : public Instrumentor {

  protected :
    void (*prolog_func)(uint16_t);
    void (*epilog_func)(uint16_t);

  public :
    Finstrumentor(instrumentation_func prolog_func, instrumentation_func epilog_func); 
    void initialize(Instrumentor* inst);
    int activate_probe(std::string name, void* func_ptr);
    int deactivate_probe(std::string name);
    virtual ~Finstrumentor();

};

#endif /* _FINSTRUMENTOR_HPP_ */

