
#include "calibrate.hpp"

#include <string>

using namespace std;

namespace calibrate { 

  void emptyInstrumentation(ProbeArg func_id) {

  }

  void calibrationCallback(const ProbeMetaData* pmd) {
    // If this callback related to the calibration function probes we activate 
    // them
    std::string func_name = "calibrationFunction"; 
    if (func_name.compare(pmd->func_name) == 0) {
      int CALIBRATION_FUNC_ID = 0;

      PROBE_PROVIDER->initialize(pmd->probe_id, CALIBRATION_FUNC_ID);
      try {
        PROBE_PROVIDER->activate(pmd->probe_id, calibrate::emptyInstrumentation);
      } catch (int e) {
        fprintf(stderr, "Error while activating probe of function : %s.\n",
            pmd->func_name.c_str());
      }
    }
  }

  const int NUM_INVOCATIONS = 100000;

  uint64_t getInstrumentationOverheadPerProbe() {
  
    uint64_t total_overhead = 0;
    for (int i = 0; i < NUM_INVOCATIONS; i++) {
      total_overhead += calibrationFunction();
    } 

    // We round since decimal cycles doesn't make much sense
    return  total_overhead /(2 * NUM_INVOCATIONS);  
  }

}
