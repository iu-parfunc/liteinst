
#include <string>
#include "ubiprof.hpp"

using namespace std;

Profiler* initializeGlobalProfiler(ProfilerType type) {

  // If already initialized throw an error
  if (PROFILER) {
    throw -1;
  }

  /*
  if (type == BACKOFF) {
    PROFILER = new BackoffProfiler();
    PROFILER->initialize();
  } else if (type == SAMPLING) {
    PROFILER = new SamplingProfiler();
    PROFILER->initialize();
  } else if (type == EMPTY) {
    PROFILER = new EmptyProfiler();
    PROFILER->initialize();
  } else if (type == ADAPTIVE) {
    PROFILER = new AdaptiveProfiler();
    PROFILER->initialize();
  } else if (type == MINIMAL_ADAPTIVE) {
    PROFILER = new MinimalAdaptiveProfiler();
    PROFILER->initialize();
  } else if (type == MINIMAL_BACKOFF) {
    PROFILER = new MinimalBackoffProfiler();
    PROFILER->initialize();
  } else if (type == MINIMAL_SAMPLING) {
    PROFILER = new MinimalSamplingProfiler();
    PROFILER->initialize();
  }
  */

  return PROFILER;
}

Profiler::Profiler(InstrumentationFunc prolog, InstrumentationFunc epilog) : 
  prolog_(prolog), epilog_(epilog) {

    ProbeProvider* p;
    try {
      p = initializeGlobalProbeProvider(ProviderType::FINSTRUMENT, callback);
    } catch (int e) {
      fprintf(stderr, "ProbeProvider already initialized. Getting the existing one..\n");
    }

    if (p == NULL) {
      fprintf(stderr, "Unable to initialize probe provider..\n");
      exit(EXIT_FAILURE);
    }

}

void Profiler::callback(const ProbeMetaData* pmd) {

  // Profiler hasn't yet been initialized properly
  if (!profiler_ || !PROBE_PROVIDER) {
    throw -1; // Make the failure signalable without terminating
  }

  // Get the exclusive access to meta data structures before mutating.
  // Future improvement should include using concurrent data structures
  profiler_->func_lock_.lock(); 

  FuncId func_id = profiler_->getFunctionId(pmd->func_name); 

  // Populate meta data
  auto it1 = profiler_->meta_data_by_id_.find(func_id);
  if (it1 != profiler_->meta_data_by_id_.end()) {
    vector<const ProbeMetaData*>* plist = it1->second;
    plist->push_back(pmd);
  } else {
    vector<const ProbeMetaData*>* plist = new vector<const ProbeMetaData*>();
    plist->push_back(pmd);
    profiler_->meta_data_by_id_.insert(make_pair(func_id, plist));
  }

  auto it2 = profiler_->meta_data_by_name_.find(pmd->func_name);
  if (it2 != profiler_->meta_data_by_name_.end()) {
    vector<const ProbeMetaData*>* plist = it2->second;
    plist->push_back(pmd);
  } else {
    vector<const ProbeMetaData*>* plist = new vector<const ProbeMetaData*>();
    plist->push_back(pmd);
    profiler_->meta_data_by_name_.insert(make_pair(pmd->func_name, plist));
  }

  profiler_->func_lock_.unlock(); 

  // Mandatory probe initialization call
  PROBE_PROVIDER->initialize(pmd->probe_id, func_id);
  try {
    if (pmd->probe_context == ProbeContext:: ENTRY) {
      PROBE_PROVIDER->activate(pmd->probe_id, profiler_->prolog_);
    } else {
      PROBE_PROVIDER->activate(pmd->probe_id, profiler_->epilog_);
    }
  } catch (int e) {
    fprintf(stderr, "Error while activating probe of function : %s.\n",
        pmd->func_name.c_str());
    throw -1; // Make the faliure signalable without terminating
  }

}

bool Profiler::profileFunction(string func_name) {
  bool result = true;
  auto pair = meta_data_by_name_.find(func_name);
  if (pair != meta_data_by_name_.end()) {
    vector<const ProbeMetaData*>* plist = pair->second;
    // Returns false if not all probe activations failed
    // It would be really nice to have some form of transactionality in this
    // if some of the activations fail so that probes are not left in an
    // inconsitent state within the function.
    for (auto it = plist->begin(); it != plist->end(); ++it) {
      const ProbeMetaData* pmd = *it;
      if (pmd->probe_context == ProbeContext::ENTRY) {
        result = result && PROBE_PROVIDER->activate(pmd->probe_id, prolog_);
      } else {
        result = result && PROBE_PROVIDER->activate(pmd->probe_id, epilog_);
      }
    }
  }

  return result;
}

bool Profiler::profileFunction(FuncId func_id) {
  bool result = false;
  auto pair = meta_data_by_id_.find(func_id);
  if (pair != meta_data_by_id_.end()) {
    vector<const ProbeMetaData*>* plist = pair->second;
    // Returns false if not all probe activations failed
    // It would be really nice to have some form of transactionality in this
    // if some of the activations fail so that probes are not left in an
    // inconsitent state within the function.
    for (auto it = plist->begin(); it != plist->end(); ++it) {
      const ProbeMetaData* pmd = *it;
      if (pmd->probe_context == ProbeContext::ENTRY) {
        result = result && PROBE_PROVIDER->activate(pmd->probe_id, prolog_);
      } else {
        result = result && PROBE_PROVIDER->activate(pmd->probe_id, epilog_);
      }
    }
  }

  return result;
}

bool Profiler::unprofileFunction(string func_name) {
  bool result = true;
  auto pair = meta_data_by_name_.find(func_name);
  if (pair != meta_data_by_name_.end()) {
    vector<const ProbeMetaData*>* plist = pair->second;
    // Returns false if not all probe activations failed
    // It would be really nice to have some form of transactionality in this
    // if some of the activations fail so that probes are not left in an
    // inconsitent state within the function.
    for (auto it = plist->begin(); it != plist->end(); ++it) {
      const ProbeMetaData* pmd = *it;
      result = result && PROBE_PROVIDER->deactivate(pmd->probe_id);
    }
  }

  return result;
}

bool Profiler::unprofileFunction(FuncId func_id) {
  bool result = false;
  auto pair = meta_data_by_id_.find(func_id);
  if (pair != meta_data_by_id_.end()) {
    vector<const ProbeMetaData*>* plist = pair->second;
    // Returns false if not all probe activations failed
    // It would be really nice to have some form of transactionality in this
    // if some of the activations fail so that probes are not left in an
    // inconsitent state within the function.
    for (auto it = plist->begin(); it != plist->end(); ++it) {
      const ProbeMetaData* pmd = *it;
      result = result && PROBE_PROVIDER->deactivate(pmd->probe_id);
    }
  }

  return result;
}
