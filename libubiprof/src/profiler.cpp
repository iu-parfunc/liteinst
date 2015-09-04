
#include <string>
#include "ubiprof.hpp"
#include "backoff_profiler.hpp"
#include "sampling_profiler.hpp"

using namespace std;

Profiler* PROFILER;

Profiler* initializeGlobalProfiler(ProfilerType type) {

  // If already initialized throw an error
  if (PROFILER) {
    throw -1;
  }

  if (type == ProfilerType::BACKOFF) {
    PROFILER = new BackoffProfiler();
    PROFILER->initialize();
  } else if (type == ProfilerType::SAMPLING) {
    PROFILER = new SamplingProfiler();
    PROFILER->initialize();
  } else if (type == ProfilerType::MINIMAL_BACKOFF) {
    PROFILER = new MinimalBackoffProfiler();
    PROFILER->initialize();
  } else if (type == ProfilerType::MINIMAL_SAMPLING) {
    PROFILER = new MinimalSamplingProfiler();
    PROFILER->initialize();
  } 
  
  /*
  else if (type == EMPTY) {
    PROFILER = new EmptyProfiler();
    PROFILER->initialize();
  } else if (type == ADAPTIVE) {
    PROFILER = new AdaptiveProfiler();
    PROFILER->initialize();
  } else if (type == MINIMAL_ADAPTIVE) {
    PROFILER = new MinimalAdaptiveProfiler();
    PROFILER->initialize();

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

    profiler_ = this;
}

void Profiler::callback(const ProbeMetaData* pmd) {

  // Profiler hasn't yet been initialized properly
  if (!profiler_ || !PROBE_PROVIDER) {
    // return;
    throw -1; // Make the failure signalable without terminating
  }

  // Get the exclusive access to meta data structures before mutating.
  // Future improvement should include using concurrent data structures
  profiler_->func_lock_.lock(); 

  FuncId func_id;
  try {
    func_id = profiler_->getFunctionId(pmd->func_name); 
    profiler_->addProbeMetaDataEntry(func_id, pmd);
  } catch (int e) {
    // Function is not yet registed with Ubiprof. Do that now.
    FuncMetaData* fmd = new FuncMetaData;
    fmd->func_name = pmd->func_name; 
    fmd->probe_meta_data = new vector<const ProbeMetaData*>();
    fmd->probe_meta_data->push_back(pmd);

    profiler_->addFunctionMetaDataEntry(fmd);
    func_id = fmd->func_id;
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

FuncId Profiler::getFunctionId(string name) {
  auto it = meta_data_by_name_.find(name);
  if (it != meta_data_by_name_.end()) {
    return it->second->func_id;
  } else {
    throw -1;
  }
}

string Profiler::getFunctionName(FuncId id) {
  auto it = meta_data_by_id_.find(id);
  if (it != meta_data_by_id_.end()) {
    return it->second->func_name;
  } else {
    throw -1;
  }
}

void Profiler::addFunctionMetaDataEntry(FuncMetaData* fmd) {
  auto it = meta_data_by_id_.find(fmd->func_id);
  if (it != meta_data_by_id_.end()) {
    throw -1;
  } else {
    if (fmd->func_name.empty()) {
      throw -1;
    }

    fmd->func_id = func_id_counter_.fetch_add(1);
    meta_data_by_id_.insert(make_pair(fmd->func_id, fmd));
    meta_data_by_name_.insert(make_pair(fmd->func_name, fmd));
  }
}

void Profiler::addProbeMetaDataEntry(FuncId id, const ProbeMetaData* pmd) {
  auto it = meta_data_by_id_.find(id);
  if (it != meta_data_by_id_.end()) {
    FuncMetaData* fmd = it->second;
    vector<const ProbeMetaData*>* plist = fmd->probe_meta_data;
    plist->push_back(pmd);
  } else {
    throw -1;
  }
}

bool Profiler::profileFunction(string func_name) {
  bool result = true;
  auto it = meta_data_by_name_.find(func_name);
  if (it != meta_data_by_name_.end()) {
    FuncMetaData* fmd = it->second;
    vector<const ProbeMetaData*>* plist = fmd->probe_meta_data;
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
  auto it = meta_data_by_id_.find(func_id);
  if (it != meta_data_by_id_.end()) {
    FuncMetaData* fmd = it->second;
    vector<const ProbeMetaData*>* plist = fmd->probe_meta_data;
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
  auto it = meta_data_by_name_.find(func_name);
  if (it != meta_data_by_name_.end()) {
    FuncMetaData* fmd = it->second;
    vector<const ProbeMetaData*>* plist = fmd->probe_meta_data;
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
  auto it = meta_data_by_id_.find(func_id);
  if (it != meta_data_by_id_.end()) {
    FuncMetaData* fmd = it->second;
    vector<const ProbeMetaData*>* plist = fmd->probe_meta_data;
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

Profiler::~Profiler() {
  delete PROBE_PROVIDER;
}
