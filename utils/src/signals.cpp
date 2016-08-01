
#include "signals.hpp"
#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <cstring>

using namespace utils::signals;

using std::vector;
using std::sort;
using std::memory_order_acquire;
using std::ostringstream;
using std::memory_order_release;
using std::range_error;
using std::logic_error;
using std::invalid_argument;

bool SignalHandlerRegistry::priority_enabled = false;
SignalHandlerRegistry::SigEntry SignalHandlerRegistry::sig_entries[];

void SignalHandlerRegistry::enableHandlerPriority() {
  priority_enabled = true;
}

void SignalHandlerRegistry::disableHandlerPriority() {
  priority_enabled = false;
}

void SignalHandlerRegistry::registerSignalHandler(HandlerRegistration& reg,
    bool sync, int priority) {
  SigEntry* entry = &sig_entries[reg.signum];

  if (!entry->initialized) {

    // Acquire lock
    while (entry->init_lock.test_and_set(memory_order_acquire)) {
      __asm__("pause"); // Gentle spinning.
    }

    if (!entry->initialized) {
      entry->signum = reg.signum;
      entry->n_sync_handlers = 0;
      entry->n_async_handlers = 0;

      entry->sync_handlers.reserve(MAX_HANDLERS);
      entry->async_handlers.reserve(MAX_HANDLERS);

      for (int i=0; i < MAX_HANDLERS; i++) {
        entry->sync_handlers.push_back(SigHandlerEntry());
        entry->async_handlers.push_back(SigHandlerEntry());
        entry->sync_handlers[i].used = false;
        entry->async_handlers[i].used = false;
      }

      entry->initialized = true;
    } 

    entry->init_lock.clear(memory_order_release);
  }

  // Acquire lock
  while (entry->reg_lock.test_and_set(memory_order_acquire)) {
      __asm__("pause"); // Gentle spinning.
  }

  int index = -1;
  if (sync) {
    if (entry->n_sync_handlers < MAX_HANDLERS) {
      bool registered = false;
      for (int i=0; i < MAX_HANDLERS; i++) {
        if (!entry->sync_handlers[i].used) {
          entry->sync_handlers[i].used = true;
          entry->sync_handlers[i].priority = priority;
          entry->sync_handlers[i].handler = reg.act.sa_sigaction;
        }

        entry->n_sync_handlers++;
        registered = true;
        index = i;
        break;
      }

      if (!registered) {
        entry->reg_lock.clear(memory_order_release);
        ostringstream out;  
        out << "Maximum number of asynchrounous handlers exceeded"
          " for signal" << reg.signum << "\n";
        throw range_error(out.str());
      }
    } else {
      entry->reg_lock.clear(memory_order_release);
      ostringstream out;  
      out << "Maximum number of asynchrounous handlers exceeded"
        " for signal" << reg.signum << "\n";
      throw range_error(out.str());
    }    
  } else {
    if (entry->n_async_handlers < MAX_HANDLERS) {
      bool registered = false;
      for (int i=0; i < MAX_HANDLERS; i++) {
        if (!entry->async_handlers[i].used) {
          entry->async_handlers[i].used = true;
          entry->async_handlers[i].priority = priority;
          entry->async_handlers[i].handler = reg.act.sa_sigaction;
        }

        entry->n_async_handlers++;
        registered = true;
        index = i;
        break;
      }

      if (!registered) {
        entry->reg_lock.clear(memory_order_release);
        ostringstream out;  
        out << "Maximum number of asynchrounous handlers exceeded"
          " for signal" << reg.signum << "\n";
        throw range_error(out.str());
      }
    } else {
      entry->reg_lock.clear(memory_order_release);
      ostringstream out;  
      out << "Maximum number of asynchrounous handlers exceeded"
        " for signal" << reg.signum << "\n";
      throw range_error(out.str());
    }    
  }

  // Do the actual signal registration
  struct sigaction act;
  memset( &act, 0, sizeof act);
  act.sa_sigaction = &handler_dispatcher;
  act.sa_mask = reg.act.sa_mask;
  act.sa_flags = reg.act.sa_flags|SA_SIGINFO; // Check if reg.act.sa_flags are valid first

  int ret = sigaction(reg.signum, &act, NULL);

  printf("REGISTERED handler for %d\n", reg.signum);
  printf("NUMBER OF REGISTERED HANDLERS : %d\n",
      sig_entries[reg.signum].sync_handlers.size());

  // Release lock
  entry->reg_lock.clear(memory_order_release);

  if (!ret) {
    reg.reg_id = index;
    reg.sync = sync;
  } else {
    ostringstream out;  
    out << "Failed registering signal handler with error : " 
      << strerror(errno) << "\n";
    throw logic_error(out.str());
  }
}

void SignalHandlerRegistry::unregisterSignalHandler(HandlerRegistration& reg) {
  SigEntry* entry = &sig_entries[reg.signum];

  if (!entry->initialized) {
    ostringstream out;  
    out << "No handler registered for signal : " << reg.signum << "\n"; 
    throw invalid_argument(out.str());
  }

  // Acquire lock
  while (entry->reg_lock.test_and_set(memory_order_acquire)) {
      __asm__("pause"); // Gentle spinning.
  }

  if (reg.sync) {
    entry->sync_handlers[reg.reg_id].used = false;
    entry->n_sync_handlers--;
  } else {
    entry->async_handlers[reg.reg_id].used = false;
    entry->n_async_handlers--;
  }

  if (entry->n_sync_handlers == 0 
      && entry->n_async_handlers == 0) {
    // Do the actual signal unregistration
    struct sigaction act;
    memset( &act, 0, sizeof act);
    act.sa_handler = SIG_DFL; 

    sigaction(reg.signum, &act, NULL);
  } 

  // Release lock
  entry->reg_lock.clear(memory_order_release);
}

/********** Private helper functions **********/
vector<SignalHandlerRegistry::SigHandlerEntry> 
  SignalHandlerRegistry::handlerSort(int signum, bool sync) {

  SigEntry* entry = &sig_entries[signum];
  vector<SignalHandlerRegistry::SigHandlerEntry> handlers;
  if (sync) {
    handlers = entry->sync_handlers;
  } else {
    handlers = entry->async_handlers;
  } 

  sort(handlers.begin(), handlers.end(),
      [] (SigHandlerEntry const& a, SigHandlerEntry const& b) -> bool {
      return a.priority < b.priority;
      }); 

  return handlers;
}

void SignalHandlerRegistry::handler_dispatcher(int signum, siginfo_t* siginfo,
    void* context) {
  SigEntry* entry = &sig_entries[signum]; 

  if (!entry->initialized) {
    return;
  }

  if (!priority_enabled) {
    for (int i=0; i < MAX_HANDLERS; i++) {
      if (entry->sync_handlers[i].used) {
        entry->sync_handlers[i].handler(signum, siginfo, context);
      }
    }
  } else {
    vector<SigHandlerEntry> handlers = handlerSort(signum, true);
    for (int i=0; i < MAX_HANDLERS; i++) {
      if (handlers[i].used) {
        handlers[i].handler(signum, siginfo, context);
      }
    }
  }

  // Do some signalfd stuff to invoke async handlers
}
