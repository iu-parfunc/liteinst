#include "BPatch.h"
#include "BPatch_point.h"
#include "BPatch_function.h"
#include <stdio.h>
#include <vector>
#include "ProbeProvider.hpp"
#include <unistd.h>

#include <sys/types.h>
#include <signal.h>


void foo() {
    // sleep(1);
    printf("[mutator] I am foo()\n");
    return;
}

void bar() {
    printf("[mutator] I am bar()\n");
    return;
}

void registerProbeCallback (const ProbeMetaData* pmd) {
  printf("     Register probe %p\n", pmd);
}


/// This captures the concept of a probe provider where the
/// instrumentation itself must happen in a separate (forked) process.
/// Thus all calls to change probe state must incur inter-process
/// communication.
///
/// This particular implementation uses named-pipes to communicate
/// between instrumentor and instrumentee threads.
///
/// This class is abstract and must be subclassed and implemented
/// using a particular binary instrumentation library, but reusing the
/// inter-process communication framework.
class OutOfProcessProvider : ProbeProvider {
private:

public:

  /// For out-of-process instrumentors initialization is usually a NOOP.
  void initialize(ProbeId probe_id, ProbeArg probe_arg) { }

  /// Send a request to the instrumentor process to activate the probe.
  bool activate(ProbeId probe_id, InstrumentationFunc func) {
  }

  /// Send a request to the instrumentor process to deactivate the probe.
  bool deactivate(ProbeId probe_id) {
  }

  /// Call this inside the instrumentor process to perform a remote
  /// invocation of the callback within the instrumentee process.
  ///
  /// This must serialize and communicate the probe metadata.
  void instrumentor_invoke_callback(const ProbeMetaData* pmd) {
    // FINISHME
  }

  /// Called within the instrumentor process to mutate the
  /// instrumentee process.
  virtual void instrumentor_activate() = 0;

  virtual void instrumentor_deactivate() = 0;

};


class DyninstProbeProvider : ProbeProvider {

private:
  BPatch_process * proc;
  BPatch_image * image;

  BPatch_point * codeLoc;

public:
  DyninstProbeProvider(Callback c) // : callback(c)
  {
    callback = c;

    BPatch bpatch;
    pid_t child_pid;
    printf("Constructing DyninstProbeProvider... forking process %p\n", callback);
    if (child_pid = fork()) {
      // Parent
      printf(" # In parent process, serving as mutator, child pid = %d\n", child_pid);

      // Option 1: mutating a subprocess:
      // // BPatch_process *proc = bpatch.processCreate(argv[1], argv + 2, NULL, stdin, stdout, stderr);
      // BPatch_process *proc = bpatch.processCreate(argv[1], argv + 2);

      // auto procs = bpatch.getProcesses();
      // printf("GOT Processes! %d\n", (int)procs->size());

      proc = bpatch.processAttach("child", child_pid);

      //bpatch.setTrampRecursive(true);
      //bpatch.setSaveFPR(false);
      //bpatch.setInstrStackFrames(false);
      //BPatch_process *proc = bpatch.processAttach(argv[1], atoi(argv[2]));
      image = proc->getImage();

      std::vector<BPatch_sourceObj*> children;
      image->getSourceObj(children);

      for(BPatch_sourceObj* m : children) {
        std::vector<BPatch_sourceObj*> funs;
        m->getSourceObj(funs);
        char buf[256];
        char* modname = ((BPatch_module*)m)->getName(buf,256);
        if (! strcmp(modname, "DEFAULT_MODULE")) {
          printf("  GOT Module: %s, %d funs inside\n", modname, (int)funs.size());
          for(BPatch_sourceObj* f : funs) {
            BPatch_function* fptr = dynamic_cast<BPatch_function *>(f);

            const std::string name = fptr->getName();

            std::cout << "    Got function: " << name << std::endl;

            std::vector<BPatch_point*>* entries = fptr->findPoint(BPatch_entry);
            std::vector<BPatch_point*>* exits   = fptr->findPoint(BPatch_exit);

            assert (entries->size() == 1);

            // UGH: this will need to be serialized across processes:

            // TODO: Move some of this initialization to a proper constructor or
            // construction function:
            for ( BPatch_point* entry : *entries) {
              ProbeMetaData* pmd1 = new ProbeMetaData();
              pmd1->func_name = name;
              pmd1->probe_name = "";
              pmd1->state = ProbeState::DEACTIVATED;
              pmd1->probe_context = ProbeContext::ENTRY;
              pmd1->probe_loc = (ProbeLoc)entry; // Point directly to the BPatch_point.
              callback(pmd1);
            }

            for ( BPatch_point* exit : *exits) {
              ProbeMetaData* pmd2 = new ProbeMetaData();

              pmd2->func_name = std::string(name);
              pmd2->probe_name = "";
              pmd2->state = ProbeState::DEACTIVATED;
              pmd2->probe_context = ProbeContext::EXIT;
              pmd2->probe_loc = (ProbeLoc)exit;
              callback(pmd2);
            }
          }
        }
      }

      // ------------------------------------------------------------

      // printf(" # Is child process stopped? %d\n", proc->isStopped()); fflush(stdout);
      proc->continueExecution();
      // printf(" # After continueExecution, child process stopped? %d\n", proc->isStopped()); fflush(stdout);

      long long spin = 0;
      while (!proc->isTerminated()) {
          bpatch.waitForStatusChange();
          spin++;
      }
      // printf(" # Child finished after %lld waits.  Mutator/parent exiting.\n", spin);
      exit(0); // Exit the whole process.
    } else {
      child_pid = getpid();
      // printf("  -> In child process... sending STOP to self \n");
      kill(child_pid, SIGSTOP);
      // auto procs = bpatch.getProcesses();
      // printf("  Child process sees, #processes = %d\n", (int)procs->size());
      // printf("  -> DyninstProbeProvider constructor finished... \n");
    }
  }

  void initialize(ProbeId probe_id, ProbeArg probe_arg) {
    // Nothing to do here because for this ProbeProvider,
    // UNINITIALIZED, INITIALIZING, and DEACTIVATED states are
    // identical.
    printf("Initializing... %d\n", (int)probe_id);
  }

  bool activate(ProbeId probe_id, InstrumentationFunc func) {
    printf("in-process activate... %d\n", (int)probe_id);
  }

  bool instrumentor_activate(ProbeId probe_id, InstrumentationFunc func) {

    printf("out-of-process activate... %d\n", (int)probe_id);

    std::vector<BPatch_function *> probeFuns;
    Dyninst::Address addr = (Dyninst::Address)func;
    image->findFunction(addr, probeFuns);

// FIXME: this is bogus:

    assert (probeFuns.size() == 1);
    std::vector<BPatch_snippet*> args;
    BPatch_funcCallExpr call_fun(*probeFuns[0], args);

    // Instrument entry and exit...
    BPatchSnippetHandle* handle =
      proc->insertSnippet(call_fun, (probeFuns[0]->findPoint(BPatch_entry))[0]);

    printf("INSERTED TEST FUNCTION:");
  }

  bool deactivate(ProbeId probe_id) {
    printf("deactivate... %d\n", (int)probe_id);
  }
};


int main (int argc, const char* argv[])  {
  printf("Hello from mutator\n");
  DyninstProbeProvider dpp( & registerProbeCallback );

  printf("  -> In main function... calling foo \n");
  foo();

  return 0;
}
