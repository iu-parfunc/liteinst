#include "BPatch.h"
#include "BPatch_point.h"
#include "BPatch_function.h"
#include <stdio.h>
#include <vector>
#include "ProbeProvider.hpp"
#include <unistd.h>

#include <sys/types.h>
#include <signal.h>

#define tic() do { struct timespec ts_start, ts_end; clock_gettime(CLOCK_MONOTONIC, &ts_start)
#define toc() clock_gettime(CLOCK_MONOTONIC, &ts_end); \
              fprintf(stderr,"%lfs\n", (ts_end.tv_sec - ts_start.tv_sec) + (double)(ts_end.tv_nsec - ts_start.tv_nsec)/1e9); } \
              while (0)

void foo() {
    // sleep(1);
    fprintf(stderr,"[mutator] I am foo()\n");
    return;
}

void bar() {
    fprintf(stderr,"[mutator] I am bar()\n");
    return;
}

ProbeMetaData * bar_probe_entry = 0;
ProbeMetaData * bar_probe_exit = 0;

// RRN: Why is this const?
void registerProbeCallback (const ProbeMetaData* pmd) {
  const char* name = pmd->func_name.c_str();
  if (! strcmp(name,"bar")) {
    fprintf(stderr,"     Register probe in pid %d : %p / %d, %s,  %d %d\n",
            getpid(), pmd, pmd->probe_context, name,
            (pmd->probe_context == ProbeContext::ENTRY),
            (pmd->probe_context == ProbeContext::EXIT)
            );
    if (pmd->probe_context == ProbeContext::ENTRY) {
      bar_probe_entry = (ProbeMetaData*)pmd;
      fprintf(stderr, "TMP: set bar_probe_entry\n");
      abort();
    }
    else if (pmd->probe_context == ProbeContext::EXIT)
      bar_probe_exit = (ProbeMetaData*)pmd;
  }
  else fprintf(stderr,".");
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
class OutOfProcessProvider : protected ProbeProvider {

private:
protected:
  pid_t child_pid;

  uint64_t cached_num_funs = -1;

  // After the fork this is set to 1 for child, 0 for parent:
  int am_child = -1;

  int pipefd_tochild[2];
  int pipefd_toparent[2];

public:

  OutOfProcessProvider (Callback cb) : ProbeProvider(cb) {
    fprintf(stderr," * Constructing OutOfProcess... forking process %p\n", callback);
    // fork_instrumentor(); // Can't do this here.
  }

  // --------------------------------------------------------------
  // Probe state transitions:

  /// For out-of-process instrumentors initialization is usually a NOOP.
  void initialize(ProbeId probe_id, ProbeArg probe_arg) { }

  /// Send a request to the instrumentor process to activate the probe.
  bool activate(ProbeId probe_id, InstrumentationFunc func) {
    fprintf(stderr,"ACTIVATE %ld %ld\n", (long)probe_id, (long)func);
  }

  /// Send a request to the instrumentor process to deactivate the probe.
  bool deactivate(ProbeId probe_id) {
    fprintf(stderr,"DEACTIVATE %ld\n", (long)probe_id);
  }

  // --------------------------------------------------------------
  // All functions beginning with "instrumentee_" are called in the
  // original, instrumentee process:

  /// The child process (instrumentee) waits for the parent before proceeding:
  /// It is the responsibility of the parent process to continue it.
  virtual void instrumentee_stop() {
    child_pid = getpid();
    fprintf(stderr, " ? Pausing child process %d until parent says to go.\n", child_pid);
    kill(child_pid, SIGSTOP);
    fprintf(stderr, " ? Child %d past kill \n", child_pid);

    char buf[1024];
    // while (read(pipefd_tochild[0], &buf, 1) > 0) write(STDERR_FILENO, &buf, 1);
    fprintf(stderr, " ? Child Finished echoing output... \n");
  }

  /// Only the instrumentor process can determine the number of threads.
  /// Thus, this method communicates with the instrumentor thread
  uint64_t getNumberOfFunctions() {
    if (cached_num_funs == -1 ) {
      fprintf(stderr, "FINISHME: need to implement getNumberOfFunctions\n");
      return -1;
    } else {
      return cached_num_funs;
    }
  }

  // --------------------------------------------------------------
  // All functions beginning with "instrumentor_" are called in the
  // instrumentor process:

  virtual void instrumentor_initialize() = 0;

  virtual uint64_t instrumentor_getNumberOfFunctions() = 0;

  // TODO: provide a default implementation of this:
  virtual void instrumentor_continue_instrumentee() = 0;

  virtual void wait_for_instrumentee_completion() = 0;

  /// Call this inside the instrumentor process to perform a remote
  /// invocation of the callback within the instrumentee process.
  ///
  /// This must serialize and communicate the probe metadata.
  void instrumentor_invoke_callback(const ProbeMetaData* pmd) {
    fprintf(stderr," FINISHME: instrumentor_invoke_callback %p\n", pmd);
    char msg[256];
    int numChars = sprintf(msg, "CALLBACK %ld\n", (long)pmd);
    write(pipefd_tochild[1], msg, numChars);
  }

  /// Called within the instrumentor process to mutate the
  /// instrumentee process.
  virtual bool instrumentor_activate(ProbeId probe_id, InstrumentationFunc func) = 0;

  virtual bool instrumentor_deactivate(ProbeId probe_id) = 0;

  // ------------------------------------------------------------

  /// This must be called in the constructor of our subclass.  It
  /// cannot be called in the OutOfProcess constructor, because the
  /// child class is not fully initialized yet.
  void fork_instrumentor() {
    // First set up communication mechanism:
    if (pipe(pipefd_toparent) == -1) {
        perror("pipe");
        exit(EXIT_FAILURE);
    }
    if (pipe(pipefd_tochild) == -1) {
        perror("pipe");
        exit(EXIT_FAILURE);
    }
    fflush(stdout); fflush(stderr); // Seeing weird duplication of output.
    child_pid = fork();
    if (child_pid == -1) {
      perror("Fork failed.");
      exit(EXIT_FAILURE);
    }
    if (child_pid > 0) {
      // PARENT is the INSTRUMENTOR.
      am_child = 0; // Hit that copy-on-write early!

      close(pipefd_toparent[1]); // Close write end of inbound.
      close(pipefd_tochild[0]);  // And read end of outbound.

      // fprintf(stderr,"About to call initialize\n");
      instrumentor_initialize();
      // fprintf(stderr,"About to call continue instrumentee\n");
      instrumentor_continue_instrumentee();
      // fprintf(stderr,"About to call continue completion\n");
      wait_for_instrumentee_completion();
      // fprintf(stderr,"About to exit\n");
      exit(0); // Exit the whole process.
    } else {
      am_child = 1;

      close(pipefd_toparent[0]); // read end of outbound.
      close(pipefd_tochild[1]);  // write end of inbound.

      instrumentee_stop(); // Wait for the parent to tell us to go.
    }
  }

};


class DyninstProbeProvider : OutOfProcessProvider {

private:
  BPatch bpatch;
  BPatch_process * proc;
  BPatch_image * image;

  BPatch_point * codeLoc;

public:
  DyninstProbeProvider(Callback c) : OutOfProcessProvider(c)
  {
    // Most of the initialization work doesn't happen until we fork
    // into a different process.
    fork_instrumentor();
  }

  void instrumentor_initialize() {
      fprintf(stderr," # In parent process, serving as mutator, child pid = %d\n", child_pid);

      // Option 1: mutating a subprocess:
      // // BPatch_process *proc = bpatch.processCreate(argv[1], argv + 2, NULL, stdin, stdout, stderr);
      // BPatch_process *proc = bpatch.processCreate(argv[1], argv + 2);

      // auto procs = bpatch.getProcesses();
      // fprintf(stderr,"GOT Processes! %d\n", (int)procs->size());

      fprintf(stderr," # Now parent attaches to child process..\n");
      proc = bpatch.processAttach("child", child_pid);
      fprintf(stderr," # Successfully attached to child process..\n");

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
          fprintf(stderr," * GOT Module: %s, %d funs inside\n", modname, (int)funs.size());
          tic();
          for(BPatch_sourceObj* f : funs) {
            BPatch_function* fptr = dynamic_cast<BPatch_function *>(f);

            const std::string name = fptr->getName();

            // std::cout << "    Got function: " << name << std::endl;

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

              // This uses ubiprof rather than fastinst.hpp level functionality to invoke the callback:
              // callback(pmd1);
              // Instead we call a method in the parent class to send this over to the child process:
              instrumentor_invoke_callback(pmd1);
            }

            for ( BPatch_point* exit : *exits) {
              ProbeMetaData* pmd2 = new ProbeMetaData();

              pmd2->func_name = std::string(name);
              pmd2->probe_name = "";
              pmd2->state = ProbeState::DEACTIVATED;
              pmd2->probe_context = ProbeContext::EXIT;
              pmd2->probe_loc = (ProbeLoc)exit;

              // callback(pmd2);
              instrumentor_invoke_callback(pmd2);
            }
          }
          clock_gettime(CLOCK_MONOTONIC, &ts_end);
          fprintf(stderr,"\n * Done processing funs in module, time spent: ");
          toc();
        }
      }
  }

  // This is called instrumentor-side and then communicated inter-process.
  uint64_t instrumentor_getNumberOfFunctions() {
    fprintf(stderr, "FINISHME: dyninst: iterate over functions in image, counting them.\n");
    return -1;
  }

  void instrumentor_continue_instrumentee() {
    // ------------------------------------------------------------
    // fprintf(stderr," # Is child process stopped? %d\n", proc->isStopped());
    proc->continueExecution();
    // fprintf(stderr," # After continueExecution, child process stopped? %d\n", proc->isStopped());
  }

  void wait_for_instrumentee_completion() {
      long long spin = 0;
      while (!proc->isTerminated()) {
          bpatch.waitForStatusChange();
          spin++;
      }
      // fprintf(stderr," # Child finished after %lld waits.  Mutator/parent exiting.\n", spin);
  }

  void initialize(ProbeId probe_id, ProbeArg probe_arg) {
    // Nothing to do here because for this ProbeProvider,
    // UNINITIALIZED, INITIALIZING, and DEACTIVATED states are
    // identical.
    fprintf(stderr,"Initializing... %d\n", (int)probe_id);
  }

  bool activate(ProbeId probe_id, InstrumentationFunc func) {
    fprintf(stderr,"in-process activate... %d\n", (int)probe_id);
  }

  bool instrumentor_activate(ProbeId probe_id, InstrumentationFunc func) {

    fprintf(stderr,"out-of-process activate... %d\n", (int)probe_id);

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

    fprintf(stderr,"INSERTED TEST FUNCTION:");
  }

  bool instrumentor_deactivate(ProbeId probe_id) {
    fprintf(stderr,"deactivate... %d\n", (int)probe_id);
  }

};


int main (int argc, const char* argv[])  {
  fprintf(stderr," * Hello from main function, pid = %d\n", getpid());
  DyninstProbeProvider dpp( & registerProbeCallback );
  fprintf(stderr," * DyninstProbeProvider created, running rest of main, pid = %d.\n", getpid());

  fprintf(stderr," bar entry %p and exit %p probes\n", bar_probe_entry, bar_probe_exit);

  fprintf(stderr,"  -> In main function... calling foo, which MAY call bar. \n");
  foo();
  fprintf(stderr,"  -> In main function... done calling foo. \n");

  return 0;
}
