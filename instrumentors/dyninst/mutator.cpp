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

int main (int argc, const char* argv[])  {
  printf("Hello from mutator\n");

  BPatch bpatch;
  pid_t child_pid;

  if (child_pid = fork()) {
    // Parent
    printf(" # In parent process, serving as mutator, child pid = %d\n", child_pid);

    // Option 1: mutating a subprocess:
    // // BPatch_process *proc = bpatch.processCreate(argv[1], argv + 2, NULL, stdin, stdout, stderr);
    // BPatch_process *proc = bpatch.processCreate(argv[1], argv + 2);

    // auto procs = bpatch.getProcesses();
    // printf("GOT Processes! %d\n", (int)procs->size());

    BPatch_process *proc = bpatch.processAttach("child", child_pid);

    //bpatch.setTrampRecursive(true);
    //bpatch.setSaveFPR(false);
    //bpatch.setInstrStackFrames(false);
    //BPatch_process *proc = bpatch.processAttach(argv[1], atoi(argv[2]));
    BPatch_image *image = proc->getImage();

    std::vector<BPatch_function *> foo_fns, bar_fns;
    image->findFunction("foo", foo_fns);
    image->findFunction("bar", bar_fns);

    std::vector<BPatch_snippet*> args;
    BPatch_funcCallExpr call_bar(*bar_fns[0], args);

    //    unsigned long insert_timings[N];
    //    unsigned long deletion_timings[N];

    BPatchSnippetHandle* handle =
      proc->insertSnippet(call_bar, (foo_fns[0]->findPoint(BPatch_entry))[0]);


    // ------------------------------------------------------------

    printf(" # Is child process stopped? %d\n", proc->isStopped()); fflush(stdout);
    proc->continueExecution();
    printf(" # After continueExecution, child process stopped? %d\n", proc->isStopped()); fflush(stdout);
    // proc->detach(true);

    long long spin = 0;
    while (!proc->isTerminated()) {
        bpatch.waitForStatusChange();
        spin++;
    }
    printf(" # Child finished after %lld waits.  Mutator/parent exiting.\n", spin);
    return 0;

  } else {
    child_pid = getpid();
    printf("  -> In child process... sending STOP to self \n");
    kill(child_pid, SIGSTOP);

    // auto procs = bpatch.getProcesses();
    // printf("  Child process sees, #processes = %d\n", (int)procs->size());

    printf("  -> In child process... calling foo \n");
    foo();
    printf("  -> Child exiting \n");
    return 0;
  }
}


#if 0
int main (int argc, const char* argv[]) {


    for (int i=0; i< N; i++) {

      ticks start = getticks();
      BPatchSnippetHandle* handle =
        proc->insertSnippet(call_bar, (foo_fns[0]->findPoint(BPatch_entry))[0]);
      ticks end = getticks();

      ticks elapsed1 = end - start;

      start = getticks();
      proc->deleteSnippet(handle);
      end = getticks();

      ticks elapsed2 = end - start;

      insert_timings[i] = elapsed1;
      deletion_timings[i] = elapsed2;

    }

    for (int i=0; i< N; i++) {
      fprintf(stderr, "Instrumentation %d cost : %lu percentage : %lf\n", i, insert_timings[i],
          ((double)insert_timings[i] / insert_timings[0]) * 100);
    }

    fprintf(stderr, "\n\n");

    for (int i=0; i< N; i++) {
      fprintf(stderr, "Instrumentation  deletion %d cost : %lu percentage : %lf\n", i, deletion_timings[i],
          ((double)deletion_timings[i] / deletion_timings[0]) * 100);
    }


    /*
    ticks start = getticks();
    BPatchSnippetHandle* handle =
      proc->insertSnippet(call_bar, (foo_fns[0]->findPoint(BPatch_entry))[0]);
    ticks end = getticks();

    ticks elapsed1 = end - start;

    start = getticks();
    proc->deleteSnippet(handle);
    end = getticks();

    ticks elapsed2 = end - start;

    start = getticks();
    handle =
      proc->insertSnippet(call_bar, (foo_fns[0]->findPoint(BPatch_entry))[0]);
    end = getticks();

    ticks elapsed3 = end - start;

    start = getticks();
    proc->deleteSnippet(handle);
    end = getticks();

    ticks elapsed4 = end - start;

    start = getticks();
    handle =
      proc->insertSnippet(call_bar, (foo_fns[0]->findPoint(BPatch_entry))[0]);
    end = getticks();

    ticks elapsed5 = end - start;

    fprintf(stderr, "First instrumentation cost : %lu\n", elapsed1);
    fprintf(stderr, "Second instrumentation cost : %lu percentage : %lf \n", elapsed3, ((double)elapsed3/elapsed1) * 100);
    fprintf(stderr, "Third instrumentation cost : %lu percentage : %lf\n\n", elapsed5, ((double)elapsed5/elapsed1) * 100);

    fprintf(stderr, "First instrumentation removal cost : %lu\n", elapsed2);
    fprintf(stderr, "Second instrumentation removal cost : %lu percentage : %lf\n", elapsed4, ((double)elapsed4/elapsed2) * 100);

    */

    // bpatch.setInstrStackFrames(false);
    // proc->insertSnippet(BPatch_nullExpr(), (foo_fns[0]->findPoint(BPatch_entry))[0]);
    // proc->insertSnippet(BPatch_nullExpr(), (foo_fns[0]->findPoint(BPatch_entry))[0]);

}
#endif
