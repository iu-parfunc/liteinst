#include "BPatch.h"
#include "BPatch_point.h"
#include "BPatch_function.h"
#include "vector"
#include "cycle.h"

#include <unistd.h>

int N = 500;

int main (int argc, const char* argv[]) {
    BPatch bpatch;
    BPatch_process *proc = bpatch.processCreate(argv[1], argv + 2);
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

    unsigned long insert_timings[N];
    unsigned long deletion_timings[N];

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

    proc->continueExecution();
    // proc->detach(true);

    // sleep(20);

    while (!proc->isTerminated()) {
        bpatch.waitForStatusChange();
    }

    return 0;
}
