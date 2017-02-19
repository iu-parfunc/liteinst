#include "BPatch.h"
#include "BPatch_point.h"
#include "BPatch_function.h"
#include "vector"
#include "cycle.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string>

using std::string;
using std::to_string;

int main (int argc, const char* argv[]) {

  /*
   long pid;
   char buf[20];
   fgets(buf, sizeof buf, stdin);
   if (buf[strlen(buf)-1] == '\n') {
     pid = atoi(buf);
     printf("[mutator] Mutatee pid is : %d\n", pid);
   } else {
     exit(-1);
   } 
   */
 

    FILE* fp = fopen(argv[1], "r");
    if (fp == nullptr) {
      printf("[mutator] Error reading the mutatee's pid. Exiting..\n");
      exit(-1);
    }

    char* line = nullptr;
    ssize_t read;
    size_t len = 0;
    if ((read = getline(&line, &len, fp)) == -1) {
      printf("[mutator] Invalid data read for mutatee's pid. Exiting..\n");
      exit(-1);
    }

    fclose(fp);

    int pid = atoi(line);
    int n_funcs = atoi(argv[2]);

    printf("[mutator] Mutatee pid : %d\n", pid);
    printf("[mutator] Number of functions : %d\n", n_funcs);
    BPatch bpatch;
    // BPatch_process *proc = bpatch.processCreate(argv[1], argv + 3);
    ticks start = getticks();
    BPatch_process *proc = bpatch.processAttach("mutatee", pid);
    ticks end = getticks();

    printf("ATTACH_COST : %ld\n", end-start);
    //bpatch.setTrampRecursive(true);
    //bpatch.setSaveFPR(false);
    //bpatch.setInstrStackFrames(false);
    //BPatch_process *proc = bpatch.processAttach(argv[1], atoi(argv[2]));
    BPatch_image *image = proc->getImage();

    std::vector<BPatch_function *> foo_fns, bar_fns;
    foo_fns.reserve(n_funcs);
    for (int i=0; i< n_funcs; i++) {
      image->findFunction(string("func"+to_string(i)).c_str(), foo_fns);
      image->findFunction("bar", bar_fns);
    }

    unsigned long insert_timings[n_funcs];
    unsigned long deletion_timings[n_funcs];

    for (int i=0; i< n_funcs; i++) {
      std::vector<BPatch_snippet*> args;
      BPatch_funcCallExpr call_bar(*bar_fns[0], args);

      // proc->continueExecution();

      ticks start = getticks();
      // proc = bpatch.processAttach("mutatee", pid);
      BPatchSnippetHandle* handle = 
        proc->insertSnippet(call_bar, (foo_fns[i]->findPoint(BPatch_entry))[0]);
      // proc->continueExecution();
      ticks end = getticks();

      ticks elapsed1 = end - start;

      start = getticks();
      proc->deleteSnippet(handle);
      end = getticks();

      ticks elapsed2 = end - start;

      insert_timings[i] = elapsed1; 
      deletion_timings[i] = elapsed2; 

    }

    ticks total = 0;
    for (int i=0; i< n_funcs; i++) {
      total += insert_timings[i];
      /*
      fprintf(stderr, "Instrumentation %d cost : %lu percentage : %lf\n", i, insert_timings[i], 
          ((double)insert_timings[i] / insert_timings[0]) * 100);
          */
    }

    printf("INSERTION_COST : %ld\n", total / n_funcs);

    fprintf(stderr, "\n\n");

    total = 0;
    for (int i=0; i< n_funcs; i++) {
      total += deletion_timings[i];
      /*
      fprintf(stderr, "Instrumentation  deletion %d cost : %lu percentage : %lf\n", i, deletion_timings[i], 
          ((double)deletion_timings[i] / deletion_timings[0]) * 100);
          */
    }

    printf("DELETION_COST : %ld\n", total / n_funcs);

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

    start = getticks();
    proc->continueExecution();
    end = getticks();

    printf("COST_RESUMING : %ld\n", end - start);

    proc->detach(true);

    // sleep(20);

    /*
    while (!proc->isTerminated()) {
        bpatch.waitForStatusChange();
    }
    */

    return 0;
}
