#include "BPatch.h"
#include "BPatch_point.h"
#include "BPatch_function.h"
#include "vector"
#include "cycle.h"

#include <unistd.h>

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

    proc->insertSnippet(call_bar, (foo_fns[0]->findPoint(BPatch_entry))[0]);
    proc->continueExecution();

    while (!proc->isTerminated()) {
        bpatch.waitForStatusChange();
    }

    return 0;
}
