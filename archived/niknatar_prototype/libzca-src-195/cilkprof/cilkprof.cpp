/*
  Copyright (C) 2012 Intel Corporation
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.
  * Neither the name of Intel Corporation nor the names of its
    contributors may be used to endorse or promote products derived
    from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
 */

/* Cilkprof is a pintool that generates work and span data for a Cilk
 * program execution.  Output shows work and span on a basic block basis
 * and a caller/callee basis.  You can divide the output by setting the
 * following environment variables:
 *    CILKPROF_BB_OUT: basic-block work/span data.
 *    CILKPROF_CC_OUT: caller/callee work/span data.
 * You can invoke the program like so:
 * % export CILKPROF_BB_OUT=bb.csv
 * % export CILKPROF_CC_OUT=cc.csv
 * % pinbin -t cilkprof.so -- ./your-program arg1 arg2 ... argn
 */

#include <assert.h>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <map>
#include <pin.H>
#include <limits.h>
// Total number of basic blocks in the program.
unsigned int num_basic_blocks;
#include "sprof.h"
#include <stack>
#include <string>
#include "util.h"
#include <vector>
#include <zca_table.h>

#ifdef TRACE_CALLBACKS
#define DBG_TRACE_CALLBACK(...) fprintf(stderr, __VA_ARGS__)
#else
#define DBG_TRACE_CALLBACK(...)
#endif

#ifdef TRACE_PROFILER
#define DBG_PROFILER_TRACE(...) fprintf(stderr, __VA_ARGS__)
#else
#define DBG_PROFILER_TRACE(...)
#endif

// BURDEN_COST = estimated cost in number-of-instructions of a burdened
// edge in a burdened DAG.
#define BURDEN_COST 15000

typedef unsigned long long work_t;
typedef sprof_single_t ss_t;
typedef sprof_pair_t sp_t;

enum function_type
{
    serial_function,
    spawn_helper_function,
    spawning_function
};

typedef struct
{
    function_type type;
    string name;
} function_info;

// Keep track of spawning and spawn helper functions by the address of their
// enter_begin annotations
typedef std::map<ADDRINT, function_info> function_type_map;
static function_type_map map_function_types;

typedef std::map<std::string, function_type> function_names_to_types_map;
static function_names_to_types_map map_function_names_to_types;

/*

   Span Algorithm Description:

   The program is represented by a DAG formed by spawns that fork and
   syncs that join:

           continue       continue       sync
   -------o--------------o--------------o------
           \        spawn \____________/|
      spawn \___________________________|


   Point s is the beginning of the function.  Initially, x is set to s.
   The "span" value tracks the distance from point s to point x.  A
   "continuation" variable tracks the distance from x to the current
   point of execution in the current function.  "longest_child" is the
   length of a spawned child in the function, the beginning of which is
   pointed to by x.  It is a lower-bound on the distance from x to the
   sync.

   The longest_child may actually be shorter than another spawned child
   function, if the shorter function was spawned after the longer one.
   The shorter child will be a tighter bound if the continuation length
   between the two spawned functions makes up the differences.  So
   longest_child is not necessarily the most accurate name for what is
   being measured, but it should give the reader the general idea.

   x is advanced whenever a new longest_child is discovered and when a
   sync is encountered.  The following pseudo-code describes how to
   accomplish this:

   Function call:
     push continuation -> c_stack
     continuation = 0

   Enter a Cilk context:
     push span -> s_stack
     push continuation -> c_stack
     push longest_child -> l_stack
     span = 0
     continuation = 0
     longest_child = 0

   Spawn-helper detaches:
     push continuation -> c_stack
     continuation = 0

   Function executes an instruction:
     continuation += 1
     // update basic block profile

   Spawn helper returns:
     child_cont = continuation
     helper_cont = pop c_stack
     // Update cc-pair profiles for child_cont and helper_cont.
     // Update cc-pair profile for instantaneous span.
     continuation = pop c_stack
     continuation += helper_cont
     if (longest_child < continuation + child_cont) then
       longest_child = child_cont
       span += continuation
       continuation = 0
     fi
     // spawn helper does _not_ execute "Function returns"

   Function syncs:
     span += max(continuation, longest_child)
     continuation = 0
     longest_child = 0

   Leave Cilk context:
     context_cont = continuation + span
     span = pop s_stack
     continuation = pop c_stack
     longest_child = pop l_stack
     continuation += context_cont

   Function returns:
     child_cont = continuation
     continuation = pop c_stack
     // Update cc-pair profile for child_cont cost.
     // Update cc-pair profile for instantaneous span.
     continuation += child_cont

   When the program terminates, the span of the program will be in the
   "continuation" variable (the "span" variable in the algorithm above
   is an intermediate, and temporary, value).

   Taking a snapshot of the total span at any function return will give
   an "instantaneous" span, local to that function location.  One should
   expect that the instantaneous span is generally less useful than the
   global ("true") span.  However, beyond merely satisfying curiosity
   regarding functions that don't actually appear on the span, the
   instantaneous span may be interesting when compared against the true
   contribution of a function to the span.

   Work Algorithm Description:

   "Work" is a trivial count of instructions that ignores all parallel
   constructs.

   Like span, it is tracked on a per-basic-block basis, and on a caller/
   callee basis.  Unlike span, both the basic block and caller/callee
   constructs are flat, since there are no logically parallel reductions.

   Note that this sense of work cannot be used to determine parallelism of
   a function that appears on the span, however.  Since a function may be
   called many times at a single callsite, and since it is possible for
   some-but-not-all of those calls to appear on the span, work / span does
   not give a proper value of parallelism for the function.  This work can
   be used with the instantaneous span, but not with the global span.

   Therefore a second sense of "work" is valuable: the work of a function
   as it appears on the span.  This span-based work needs to take parallel
   constructs into account, and is tracked in much the same way as span
   except that the reductions from children into parents only apply to the
   profiles as a whole, and not instruction counts.  When a cc-pair entry
   is updated, it is updated with the whole work of the function, as
   calculated by the trivial "work" algorithm.

   Practically, this span-based work should be done alongside the span
   algorithm.

   The following algorithm tracks work in both senses:

   Function call:
     push work -> w_stack
     work = 0

   Spawn-helper detaches:
     push work -> w_stack
     work = 0

   Function executes an instruction:
     work += 1
     // update basic block profile

   Spawn helper returns:
     child_work = work
     helper_work = pop w_stack
     // Update cc-pair profile for child_work + helper_work cost.
     // Retain child_work for child_cont in span calculation.
     // Retain hepler_work for helper_cont in span calculation.
     work += child_work + helper_work

   Function returns:
     child_work = work
     work = pop w_stack
     // Update cc-pair profile for child_work cost.
     work += child_work

 */

struct basic_block_t
{
    enum burden_t { BURDEN_NONE = 0,
                    BURDEN_RETURN_FROM_SPAWN,
                    BURDEN_CONTINUATION };

    basic_block_t (BBL bbl) :
        address(BBL_Address(bbl)),
        insn_count(BBL_NumIns(bbl)),
        burden(BURDEN_NONE),
        function_name(RTN_FindNameByAddress(BBL_Address(bbl))),
        is_unresolved_plt(false)
    {
        int column;
        PIN_GetSourceLocation(address, &column, &this->line, &this->filename);
        if (this->filename == "") {
            this->filename = "unknown";
        }
        if (this->function_name == ".plt") {

            // .plt is the trampoline section between modules.  See
            // trace_callback() for details on how this is dealt with.

            is_unresolved_plt = true;

        } else if (this->function_name == "") {
            this->function_name = "unknown";
        }
    }

    ADDRINT address;
    unsigned int insn_count;
    std::string filename;
    int line;
    burden_t burden;
    std::string function_name;
    bool is_unresolved_plt;

    /*
     * Return a string-ified representation of the burden field.
     */
    const char *burden_str ()
    {
        switch (this->burden) {
        case BURDEN_NONE:
            return "none";
        case BURDEN_RETURN_FROM_SPAWN:
            return "return from spawn";
        case BURDEN_CONTINUATION:
            return "continuation";
        default:
            cilkprof_assert(0);
            exit(1);
        }
    }
};

// Mapping of indexes to addresses.
static std::vector<basic_block_t> address_table;

// Mapping of addreses to basic block numbers
typedef struct std::multimap<ADDRINT, unsigned int> map_address_2_bblock_t;
static map_address_2_bblock_t address_2_bblock_map;


unsigned int duplicate_basic_blocks = 0;

/*
 * Structure to associate callers with callees.
 */
union cc_pair_t
{
    struct {
        UINT32 caller;
        UINT32 callee;
    };
    UINT64 comp;

    /*
     * The comparison class for storing a cc-pair in a map.
     */
    struct comparator_t
    {
        bool operator () (const cc_pair_t &left,
                          const cc_pair_t &right) const
        {
            return left.comp < right.comp;
        }
    };
};

/*
 * Structure for associating cc-pairs with information about them.
 */
struct cc_data_t
{
    cc_pair_t cc_pair;
    UINT32 count;    // Number of times the cc-pair is on the stack.
};

// Track the number of pairs we have seen in this execution.
unsigned int num_cc_pairs;

// Data structures for getting a cc-pair from an index and vice versa:
// Map: cc-pair -> index
// Table: index -> cc-data (includes cc-pair info)
typedef std::map<cc_pair_t, unsigned int, cc_pair_t::comparator_t> cc_map_t;
static cc_map_t cc_map;
static std::vector<cc_data_t> cc_data_table;

static std::vector<unsigned int> shadow_stack;

struct span_t
{
    span_t () :
        self(),
        accum(),
        work(),
        total(0)
    {
    }

    void add_and_clear (span_t *other)
    {
        total += other->total;
        other->total = 0;
        self.add_and_clear(&other->self);
        accum.add_and_clear(&other->accum);
        work.add_and_clear(&other->work);
    }

    void assign_and_clear (span_t *other)
    {
        total = other->total;
        self.assign_and_clear(&other->self);
        accum.assign_and_clear(&other->accum);
        work.assign_and_clear(&other->work);
    }
    void clear ()
    {
        total = 0;
        self.clear();
        accum.clear();
        work.clear();
    }

    void bb (UINT32 index, UINT32 count)
    {
        self.increment(ss_t(index));
        total += count;
    }

    void cc (unsigned int index, work_t span_count, work_t work_count)
    {
        if (index != UINT32_MAX) {
            accum.increment(sp_t(index, span_count));
            work.increment(sp_t(index, work_count));
        }
    }

    void finalize ()
    {
        self.tabulate();
        accum.tabulate();
        work.tabulate();
    }

    // Self-cost: basic block-based values.
    sprof_t<ss_t> self;

    // Accumulated-cost: caller/callee-based values.
    sprof_t<sp_t> accum;

    // Amount of accumulated work relating to the span.
    sprof_t<sp_t> work;

    // Total span instruction count.  This is maintained for comparisons.
    work_t total;
};

class profiler_t
{
public:

    profiler_t () :
        s_stack(),
        c_stack(),
        l_stack(),
        span(NULL),
        cont(NULL),
        lchild(NULL),
        ispan(),
        work(0),
        w_stack(),
        bb_profile(),
        cc_profile()
    {
    }

    void function_call ()
    {
        DBG_PROFILER_TRACE("function_call enter\n");
        // *SPAN*
        // Function call:
        //   push continuation -> c_stack
        //   continuation = 0

        c_stack.push_back(cont);
        cont = new span_t();

        // *WORK*
        // Function call:
        //   push work -> w_stack
        //   work = 0

        w_stack.push_back(work);
        work = 0;
        DBG_PROFILER_TRACE("function_call leave\n");
    }

    void execute_basic_block (UINT32 index, UINT32 n)
    {
        DBG_PROFILER_TRACE("execute_basic_block enter - index: %d, n: %d\n", index, n);
        // *SPAN*
        // Function executes an instruction:
        //   continuation += 1
        //   // update basic block profile

        cont->bb(index, n);

        // *WORK*
        // Function executes an instruction:
        //   work += 1
        //   // update basic block profile

        cilkprof_assert(bb_profile.size() > index);
        work += n;
        ++bb_profile[index];

        DBG_PROFILER_TRACE("execute_basic_block leave\n");
    }

    void helper_detach ()
    {
        DBG_PROFILER_TRACE("helper_detach enter\n");
        // *SPAN*
        // Spawn-helper detaches:
        //   push continuation -> c_stack
        //   continuation = 0

        c_stack.push_back(cont);
        cont = new span_t();

        // Spawn-helper detaches:
        //   push continuation -> c_stack
        //   continuation = 0

        w_stack.push_back(work);
        work = 0;
        DBG_PROFILER_TRACE("helper_detach leave\n");
    }

    void spawn_helper_returns (unsigned int cc_index)
    {
        DBG_PROFILER_TRACE("spawn_helper_returns enter - cc_index: %d\n", cc_index);
        // *WORK*
        // Spawn helper returns:
        //   child_work = work
        //   helper_work = pop w_stack
        //   // Update cc-pair profile for child_work + helper_work cost.
        //   // Retain child_work for child_cont in span calculation.
        //   // Retain hepler_work for helper_cont in span calculation.
        //   work += child_work + helper_work

        cilkprof_assert(w_stack.size() > 1);
        work_t child_work = work;
        work_t helper_work = w_stack.back();
        w_stack.pop_back();
        work = w_stack.back();
        w_stack.pop_back();
        if (cc_index != UINT32_MAX) {
            cilkprof_assert(cc_profile.size() > cc_index);
            cc_profile[cc_index] += child_work + helper_work;
        }
        work += child_work + helper_work;

        // *SPAN*
        // Spawn helper returns:
        //   child_cont = continuation
        //   helper_cont = pop c_stack
        //   // Update cc-pair profiles for child_cont and helper_cont.
        //   // Update cc-pair profile for instantaneous span.
        //   continuation = pop c_stack
        //   continuation += helper_cont
        //   if (longest_child < continuation + child_cont) then
        //     longest_child = child_cont
        //     span += continuation
        //     continuation = 0
        //   fi

        span_t *child_cont = cont;
        span_t *helper_cont = pop_c_stack();

        child_cont->cc(cc_index, child_cont->total, child_work);
        helper_cont->cc(cc_index, helper_cont->total, helper_work);
        ispan.cc(cc_index, child_cont->total + helper_cont->total, 0);

        cont = pop_c_stack();
        cont->add_and_clear(helper_cont);

        cilkprof_assert(lchild);
        if (lchild->total < cont->total + child_cont->total) {
            lchild->assign_and_clear(child_cont);
            span->add_and_clear(cont);
        }

        // Clean up after ourselves.
        delete child_cont;
        delete helper_cont;
        DBG_PROFILER_TRACE("spawn_helper_returns leave\n");
    }

    void sync ()
    {
        DBG_PROFILER_TRACE("sync enter\n");
        // Function syncs:
        //   span += max(continuation, longest_child)
        //   continuation = 0
        //   longest_child = 0

        if (cont->total > lchild->total) {
            span->add_and_clear(cont);
            lchild->clear();
        } else {
            span->add_and_clear(lchild);
            cont->clear();
        }
        DBG_PROFILER_TRACE("sync leave\n");
    }

    void enter_cilk_context ()
    {
        DBG_PROFILER_TRACE("enter_cilk_context enter\n");
        // Enter a Cilk context:
        //   push span -> s_stack
        //   push continuation -> c_stack
        //   push longest_child -> l_stack
        //   span = 0
        //   continuation = 0
        //   longest_child = 0

        s_stack.push_back(span);
        c_stack.push_back(cont);
        l_stack.push_back(lchild);
        span = new span_t();
        cont = new span_t();
        lchild = new span_t();
        cilkprof_assert(span && cont && lchild);
        DBG_PROFILER_TRACE("enter_cilk_context leave\n");
    }

    void leave_cilk_context ()
    {
        DBG_PROFILER_TRACE("leave_cilk_context enter\n");
        // Leave Cilk context:
        //   context_cont = continuation + span
        //   span = pop s_stack
        //   continuation = pop c_stack
        //   longest_child = pop l_stack
        //   continuation += context_cont

        span_t *context_cont = cont;
        context_cont->add_and_clear(span);
        delete span;
        delete lchild;
        span = pop_s_stack();
        cont = pop_c_stack();
        lchild = pop_l_stack();
        cont->add_and_clear(context_cont);
        delete context_cont;
        DBG_PROFILER_TRACE("leave_cilk_context leave\n");
    }

    void function_returns (unsigned int cc_index)
    {
        DBG_PROFILER_TRACE("function_returns enter - cc_index: %d, cc_data_table.size: %d\n",
                           cc_index, cc_data_table.size());
        // *SPAN*
        // Function returns:
        //   child_cont = continuation
        //   continuation = pop c_stack
        //   // Update cc-pair profile for child_cont cost.
        //   // Update cc-pair profile for instantaneous span.
        //   continuation += child_cont

        span_t *child_cont = cont;
        cont = pop_c_stack();
        cont->cc(cc_index, child_cont->total, work);
        ispan.cc(cc_index, child_cont->total, 0);
        cont->add_and_clear(child_cont);
        delete child_cont;

        // *WORK*
        // Function returns:
        //   child_work = work
        //   work = pop w_stack
        //   // Update cc-pair profile for child_work cost.
        //   work += child_work

        cilkprof_assert(w_stack.size() > 0);
        work_t child_work = work;
        work = w_stack.back();
        w_stack.pop_back();
        if (cc_index != UINT32_MAX) {
            cilkprof_assert(cc_profile.size() > cc_index);
            cc_profile[cc_index] += child_work;
        }
        work += child_work;
        DBG_PROFILER_TRACE("function_returns leave\n");
    }

    void register_basic_block ()
    {
        bb_profile.push_back(0);
    }

    void register_cc_pair ()
    {
        cc_profile.push_back(0);
    }

    void finalize ()
    {  
        cilkprof_assert(c_stack.size() == 1);
        cilkprof_assert(s_stack.size() == 0);
        cilkprof_assert(l_stack.size() == 0);
        cilkprof_assert(cont);
        cilkprof_assert(NULL == span);
        cilkprof_assert(NULL == lchild);
        span_t *tmp = pop_c_stack();
        cilkprof_assert(NULL == tmp);
        cont->finalize();
        ispan.finalize();
        cilkprof_assert(w_stack.size() == 1);

        // We're going to use the basic block numbers to index into the
        // span table.  There MUST be enough entries after we've finalized
        // the table!
        cilkprof_assert(num_basic_blocks == cont->self.table_size());
    }

    /**                   **/
    /** Accessor methods. **/
    /**                   **/

    span_t *get_span ()
    {
        // The cont _is_ the span of the program, at this point.
        return cont;
    }

    span_t &get_ispan ()
    {
        return ispan;
    }

    work_t get_bb (UINT32 bb_num)
    {
        cilkprof_assert(bb_num < bb_profile.size());
        return bb_profile[bb_num];
    }

    work_t get_cc (unsigned int cc_index)
    {
        cilkprof_assert(cc_index < cc_profile.size());
        return cc_profile[cc_index];
    }

private:

    span_t *pop_stack (std::vector<span_t*> &stack)
    {
        cilkprof_assert(stack.size() > 0);
        span_t *top = stack.back();
        stack.pop_back();
        return top;
    }

    span_t *pop_s_stack ()
    {
        return pop_stack(s_stack);
    }

    span_t *pop_c_stack ()
    {
        return pop_stack(c_stack);
    }

    span_t *pop_l_stack ()
    {
        return pop_stack(l_stack);
    }

    /**                            **/
    /** Span-related data members. **/
    /**                            **/

    // Span stack.
    std::vector<span_t*> s_stack;

    // Continuation stack.
    std::vector<span_t*> c_stack;

    // Longest child stack.
    std::vector<span_t*> l_stack;

    // Span w.r.t. the current Cilk context.
    span_t *span;

    // Continuation w.r.t. the current function or Cilk context.
    span_t *cont;

    // Longest child w.r.t. the current Cilk context.
    span_t *lchild;

    // Instantaneous span.
    // FIXME: Obviously, this should be a vector, and not a span_t.  I don't
    // know how much that'll actually save, but it's the right data structure.
    span_t ispan;

    /**                            **/
    /** Work-related data members. **/
    /**                            **/

    // Total aggregate work performed in the current function.
    work_t work;

    // "Stack" of functions.
    std::vector<work_t> w_stack;

    // Basic block counters.
    std::vector<work_t> bb_profile;

    // Caller/callee pair profile.
    std::vector<work_t> cc_profile;
};

static profiler_t profiler;

// Stack for distinguishing spawning functions from spawn helpers.
enum enter_t { ENTER_SPAWNER, ENTER_HELPER };
static std::stack<enter_t> enter_stack;

/************************************************************************/
/*                         Conversion Functions                         */
/************************************************************************/

/*
 * Take a cc-pair as input and return its index.
 */
static unsigned int cc_pair_to_index (cc_pair_t cc_pair)
{
    // If this cc-pair has not been seen before, add it to the map.
    std::pair<cc_map_t::iterator, bool> ret =
        cc_map.insert(std::pair<cc_pair_t, unsigned int>
                      (cc_pair, num_cc_pairs));
    if (true == ret.second) {
        ++num_cc_pairs;
        cc_data_table.push_back((cc_data_t) { cc_pair, 0 });
        profiler.register_cc_pair();
    }
    return (*ret.first).second;
}

/************************************************************************/
/*                          Callback Functions                          */
/************************************************************************/

// Keep track of the last block executed.  This is used in span calculations
// to note which blocks contributed to burdening.
static unsigned int last_block;

#define NO_SAVED_BB UINT32_MAX
// Track when calls occur.  If a call instruction is reached, this field will
// be populated by the block in which that call exists for the destination to
// find and use.
static UINT32 bb_caller = NO_SAVED_BB;

// Since the metadata is not precisely in line with our purposes, in the case
// of a return-from-spawn, we have to do the line-up, manually:
static bool next_ret_is_return_from_spawn = false;

/*
 * Callback for a trace (basic block).  There were "n" instructions in the
 * basic block identified by "index".
 */
static void trace_callback (UINT32 n, UINT32 index)
{
    static UINT32 saved_plt_bb = NO_SAVED_BB;

    DBG_TRACE_CALLBACK("trace_callback - n: %d, index: %d, saved_plt_bb: %d\n",
                       n, index, saved_plt_bb);

    if (NO_SAVED_BB != bb_caller) {

        // This basic block was the target of a call instruction.
        // bb_caller is the basic block of the caller.  index is the
        // basic block of the callee.

        cc_pair_t cc_pair = (cc_pair_t){ bb_caller, index };
        unsigned int cc_index = cc_pair_to_index(cc_pair);

        shadow_stack.push_back(cc_index);
        ++cc_data_table[cc_index].count;
        bb_caller = NO_SAVED_BB;

        profiler.function_call();

        // A little bookkeeping:
        // .plt is the trampoline section between modules.  If the user
        // ran the program with LD_BIND_NOW=1, everything should be jmp
        // instructions with the proper destinations.  In that case, we
        // can simply wait to find the destination and take its name
        // for ourselves.  If there was lazy binding, a lookup has to
        // be done the first time the function is called through this
        // basic block (the code will be modified at runtime).  In that
        // case, even the following block will be .plt and it's a pain
        // to find the real name.  Just set LD_BIND_NOW=1.  Any driver
        // for this pintool should do that.

        if (address_table[index].is_unresolved_plt) {
            saved_plt_bb = index;
        }

    } else if (NO_SAVED_BB != saved_plt_bb) {
        basic_block_t &plt_bb = address_table[saved_plt_bb];
        basic_block_t &target_bb = address_table[index];

        plt_bb.function_name = RTN_FindNameByAddress(target_bb.address);
        if (plt_bb.function_name == "") plt_bb.function_name = "unknown";
        plt_bb.function_name += "@plt"; // gdb sticks an @plt on the end.
        plt_bb.is_unresolved_plt = false;

        saved_plt_bb = NO_SAVED_BB;
    }

    profiler.execute_basic_block(index, n);

    last_block = index;
}

/*
 * Record that we have seen a call at the specified basic block.  This
 * information is used to shadow the program stack so that costs of
 * subtrees can be computed.
 */
static void see_call_callback (UINT32 index)
{

    DBG_TRACE_CALLBACK("see_call_callback - index: %d\n", index);

    cilkprof_assert(false == next_ret_is_return_from_spawn);
    bb_caller = index;
}

/*
 * Record that we have seen a ret at the specified basic block.  This may be
 * a return-from-spawn.
 */
static void see_ret_callback ()
{
    DBG_TRACE_CALLBACK("see_ret_callback\n");

    cilkprof_assert(shadow_stack.size() > 1);
    unsigned int cc_index = shadow_stack.back();
    shadow_stack.pop_back();
    cilkprof_assert(cc_index < cc_data_table.size());
    cc_data_t &cc_data = cc_data_table[cc_index];
    // Set cc_index to UINT32_MAX if this is not the last of this cc-pair
    // on the stack.  UINT32_MAX indicates not to add the accumulation.
    cilkprof_assert(cc_data.count > 0);
    if (--cc_data.count != 0) cc_index = UINT32_MAX;

    if (next_ret_is_return_from_spawn) {
        // This is a return-from-spawn.  Do the appropriate bookkeeping for
        // leaving a spawn helper function.
        next_ret_is_return_from_spawn = false;
        profiler.spawn_helper_returns(cc_index);
    } else {
        profiler.function_returns(cc_index);
    }
}

/*
 * Detach-end: We use detach to identify the moment when a spawn has
 * properly occurred.
 */
static void detach_callback ()
{
    DBG_TRACE_CALLBACK("detach_callback\n");

    profiler.helper_detach();
}

/*
 * Sync-begin: This indicates the join point between two or more strands.
 * Parameters: not used.
 */
static void sync_callback (ADDRINT cilk_stack_frame)
{
    DBG_TRACE_CALLBACK("sync_callback - cilk_stack_frame: %x\n", cilk_stack_frame);

    profiler.sync();
}

/*
 * Enter-frame-begin: Notification that a spawning context has been entered.
 * Parameters: not used.
 */
static void enter_frame_callback (ADDRINT cilk_stack_frame, ADDRINT ip)
{
    DBG_TRACE_CALLBACK("enter_frame_callback - cilk_stack_frame: %x\n", cilk_stack_frame);

    // Keep track of spawning functions by the address of the cilk_enter_begin
    // annotation
    if (map_function_types.end() == map_function_types.find(ip))
    {
        function_info info;
        info.type = spawning_function;
        info.name = RTN_FindNameByAddress(ip);

//        map_function_types[ip] = spawning_function;
        map_function_types.insert(pair<ADDRINT, function_info>(ip, info));
    }

    enter_stack.push(ENTER_SPAWNER);
    profiler.enter_cilk_context();
}

/*
 * cilk_enter_helper_begin: Notification that a spawn-helper function has
 * been entered.
 * Parameters: not used.
 */
static void enter_helper_callback (ADDRINT cilk_stack_frame, ADDRINT ip)
{
    DBG_TRACE_CALLBACK("enter_helper_callback - cilk_stack_frame: %x\n", cilk_stack_frame);

    // Keep track of spawn helper functions by the address of the
    // cilk_enter_helper_begin annotation
    {
        function_info info;
        info.type = spawn_helper_function;
        info.name = RTN_FindNameByAddress(ip);

        map_function_types.insert(pair<ADDRINT, function_info>(ip, info));
    }

    cilkprof_assert(enter_stack.size() > 0);
    enter_stack.push(ENTER_HELPER);
}

/*
 * Leave-frame-end: Leaving a spawn-helper function indicates the beginning
 * of a continuation, in principle.  In practice, there will be a few more
 * instructions in the spawn-helper, before the parent's continuation truly
 * resumes.
 */
static void leave_callback ()
{
    DBG_TRACE_CALLBACK("leave_callback\n");


    // Determine whether this is a "return-from-spawn" or just a return from
    // a spawning function.

    cilkprof_assert(enter_stack.size() > 0);
    enter_t e = enter_stack.top();
    enter_stack.pop();

    if (e == ENTER_HELPER) {
        // This is a return-from-spawn leave-frame callback.  We delay the
        // return-from-spawn calculation until the ret instruction.
        // Otherwise, we would end up with the residual instructions in this
        // function being attributed to the continuation of the parent
        // function, which would be silly.

        cilkprof_assert(enter_stack.size() > 0);
        next_ret_is_return_from_spawn = true;
    } else {

        // This is leaving a spawning context.
        cilkprof_assert(e == ENTER_SPAWNER);
        profiler.leave_cilk_context();
    }
}

/************************************************************************/
/*                              Metacalls                               */
/************************************************************************/

/*
 * Process a metacall.  In this case, we are only concerned about the call
 * that the runtime makes to ask whether it is running under a sequential
 * pintool.
 * Parameters: address of the metacall_data_t
 */
static void metacall (ADDRINT data)
{
    struct metacall_data_t
    {
        unsigned int tool;   // Intended tool.
        unsigned int code;   // How to interpret data.
        void *data;
    };

    metacall_data_t *metacall_data = (metacall_data_t*)data;

    if (0 == metacall_data->tool && 2 == metacall_data->code) {
        // metacall_data->tool == 0 means this is a "system" metacall.
        // metacall_data->code == 2 means the runtime wants to know whether
        //   to run sequentially.  *metacall_data->data is expected to be
        //   assigned to zero if the answer is yes.
        *(char*)metacall_data->data = 0;
    }
}

/************************************************************************/
/*                       Instrumentation Functions                      */
/************************************************************************/

unsigned int known_bblock_index(BBL bbl)
{
    map_address_2_bblock_t::iterator lower, upper;
    ADDRINT address = BBL_Address(bbl);
    unsigned int insn_count = BBL_NumIns(bbl);

    // Search the multimap for a basic block with the same address
    // and number of instructions
    lower = address_2_bblock_map.lower_bound(address);
    upper = address_2_bblock_map.upper_bound(address);

    for (map_address_2_bblock_t::iterator i = lower; i != upper; i++)
    {
        unsigned int bbl_num = i->second;
        if (insn_count == address_table.at(bbl_num).insn_count)
        {
            duplicate_basic_blocks++;
            return bbl_num;
        }
    }

    // We didn't find a match
    return UINT_MAX;
}

/*
 * Trace = "Single entrance, multiple exit sequence of instructions".  We use
 * traces as basic blocks, and instrument_trace registers our callbacks.
 */
static void instrument_trace (TRACE trace, void *)
{
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl))
    {
        //ADDRINT address = BBL_Address(bbl);

        // See if we already know this basic block.  It's considered a match
        // if both the starting address and instruction count match
        unsigned int bbl_num = known_bblock_index(bbl);

        // If we don't already know this basis block, add it to our table(s)
        if (UINT_MAX == bbl_num)
        {
            // Pushing the basic block onto the vector will increment the size
            // by 1.  The vector index for the block in the vector is 0-based
            // and is the size of the vector before we do the push
            bbl_num = address_table.size();
            address_table.push_back(basic_block_t(bbl));
            profiler.register_basic_block();
            ++num_basic_blocks;

            // Add an entry for this basic block to the map from addresses to
            // basic block indicies
            address_2_bblock_map.
                insert(pair<ADDRINT, unsigned int>(BBL_Address(bbl), bbl_num));
        }

        BBL_InsertCall(bbl, IPOINT_BEFORE, (AFUNPTR)trace_callback,
                       IARG_UINT32, BBL_NumIns(bbl),
                       IARG_UINT32, bbl_num,
                       IARG_END);

        // In addition to caring only about the basic blocks as a whole,
        // we will also track calls and returns to shadow the program
        // stack.  We use this data to collect costs that children
        // contribute to their parents.
        INS ins = BBL_InsTail(bbl);
        if (INS_IsProcedureCall(ins)) {
            // This call may already be instrumented in a different basic
            // block.  If so, we still want to register the callback, but
            // the callback should always get the same ID for the same
            // caller.  Thus: caller_map.
            static std::map<ADDRINT, UINT32> caller_map;
            ADDRINT addr = INS_Address(ins);
            std::pair<std::map<ADDRINT, UINT32>::iterator, bool> ret =
                caller_map.insert(std::pair<ADDRINT, UINT32>
                                  (addr, bbl_num));
            INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)see_call_callback,
                           IARG_UINT32, (*ret.first).second,
                           IARG_END);
        } else if (INS_IsRet(ins)) {
            INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)see_ret_callback,
                           IARG_END);
        }
    }
}

static const char *split_path(const string &path)
{
    const char *path_c_str = path.c_str();

    // Look for both Windows & Unix directory delimiters
    size_t delimiter = path.find_last_of("/\\");
    if (path.npos == delimiter)
        return path_c_str;
    else
        return path_c_str + delimiter + 1;
}

static void dump_address_table()
{
    FILE *f = fopen("address_table.csv", "w");

    fprintf(f, "Index,Address,Instructions,Called,Burden,UnresolvedPLT,File,Line,Function\n");
    for (unsigned int i = 0; i < num_basic_blocks; ++i)
    {
        basic_block_t &bb = address_table[i];
        fprintf(f, "%d,\"%llx\",%d,%llu,%d,%d,\"%s\",%d,\"%s\"\n",
                i,
                (long long unsigned)bb.address, 
                bb.insn_count,
                profiler.get_bb(i),
                bb.burden,
                bb.is_unresolved_plt,
                bb.filename.c_str(),
                bb.line,
                bb.function_name.c_str());
    }
    fclose(f);
}

void build_function_map()
{
    // Build map of spawing and spawn helper functions so we can
    // display the function types
    for (function_type_map::iterator j = map_function_types.begin();
         j != map_function_types.end();
         ++j)
    {
        map_function_names_to_types[j->second.name] = j->second.type;
    }
}

const char *get_function_type(const std::string &name)
{
    function_names_to_types_map::iterator i =
        map_function_names_to_types.find(name);

    // We only have spawn helpers and spawning functions in our map.  If we
    // didn't find it, then it's a plain old serial function
    if (map_function_names_to_types.end() == i)
        return "Serial";

    // Translate the type enum to a string
    switch (i->second)
    {
        case spawning_function:
            return "Spawning";
        case spawn_helper_function:
            return "Helper";
    }

    // Should never get here...
    return "Unknown";
}

/*
 * Output a CSV file.
 */
static void output_csv ()
{
    FILE *bb_out = get_bb_outfile();
    FILE *cc_out = get_cc_outfile();
    span_t *span = profiler.get_span();
    span_t &ispan = profiler.get_ispan();

    sprof_t<ss_t> *self = &span->self;
    sprof_t<sp_t> *agg = &span->accum;
    sprof_t<sp_t> *sr_work = &span->work;

    // Associate basic blocks with functions.
    std::map<std::string, std::vector<unsigned int> > fn_map;
    for (unsigned int i = 0; i < num_basic_blocks; ++i)
    {
        std::string fname = address_table[i].function_name;
        fn_map[fname].push_back(i);
    }

    // Associate called functions with their respective callers.
    std::map<unsigned int, std::vector<unsigned int> > call_map;
    for (cc_map_t::iterator iter = cc_map.begin();
         cc_map.end() != iter;
         ++iter)
    {
        cc_pair_t cc_pair = (*iter).first;
        call_map[cc_pair.caller].push_back(cc_pair.callee);
    }

    // Output CSVs.
    std::fprintf(bb_out,
                 "\"Basic Block\",\"Function Name\","
                 "\"Undecorated Function Name\",\"Function Type\","
                 "\"Work\",\"Span\",\"File:Line #\",\"File Path\"\n");
    std::fprintf(cc_out,
                 "\"Call Site\","
                 "\"Caller\",\"Undecorated Caller\",\"Caller Type\","
                 "\"Callee\",\"Undecorated Callee\",\"Callee Type\","
                 "\"Work on Work\",\"Span on Work\","
                 "\"Work on Span\",\"Span on Span\","
                 "\"File:Line #\",\"File Path\","
                 "\"Calling BB\",\"Called BB\""
                 "\n");

    for (std::map<std::string, std::vector<unsigned int> >::iterator iter =
             fn_map.begin();
         iter != fn_map.end();
         ++iter)
    {
        const std::string &fname = (*iter).first;
        std::string undfname = PIN_UndecorateSymbolName(fname,
                                                        UNDECORATION_NAME_ONLY);

        
        std::vector<unsigned int> &fn = (*iter).second;
        for (unsigned int i = 0; i < fn.size(); ++i) {
            unsigned int bb_num = fn[i];
            const basic_block_t &bb = address_table[bb_num];
            const char *bb_file_path = bb.filename.c_str();
            const char *bb_file_name = split_path(bb.filename);

            // bb_num, fname, undecorated fname, ftype, work, span,
            // file:line_num, path
            std::fprintf(bb_out, "%u,%s,\"%s\",%s,%llu,%llu,\"%s:%d\",\"%s\"\n",
                         bb_num,
                         fname.c_str(),
                         undfname.c_str(),
                         get_function_type(fname),
                         profiler.get_bb(bb_num) * bb.insn_count,
                         (work_t)self->get(bb_num) * (work_t)bb.insn_count,
                         bb_file_name,
                         bb.line,
                         bb_file_path);

            std::map<unsigned int, std::vector<unsigned int> >::iterator call =
                call_map.find(bb_num);
            if (call_map.end() != call) {
                const std::vector<unsigned int> &callee_list = (*call).second;
                for (unsigned int j = 0; j < callee_list.size(); ++j) {
                    unsigned int callee = callee_list[j];
                    const basic_block_t &callee_bb = address_table[callee];
                    cc_pair_t cc_pair = (cc_pair_t){ bb_num, callee };
                    unsigned int cc_index = cc_map[cc_pair];
                    const cc_data_t &cc_data = cc_data_table[cc_index];

                    std::fprintf(cc_out,
                                 "%u,"
                                 "%s,\"%s\",%s,"
                                 "%s,\"%s\",%s,"
                                 "%llu,%llu,%llu,%llu,\"%s:%d\",\"%s\","
                                 "%u,%u\n",
                                 cc_index,
                                 fname.c_str(),
                                 undfname.c_str(),
                                 get_function_type(fname),
                                 callee_bb.function_name.c_str(),
                                 PIN_UndecorateSymbolName(callee_bb.function_name,
                                                          UNDECORATION_NAME_ONLY).c_str(),
                                 get_function_type(callee_bb.function_name),
                                 profiler.get_cc(cc_index),
                                 (work_t)ispan.accum.get(cc_index),
                                 (work_t)sr_work->get(cc_index),
                                 (work_t)agg->get(cc_index),
                                 bb_file_name,
                                 bb.line,
                                 bb_file_path,
                                 bb_num,
                                 callee);
                }
            }
        }
    }

    if (bb_out != stdout && bb_out != stderr) {
        std::fclose(bb_out);
    }

    if (cc_out != stdout && cc_out != stderr) {
        std::fclose(cc_out);
    }
}

void dump_function_type_map()
{
    string name;

    fprintf(stderr, "Function type map:\n");
    for (function_type_map::iterator i = map_function_types.begin();
         i != map_function_types.end();
         ++i)
    {
        fprintf (stderr, "%p: ", (void *)i->first);
        switch (i->second.type)
        {
            case serial_function:
                fprintf(stderr, "serial_function");
                break;
            case spawning_function:
                fprintf(stderr, "spawning_function");
                break;
            case spawn_helper_function:
                fprintf(stderr, "spawn_helper_function");
                break;
            default:
                fprintf(stderr, "Unknown function");
        }
        fprintf(stderr, " - %s\n", i->second.name.c_str());
    }
}

/*
 * Pin calls this function when the program terminates.  Do all the post-
 * processing and dump the profile.
 */
static void instrument_fini (INT32 code, void *)
{
    // The program doesn't exit with a stack depth of one, so we'll
    // simulate returns until our stack is down to where it was when
    // the program began.
    while (shadow_stack.size() > 1)
    {
        see_ret_callback();
    }

    // Finialize our data - condense all lists into the tables
    profiler.finalize();

    // Build the map of function names to type
    build_function_map();

    // Write the caller/callee and basic block CSVs
    output_csv();
}

/*
 * PIN calls this function to notify us of loaded images.
 */
static void image_load (IMG img, void *context)
{
    // We're only looking for the main executable of the application
    if (! IMG_IsMainExecutable(img))
        return;

    // Save the name of the main executable
    save_executable_file_name(IMG_Name(img).c_str());
}

/************************************************************************/
/*                                 Main                                 */
/************************************************************************/

// Table of metadata and callbacks based on zero-cost annotations (ZCA).
static ZCA::zca_table zca;

int main (int argc, char *argv[])
{
    PIN_InitSymbols();
    if (PIN_Init(argc, argv)) {
        die("cannot parse arguments.\n");
    }

    zca.initialize();

    // Register instrumentation functions with Pin.
    TRACE_AddInstrumentFunction(instrument_trace, 0);
    PIN_AddFiniFunction(instrument_fini, 0);
    IMG_AddInstrumentFunction(image_load, 0);

    // Register callback functions for when certain Cilk-ish things happen.
    zca.insert_annotation_calls("cilk_detach_end",
                                (AFUNPTR)detach_callback,
                                IARG_END);
    zca.insert_annotation_calls("cilk_sync_begin",
                                (AFUNPTR)sync_callback,
                                IARG_END);
    zca.insert_annotation_calls("cilk_enter_begin",
                                (AFUNPTR)enter_frame_callback,
                                IARG_INST_PTR,
                                IARG_END);
    zca.insert_annotation_calls("cilk_enter_helper_begin",
                                (AFUNPTR)enter_helper_callback,
                                IARG_INST_PTR,
                                IARG_END);
    zca.insert_annotation_calls("cilk_leave_end",
                                leave_callback,
                                IARG_END);

    // Register a callback for metacalls (calls from the user program
    // into the pintool).
    zca.insert_annotation_calls("cilkscreen_metacall",
                                (AFUNPTR)metacall,
                                IARG_END);

    // Start the shadow stack.
    cc_pair_t initial_pair = (cc_pair_t){ UINT32_MAX, 0 };
    cc_map[initial_pair] = 0;
    cc_data_table.push_back((cc_data_t){ initial_pair, 1 });
    profiler.register_cc_pair();
    shadow_stack.push_back(num_cc_pairs++);
    profiler.function_call();

    PIN_StartProgram();
    return 0;
}
