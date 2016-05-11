
#include <iostream>
#include <regex>
#include <list>

#include <unistd.h>
#include <sys/param.h> // MAXPATHLEN
#include <assert.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

#include "analysis.hpp"
#include "statistics.hpp"
#include "utils.hpp"
#include "counter.hpp"
#include "proc.hpp"
#include "range.hpp"

namespace statistics {

  using namespace std;
  using namespace disassembly;
  using namespace analysis;
  using namespace defs;
  using namespace utils;
  using namespace proc;
  using namespace range;

  typedef vector<MappedRegion> MappedRegions;
  MappedRegions* mem_regions = NULL;

  /* Constants */
  const uint8_t probe_able_size = 5;
  const uint32_t giga_bytes = 1 << 30;
  const uint64_t stack_allowance = 2 * giga_bytes;
  const uint8_t invalid_opcodes [] = {0x06, 0x07, 0x0E, 0x16, 0x17, 0x1E, 0x1F, 
    0x27, 0x2F, 0x37, 0x3F, 0x60, 0x61, 0x62};

  /* Global data */
  Counter* single_byte_op_sequences = NULL;
  Range text = Range();
  Range heap = Range();
  Range stack = Range();
  Range stack_reserved = Range();

  Address lower_limit;

  /* Private helper functions */

  void initializeRanges() {

    if (mem_regions == NULL) {
      readMappedRegions();
    }

    bool first_region = true;
    Address text_start = NULL;
    Address prev_end = NULL;
    for (auto r : *mem_regions) {
      if (first_region) {
        text_start = r.start;
        first_region = false;
      }

      if (string("[heap]").compare(r.file)) {
        text = Range(text_start, prev_end);
        heap = Range(r.start, r.end);
      }

      if (string("[stack]").compare(r.file)) {
        stack = Range(r.start, r.end);
        stack_reserved = Range(r.start - stack_allowance, r.end);
      }
    }

    assert(text.start != NULL);
    assert(heap.start != NULL);
    assert(stack.start != NULL);
  }

  void handle_error(const char* msg) {
    perror(msg);
  }

  void readMemoryMappingInfo() {

    mem_regions = readMappedRegions();
  }

  int32_t swap_int32(int32_t val) {
    val = ((val << 8) & 0xFF00FF00) | ((val >> 8) & 0xFF00FF ); 
            return (val << 16) | ((val >> 16) & 0xFFFF);
  }

  inline bool isSafeTrampolineAvailable(uint64_t index, 
         uint64_t n_instructions, _DInst* decoded, Address ip) {
    if (index >= n_instructions - 1) {
      return false;
    }

    int8_t probe_size = probe_able_size;
    uint8_t ins_boundaries[5] = {0};
    uint64_t i = index;
    uint8_t clobbered_instruction_count = 0;
    uint8_t current_boundary = 0;
    while (probe_size > 0 && i < n_instructions) {
      probe_size -= decoded[i].size;
      ins_boundaries[i - index] = current_boundary + decoded[i].size - 1;
      current_boundary += decoded[i].size;
      i++; clobbered_instruction_count++;
    }

    // Not enough space before end of the block for a probe
    if (i == n_instructions) {
      return false;
    }

    int32_t rel_addr = 0x0;
    int32_t int3_mask = 0xCC;
    for (int i=0; i < clobbered_instruction_count - 1; i++) { 
      rel_addr = rel_addr | 
      int3_mask << (ins_boundaries[i] * 8);
    }

    // rel_addr = swap_int32(rel_addr);

    if (((uint8_t*)&rel_addr)[3] == 0xCC) {
      // fprintf(stderr, "\n[swap] A : %p\n", rel_addr);
      // fprintf(stderr, "[swap]Swapped MSB..\n");
      ((uint8_t*)&rel_addr)[3] = 0x62;
      // fprintf(stderr, "[swap] B : %p\n\n", rel_addr);
    }


    Address target = ip + decoded[index].size + rel_addr;

    if ((int64_t) target < 0) {
      fprintf(stderr, "[alloc] Negative target address recieved : %p\n",
          target);
    }

    // fprintf(stderr, "[alloc] Address : %p Relative address : %p\n", ip, 
    //     rel_addr);

    long page_size = sysconf(_SC_PAGESIZE); 
    long alloc_size = page_size;
    Address page_start = target - (uint64_t) target % page_size; // align it to a page boundary
    // If the stub is located at the end of the page allocate the next page too
    if ((page_size - (uint64_t) target % page_size) < 72) {
      fprintf(stderr, "[alloc] NEED TO GET THE NEXT PAGE AS WELL..\n");
      alloc_size = page_size * 2;
    }

    /*
    for (auto r : *mem_regions) {
      if (target >= r.start && target <= r.end) {
        fprintf(stderr, "[alloc] ERROR : THE ADDRESS IS WITHIN A MAPPED " 
            "REGION..\n");
      } 

      return false;
    }
    */

    fprintf(stderr, "[mmap] Trying to allocate a stub at : %p\n", target);
    fprintf(stderr, "[mmap] Also Trying to allocate the next page at : %p\n", 
        page_start + page_size);
    Address stub_address = (Address)mmap(target, 90,
        PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1,0);
    Address next_page = page_start + page_size;
    Address page = (Address)mmap(next_page, 90,
        PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1,0);

    if (page == MAP_FAILED) {
      handle_error(string("mmap").c_str());
      fprintf(stderr, "[mmap] Failed allocating page at : %p with a relative " 
          "address of : %p\n\n", next_page, rel_addr);
      return false;
    } else {
      if (page != next_page) {
        fprintf(stderr, "[mmap] Got different stub address target: %p " 
            " requested: %p recieved : %p\n", next_page, next_page, page);
        return false;
      }
    }

    fprintf(stderr, "[mmap] GOT NEXXT PAGE AT : %p - %p\n\n", page, 
        page + 90);

    if (stub_address == MAP_FAILED) {
      handle_error(string("mmap").c_str());
      fprintf(stderr, "[mmap] Failed allocating stub at : %p with a relative " 
          "address of : %p\n\n", target, rel_addr);
      return false;
    } else {
      if (stub_address != target) {
        fprintf(stderr, "[mmap] Got different stub address target: %p " 
            " requested: %p recieved : %p\n", target, target, stub_address);
        return false;
      }
    }


    fprintf(stderr, "[mmap] GOT MEMORY RANGE AT : %p - %p\n\n", stub_address, 
        stub_address + 90);

    return true;
  }

  inline bool isProbeAble(_DInst i) {
    return (i.size >= probe_able_size);
  }

  /* Public API implementation */
  Function generateMetaDataForFunction(BlockStructure bs, Decoded d) {

    if (single_byte_op_sequences == NULL) {
      single_byte_op_sequences = new Counter();
    }

    _DInst* decoded = d.decoded_instructions;

    Function fn;
    fn.start = bs.fn_start;
    fn.end = bs.fn_end;
    fn.end_padding_size = bs.end_padding_size;
    fn.bbl = bs.bbl;
    fn.returns = bs.returns;

    ProbeMetaData fn_pmd;
    fn_pmd.total_instruction_count = 0;
    fn_pmd.probe_able_count= 0;
    fn_pmd.pii_count = 0;
    fn_pmd.start_instruction_size = 0;
    fn_pmd.start_probe_able_inst_distance = 0;
    fn_pmd.end_instruction_size = 0;
    fn_pmd.end_probe_able_inst_distance = 0;
    fn_pmd.safe_patchable_start = false;
    fn_pmd.safe_patchable_end = false;
    fn_pmd.punnable_start = false;

    bool fn_start_block = true;
    int64_t last_probe_able_inst_distance = 0;
    uint32_t last_instruction_size = 0;

    bool probe_able_ins_found = false;
    ProbeMetaData bb_pmd;
    Address last_probe_able_inst_ip = (Address) NULL;

    // assert(bs.bbl.size() > 0);

    for (BasicBlock* bb : bs.bbl) {
      probe_able_ins_found = false;
      bb_pmd.total_instruction_count = 0;
      bb_pmd.probe_able_count= 0;
      bb_pmd.pii_count = 0;
      bb_pmd.start_instruction_size = 0;
      bb_pmd.start_probe_able_inst_distance = 0;
      bb_pmd.end_instruction_size = 0;
      bb_pmd.end_probe_able_inst_distance = 0;
      bb_pmd.safe_patchable_start = false;
      bb_pmd.safe_patchable_end = false;
      bb_pmd.punnable_start = false;

      int64_t offset = 0;

      for (uint64_t i = bb->start_ins_offset; i <= bb->end_ins_offset; i++) {
        bb_pmd.total_instruction_count++;
        fn_pmd.total_instruction_count++;

        if (decoded[i].size == 1 && i < (d.n_instructions-1)) {
          single_byte_op_sequences->increment("1|" + to_string(decoded[i+1].size), 1);
        }

        if (isProbeAble(decoded[i])) {
          bb_pmd.probe_able_count++;
          fn_pmd.probe_able_count++;

          if (!probe_able_ins_found) {
            bb_pmd.start_probe_able_inst_distance = offset;

            if (fn_start_block) {
              fn_pmd.start_probe_able_inst_distance = offset;
            }
            probe_able_ins_found = true;
          }

          last_probe_able_inst_distance = offset;
          last_probe_able_inst_ip = bb->start + offset;
        }

        /*
           if (isPositionIndependent(decoded[i])) {
           bb_pmd.pii_count++;
           fn_pmd.pii_count++;
           }
           */

        if (i == bb->start_ins_offset) {
          bb_pmd.start_instruction_size = decoded[i].size;

          if (!isProbeAble(decoded[i])) {
            Address ip = bb->start + offset;
            if (isSafeTrampolineAvailable(i, d.n_instructions, decoded, ip)) {
              bb_pmd.punnable_start = true;
              bb_pmd.safe_patchable_start = true;
            } 
          } else {
            bb_pmd.safe_patchable_start = true;
          }

          if (fn_start_block) {
            fn_pmd.start_instruction_size = decoded[i].size;

            if (!isProbeAble(decoded[i])) {
              Address ip = bb->start + offset;
              if (isSafeTrampolineAvailable(i, d.n_instructions, decoded, ip)) {
                fn_pmd.punnable_start = true;
                fn_pmd.safe_patchable_start = true;
              } 
            } else {
              fn_pmd.safe_patchable_start = true;
            }
          }
        }

        if (i == bb->end_ins_offset) {
          bb_pmd.end_instruction_size = decoded[i].size;
          last_instruction_size = decoded[i].size;

          if (isProbeAble(decoded[i])) {
            bb_pmd.safe_patchable_end = true;
          }
        }

        offset += decoded[i].size;
      }

      if (last_probe_able_inst_ip >= bb->start) {
        bb_pmd.end_probe_able_inst_distance =  (int64_t)(bb->end - bb->start) - 
          last_probe_able_inst_distance;
      } else {
        bb_pmd.end_probe_able_inst_distance = -1;
      }

      /*
      if (bb_pmd.end_probe_able_inst_distance > 1000000) {
        fprintf(stderr, "[Block] End : %p  Start : %p Offset : %ld\n", 
            bb->end, bb->start, last_probe_able_inst_distance);
      }*/

      if (fn_start_block) {
        fn_start_block = false;
      }

      bb->pmd = bb_pmd;
    }

    fn_pmd.end_instruction_size = last_instruction_size;
    if (last_instruction_size >= probe_able_size) {
      fn_pmd.safe_patchable_end = true;
    }
    
    if (last_probe_able_inst_ip != NULL) {
      fn_pmd.end_probe_able_inst_distance = (int64_t)fn.end - 
        (int64_t)last_probe_able_inst_ip;
    } else {
      fn_pmd.end_probe_able_inst_distance = -1;
    }

    if (fn_pmd.start_instruction_size == 0) {
      assert(false);
    }

    /*
    if (fn_pmd.end_probe_able_inst_distance > 100000) {
      fprintf(stderr, "End : %p  Start : %p Offset : %ld\n", 
          fn.end, fn.start, last_probe_able_inst_ip);
    }*/

    fn.pmd = fn_pmd;

    // fn_pmd.show(stderr, 2);
    // fn.pmd.show(stderr, 0);

    return fn;
  }

  Statistics generateStatistics(list<Function>* fns) {
    Statistics stats;
    stats.per_func_probe_able_count_hist = new Histogram(0, 5, true);
    stats.per_func_pii_count_hist = new Histogram(0, 10, true);
    stats.per_func_start_inst_size_hist = new Histogram(0, 15, 1, true);
    stats.per_func_end_inst_size_hist = new Histogram(0, 15 , 1, true);
    stats.per_func_start_probe_able_inst_distance_hist = new Histogram(0, 5, true);
    stats.per_func_end_probe_able_inst_distance_hist = new Histogram(0, 5, true);

    stats.per_bb_probe_able_count_hist = new Histogram(0, 5, true);
    stats.per_bb_pii_count_hist = new Histogram(0, 5, true);
    stats.per_bb_start_inst_size_hist = new Histogram(0, 15, 1, true);
    stats.per_bb_end_inst_size_hist = new Histogram(0, 15, 1, true);
    stats.per_bb_start_probe_able_inst_distance_hist = new Histogram(0, 5, true);
    stats.per_bb_end_probe_able_inst_distance_hist = new Histogram(0, 5, true);

    uint64_t total_instruction_count = 0;
    uint64_t total_probe_able_count = 0;
    uint64_t total_pii_count = 0;

    uint64_t probe_ready_func_starts = 0;
    uint64_t probe_ready_func_ends = 0;
    uint64_t probe_ready_bb_starts= 0;
    uint64_t probe_ready_bb_ends = 0;

    stats.total_funcs = fns->size();
    stats.total_basic_blocks = 0; 
    stats.punning_ready_func_starts = 0;
    stats.punning_ready_bb_starts = 0;

    for (Function fn: *fns) {
      if (fn.pmd.safe_patchable_start) {
        probe_ready_func_starts++;
      }

      if (fn.pmd.punnable_start) {
        stats.punning_ready_func_starts++;
      }

      if (fn.pmd.safe_patchable_end) {
        probe_ready_func_ends++;
      }

      total_instruction_count += fn.pmd.total_instruction_count;
      total_probe_able_count += fn.pmd.probe_able_count;
      total_pii_count += fn.pmd.pii_count;

      stats.per_func_probe_able_count_hist->addItem(fn.pmd.probe_able_count);
      stats.per_func_pii_count_hist->addItem(fn.pmd.pii_count);
      stats.per_func_start_inst_size_hist->
        addItem(fn.pmd.start_instruction_size);
      stats.per_func_end_inst_size_hist->
        addItem(fn.pmd.end_instruction_size);
      if (fn.pmd.start_probe_able_inst_distance >= 0) {
        stats.per_func_start_probe_able_inst_distance_hist->
          addItem(fn.pmd.start_probe_able_inst_distance);
      }

      if (fn.pmd.end_probe_able_inst_distance >=0) {
        stats.per_func_end_probe_able_inst_distance_hist->
          addItem(fn.pmd.end_probe_able_inst_distance);
      }

      stats.total_basic_blocks += fn.bbl.size();
      for (BasicBlock* bb : fn.bbl) {
        if (bb->pmd.safe_patchable_start) {
          probe_ready_bb_starts++;
        }

        if (bb->pmd.punnable_start) {
          stats.punning_ready_bb_starts++;
        }

        if (bb->pmd.safe_patchable_end) {
          probe_ready_bb_ends++;
        }

        stats.per_bb_probe_able_count_hist->addItem(bb->pmd.probe_able_count);
        stats.per_bb_pii_count_hist->addItem(bb->pmd.pii_count);
        stats.per_bb_start_inst_size_hist->
          addItem(bb->pmd.start_instruction_size);
        stats.per_bb_end_inst_size_hist->
          addItem(bb->pmd.end_instruction_size);
        if (bb->pmd.start_probe_able_inst_distance >=0) {
          stats.per_bb_start_probe_able_inst_distance_hist->
            addItem(bb->pmd.start_probe_able_inst_distance);
        }

        if (bb->pmd.end_probe_able_inst_distance >=0) {
          stats.per_bb_end_probe_able_inst_distance_hist->
            addItem(bb->pmd.end_probe_able_inst_distance);
        }
      }

    }

    stats.probe_ready_func_starts = probe_ready_func_starts;
    stats.probe_ready_func_ends = probe_ready_func_ends;
    stats.probe_ready_bb_starts = probe_ready_bb_starts;
    stats.probe_ready_bb_ends = probe_ready_bb_ends;

    stats.total_instruction_count = total_instruction_count;
    stats.total_probe_able_count = total_probe_able_count;
    stats.total_pii_count = total_pii_count;

    return stats;
  }

  void printStatistics(Statistics stats, FILE* fp, bool verbose) {

    fprintf(fp, "============= Probe Audit Report ===========\n\n");
    fprintf(fp, "Executable : %s\n", getProgramPath());
    fprintf(fp, "Instructions : %lu\n", stats.total_instruction_count); 
    fprintf(fp, "Total number of functions : %lu\n", stats.total_funcs);
    fprintf(fp, "Total number of basic blocks : %lu\n", stats.total_basic_blocks);
    fprintf(fp, "Number of probe ready function prologs (with punning) :"
        " %lu/%lu\n", stats.punning_ready_func_starts, stats.total_funcs);
    fprintf(fp, "Number of probe ready function prologs (without relocation) :"
        " %lu/%lu\n", stats.probe_ready_func_starts, stats.total_funcs);
    fprintf(fp, "Number of probe ready function epilogs (without relocation) :"
        " %lu\n", stats.probe_ready_func_ends);
    fprintf(fp, "Number of probe ready basic block prologs (with punning) :"
        " %lu/%lu\n", stats.punning_ready_bb_starts, stats.total_basic_blocks);
    fprintf(fp, "Number of probe ready basic block prologs (without relocation)" 
        " : %lu\n", stats.probe_ready_bb_starts);
    fprintf(fp, "Number of probe ready basic block epilogs (without relocation)" 
        " : %lu\n", stats.probe_ready_bb_ends);
    fprintf(fp, "Probe able instructions (%%) : %lu (%f)\n", 
        stats.total_probe_able_count, 
        (double) stats.total_probe_able_count / stats.total_instruction_count);
    fprintf(fp, "Position independent instructions (%%) : %lu (%f)\n", 
        stats.total_pii_count, 
        (double) stats.total_pii_count / stats.total_instruction_count);
    fprintf(fp, "Single byte instruction configurations : \n");
    single_byte_op_sequences->show(fp, 0);

    fprintf(fp, "\n");

    if (verbose) {
      fprintf(fp, "\n");
      fprintf(fp, "Histograms\n\n");
      fprintf(fp, "------------------\n");
      fprintf(fp, "| Function Level |\n");
      fprintf(fp, "------------------\n\n");
      fprintf(fp, "-------- Probe able instructions per function \n");
      // printHistogram(stats.per_func_probe_able_count_hist, fp);
      stats.per_func_probe_able_count_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- Position independent instructions per function \n");
      // printHistogram(stats.per_func_pii_count_hist, fp);
      stats.per_func_pii_count_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- Start instruction size of functions \n");
      // printHistogram(stats.per_func_start_inst_size_hist, fp);
      stats.per_func_start_inst_size_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- End instruction size of functions \n");
      // printHistogram(stats.per_func_end_inst_size_hist, fp);
      stats.per_func_end_inst_size_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- First probe able instruction distance from start for " 
          "functions \n");
      // printHistogram(stats.per_func_start_probe_able_inst_distance_hist, fp);
      stats.per_func_start_probe_able_inst_distance_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- First probe able instruction distance from end for " 
          "functions \n");
      // printHistogram(stats.per_func_end_probe_able_inst_distance_hist, fp);
      stats.per_func_end_probe_able_inst_distance_hist->show(fp, 0);

      fprintf(fp, "\n\n");

      fprintf(fp, "--------------------\n");
      fprintf(fp, "| Basic Block Level |\n");
      fprintf(fp, "--------------------\n\n");
      fprintf(fp, "-------- Probe able instructions per block \n");
      // printHistogram(stats.per_bb_probe_able_count_hist, fp);
      stats.per_bb_probe_able_count_hist->show(fp, 0);

      fprintf(fp, "\n");
      // fprintf(fp, "-------- Position independent instructions per block \n");
      // printHistogram(stats.per_bb_pii_count_hist, fp);
      // stats.per_bb_pii_count_hist->show(fp, 0); 

      fprintf(fp, "\n");
      fprintf(fp, "-------- Start instruction size of blocks \n");
      // printHistogram(stats.per_bb_start_inst_size_hist, fp);
      stats.per_bb_start_inst_size_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- End instruction size of blocks \n");
      // printHistogram(stats.per_bb_end_inst_size_hist, fp);
      stats.per_bb_end_inst_size_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- First probe able instruction distance from start for " 
          "blocks\n");
      // printHistogram(stats.per_bb_start_probe_able_inst_distance_hist, fp);
      stats.per_bb_start_probe_able_inst_distance_hist->show(fp, 0);

      fprintf(fp, "\n");
      fprintf(fp, "-------- First probe able instruction distance from end for " 
          "blocks\n");
      // printHistogram(stats.per_bb_end_probe_able_inst_distance_hist, fp);
      stats.per_bb_end_probe_able_inst_distance_hist->show(fp, 0);

    }

    fprintf(fp, "\n");
    fprintf(fp, "==================== END REPORT ====================\n\n");

    fflush(fp);

  }

  void printBlockInfo(list<Function> fns, FILE* fp) {
    fprintf(fp, "================== Block Information ================\n");
    for (Function fn : fns) {
      fn.show(fp, 0);
    }
    fprintf(fp, "================= END Block Information =================\n\n");
    fflush(fp);
  }

}
