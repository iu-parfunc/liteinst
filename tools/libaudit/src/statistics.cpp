
#include <iostream>
#include <list>

#include <unistd.h>
#include <sys/param.h> // MAXPATHLEN
#include <assert.h>

#include "analysis.hpp"
#include "statistics.hpp"
#include "utils.hpp"

namespace statistics {

  using namespace std;
  using namespace disassembly;
  using namespace analysis;
  using namespace defs;
  using namespace utils;

  /* Constants */
  const uint8_t probe_able_size = 5;

  /* Private helper functions */

  inline bool isProbeAble(_DInst i) {
    return (i.size >= probe_able_size);
  }

  /* Public API implementation */
  Function generateMetaDataForFunction(BlockStructure bs, Decoded d) {

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

    bool fn_start_block = true;
    uint32_t last_probe_able_inst_distance = 0;
    uint32_t last_instruction_size = 0;

    bool probe_able_ins_found = false;
    ProbeMetaData bb_pmd;
    Address last_probe_able_inst_ip = (Address) NULL;

    // assert(bs.bbl.size() > 0);

    for (BasicBlock* bb : bs.bbl) {
      bb_pmd.total_instruction_count = 0;
      bb_pmd.probe_able_count= 0;
      bb_pmd.pii_count = 0;
      bb_pmd.start_instruction_size = 0;
      bb_pmd.start_probe_able_inst_distance = 0;
      bb_pmd.end_instruction_size = 0;
      bb_pmd.end_probe_able_inst_distance = 0;

      uint64_t offset = 0;

      for (uint64_t i = bb->start_ins_offset; i <= bb->end_ins_offset; i++) {
        bb_pmd.total_instruction_count++;
        fn_pmd.total_instruction_count++;

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

          if (fn_start_block) {
            fn_pmd.start_instruction_size = decoded[i].size;
          }
        }

        if (i == bb->end_ins_offset) {
          bb_pmd.end_instruction_size = decoded[i].size;
          last_instruction_size = decoded[i].size;
        }

        offset += decoded[i].size;
      }

      bb_pmd.end_probe_able_inst_distance =  (uint64_t)(bb->end - bb->start) - 
        last_probe_able_inst_distance;

      if (fn_start_block) {
        fn_start_block = false;
      }

      bb->pmd = bb_pmd;
    }

    fn_pmd.end_instruction_size = last_instruction_size;
    fn_pmd.end_probe_able_inst_distance = (uint64_t)fn.end - 
      (uint64_t)fn.start - (uint64_t)last_probe_able_inst_ip;

    if (fn_pmd.start_instruction_size == 0) {
      assert(false);
    }

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

    for (Function fn: *fns) {
      total_instruction_count += fn.pmd.total_instruction_count;
      total_probe_able_count += fn.pmd.probe_able_count;
      total_pii_count += fn.pmd.pii_count;

      fprintf(stderr, "Adding probe able count : %lu\n", fn.pmd.probe_able_count);
      stats.per_func_probe_able_count_hist->addItem(fn.pmd.probe_able_count);
      stats.per_func_pii_count_hist->addItem(fn.pmd.pii_count);
      stats.per_func_start_inst_size_hist->
        addItem(fn.pmd.start_instruction_size);
      stats.per_func_end_inst_size_hist->
        addItem(fn.pmd.end_instruction_size);
      stats.per_func_start_probe_able_inst_distance_hist->
        addItem(fn.pmd.start_probe_able_inst_distance);
      stats.per_func_end_probe_able_inst_distance_hist->
        addItem(fn.pmd.end_probe_able_inst_distance);

      for (BasicBlock* bb : fn.bbl) {
        stats.per_bb_probe_able_count_hist->addItem(bb->pmd.probe_able_count);
        stats.per_bb_pii_count_hist->addItem(bb->pmd.pii_count);
        stats.per_bb_start_inst_size_hist->
          addItem(bb->pmd.start_instruction_size);
        stats.per_bb_end_inst_size_hist->
          addItem(bb->pmd.end_instruction_size);
        stats.per_bb_start_probe_able_inst_distance_hist->
          addItem(bb->pmd.start_probe_able_inst_distance);
        stats.per_bb_end_probe_able_inst_distance_hist->
          addItem(bb->pmd.end_probe_able_inst_distance);
      }

    }

    stats.total_instruction_count = total_instruction_count;
    stats.total_probe_able_count = total_probe_able_count;
    stats.total_pii_count = total_pii_count;

    return stats;
  }

  void printStatistics(Statistics stats, FILE* fp, bool verbose) {

    fprintf(fp, "============= Probe Audit Report ===========\n\n");
    fprintf(fp, "Executable : %s\n", getProgramPath());
    fprintf(fp, "Instructions : %lu\n", stats.total_instruction_count); 
    fprintf(fp, "Probe able instructions (%%) : %lu (%f)\n", 
        stats.total_probe_able_count, 
        (double) stats.total_probe_able_count / stats.total_instruction_count);
    fprintf(fp, "Position independent instructions (%%) : %lu (%f)\n", 
        stats.total_pii_count, 
        (double) stats.total_pii_count / stats.total_instruction_count);

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
