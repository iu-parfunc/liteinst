
#include <unistd.h>
#include <sys/param.h> // MAXPATHLEN

#include "statistics.hpp"

using namespace std;
using namespace disassembly;
using namespace analysis;

namespace statistics {

  /* Constants */
  const uint8_t probe_able_size = 5;

  /* Private helper functions */

  inline bool isProbeAble(_DInst i) {
    return (i.size >= probe_able_size);
  }

  void printHistogram(Histogram* hist, FILE* fp) {
    int PRINT_PRECISION = 2;
    list<Bin> bins = hist->getBins();
    for (Bin bin : bins) {
      fprintf(fp, "[%lu-%lu] ", bin.bin_range.start, bin.bin_range.end);
      uint64_t num_bars = (uint64_t) ceil(bin.contribution / PRINT_PRECISION);
      for (uint64_t j = 0; j < num_bars; j++) {
        fprintf(fp, "@");
      }

      // Max num bars is 50 with PRINT_PRECISION 2. i.e. one bar represents 2% of 
      // the total
      uint64_t padding = 100/PRINT_PRECISION - num_bars; 
      for (uint64_t j = 0; j < padding; j++) {
        fprintf(fp, " ");
      }

      fprintf(fp, " %f\n", bin.contribution);
    } 
  }

  char* getExecutablePath() {
    char* binary_path = (char*) malloc(sizeof(char) * MAXPATHLEN);
    ssize_t len = readlink("/proc/self/exe", binary_path, sizeof(binary_path));
    if (len == -1 || len == sizeof(binary_path)) {
      len = 0;
    }
    binary_path[len] = '\0';

    return binary_path;
  }


  /* Public API implementation */
  Function generateMetaData(Address start, Address end,  BlockStructure bs,
      Decoded d) {

    _DInst* decoded = d.decoded_instructions;

    Function fn;
    fn.start = start;

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

    if (bs.bbl != NULL) {

      bool probe_able_ins_found = false;
      ProbeMetaData bb_pmd;

      for (BasicBlock bb : *(bs.bbl)) {
        bb_pmd.total_instruction_count = 0;
        bb_pmd.probe_able_count= 0;
        bb_pmd.pii_count = 0;
        bb_pmd.start_instruction_size = 0;
        bb_pmd.start_probe_able_inst_distance = 0;
        bb_pmd.end_instruction_size = 0;
        bb_pmd.end_probe_able_inst_distance = 0;

        uint32_t offset = 0;

        for (uint64_t i = bb.start_ins_offset; i <= bb.end_ins_offset; i++) {
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
          }

          /*
          if (isPositionIndependent(decoded[i])) {
            bb_pmd.pii_count++;
            fn_pmd.pii_count++;
          }
          */

          if (i == bb.start_ins_offset) {
            bb_pmd.start_instruction_size = decoded[i].size;

            if (fn_start_block) {
              fn_pmd.start_instruction_size = decoded[i].size;
            }
          }

          if (i == bb.end_ins_offset) {
            bb_pmd.end_instruction_size = decoded[i].size;
            last_instruction_size = decoded[i].size;
          }
        }

        bb_pmd.end_probe_able_inst_distance = last_probe_able_inst_distance;
        if (fn_start_block) {
          fn_start_block = false;
        }

        bb.pmd = bb_pmd;
      }

      fn_pmd.end_instruction_size = last_instruction_size;
      fn_pmd.end_probe_able_inst_distance = last_probe_able_inst_distance;

      fn.pmd = fn_pmd;
      fn.end = bs.fn_end;
      fn.bbl = bs.bbl;
      fn.returns = bs.returns;
    } else {
      fn.end = end;
      fn.bbl = NULL;
      fn.returns = NULL;
      fprintf(stderr, "No basic block information found..\n");
    }

    return fn;
  }

  Statistics generateStatistics(list<Function>* fns) {
    Statistics stats;
    stats.per_func_probe_able_count_hist = new Histogram(0, 10);
    stats.per_func_pii_count_hist = new Histogram(0, 10);
    stats.per_func_start_inst_size_hist = new Histogram(0, 15, 1);
    stats.per_func_end_inst_size_hist = new Histogram(0, 15 , 1);
    stats.per_func_start_probe_able_inst_distance_hist = new Histogram(0, 5);
    stats.per_func_end_probe_able_inst_distance_hist = new Histogram(0, 5);

    stats.per_bb_probe_able_count_hist = new Histogram(0, 5);
    stats.per_bb_pii_count_hist = new Histogram(0, 5);
    stats.per_bb_start_inst_size_hist = new Histogram(0, 15, 1);
    stats.per_bb_end_inst_size_hist = new Histogram(0, 15, 1);
    stats.per_bb_start_probe_able_inst_distance_hist = new Histogram(0, 5);
    stats.per_bb_end_probe_able_inst_distance_hist = new Histogram(0, 5);

    uint64_t total_instruction_count;
    uint64_t total_probe_able_count;
    uint64_t total_pii_count;

    for (Function fn: *fns) {
      total_instruction_count += fn.pmd.total_instruction_count;
      total_probe_able_count += fn.pmd.probe_able_count;
      total_pii_count += fn.pmd.pii_count;

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

      for (BasicBlock bb : *(fn.bbl)) {
        stats.per_bb_probe_able_count_hist->addItem(bb.pmd.probe_able_count);
        stats.per_bb_pii_count_hist->addItem(bb.pmd.pii_count);
        stats.per_bb_start_inst_size_hist->
          addItem(bb.pmd.start_instruction_size);
        stats.per_bb_end_inst_size_hist->
          addItem(bb.pmd.end_instruction_size);
        stats.per_bb_start_probe_able_inst_distance_hist->
          addItem(bb.pmd.start_probe_able_inst_distance);
        stats.per_bb_end_probe_able_inst_distance_hist->
          addItem(bb.pmd.end_probe_able_inst_distance);
      }
    }

    stats.total_instruction_count = total_instruction_count;
    stats.total_probe_able_count = total_probe_able_count;
    stats.total_pii_count = total_pii_count;

    return stats;
  }

  void printStatistics(Statistics stats, FILE* fp, bool verbose) {

    char* path = getExecutablePath();
    fprintf(fp, "======= Probe Audit Report ========\n\n");
    fprintf(fp, "Executable : %s\n", path);
    fprintf(fp, "# instructions : %lu\n", stats.total_instruction_count); 
    fprintf(fp, "# probe able instructions (%%) : %lu (%f)\n", 
        stats.total_probe_able_count, 
        (double) stats.total_probe_able_count / stats.total_instruction_count);
    fprintf(fp, "# position independent instructions (%%) : %lu (%f)\n", 
        stats.total_pii_count, 
        (double) stats.total_pii_count / stats.total_instruction_count);

    if (verbose) {
      fprintf(fp, "\n");
      fprintf(fp, "-- Histograms \n");
      fprintf(fp, "---- Function Level \n");
      fprintf(fp, "------ # probe able instructions per function \n\n");
      printHistogram(stats.per_func_probe_able_count_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ # position independent instructions per function \n\n");
      printHistogram(stats.per_func_pii_count_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ Start instruction size of functions \n\n");
      printHistogram(stats.per_func_start_inst_size_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ End instruction size of functions \n\n");
      printHistogram(stats.per_func_end_inst_size_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ First probe able instruction distance from start for " 
          "functions \n\n");
      printHistogram(stats.per_func_start_probe_able_inst_distance_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ First probe able instruction distance from end for " 
          "functions \n\n");
      printHistogram(stats.per_func_end_probe_able_inst_distance_hist, fp);

      fprintf(fp, "\n\n");

      fprintf(fp, "---- Basic Block Level \n");
      fprintf(fp, "------ # probe able instructions per function \n\n");
      printHistogram(stats.per_func_probe_able_count_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ # position independent instructions per function \n\n");
      printHistogram(stats.per_func_pii_count_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ Start instruction size of functions \n\n");
      printHistogram(stats.per_func_start_inst_size_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ End instruction size of functions \n\n");
      printHistogram(stats.per_func_end_inst_size_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ First probe able instruction distance from start for " 
          "functions \n\n");
      printHistogram(stats.per_func_start_probe_able_inst_distance_hist, fp);

      fprintf(fp, "\n");
      fprintf(fp, "------ First probe able instruction distance from end for " 
          "functions \n\n");
      printHistogram(stats.per_func_end_probe_able_inst_distance_hist, fp);

    }

    fprintf(fp, "\n");
    fprintf(fp, "========== END ===========\n");

    free(path);

  }

}
