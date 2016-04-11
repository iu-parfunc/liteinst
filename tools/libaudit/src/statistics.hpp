
#ifndef _STATISTICS_HPP_
#define _STATISTICS_HPP_

#include <list>

#include "defs.hpp"
#include "histogram.hpp"
#include "analysis.hpp"

namespace statistics {

  typedef struct {
    uint64_t total_instruction_count;
    uint64_t total_probe_able_count;
    uint64_t total_pii_count;

    Histogram* per_func_probe_able_count_hist;
    Histogram* per_func_pii_count_hist; 

    Histogram* per_func_start_inst_size_hist;
    Histogram* per_func_end_inst_size_hist; 

    Histogram* per_func_start_probe_able_inst_distance_hist;
    Histogram* per_func_end_probe_able_inst_distance_hist; 

    Histogram* per_bb_probe_able_count_hist;
    Histogram* per_bb_pii_count_hist; 

    Histogram* per_bb_start_inst_size_hist;
    Histogram* per_bb_end_inst_size_hist; 

    Histogram* per_bb_start_probe_able_inst_distance_hist;
    Histogram* per_bb_end_probe_able_inst_distance_hist; 

  } Statistics;


  analysis::Function generateMetaData(Address start, Address end,  
      analysis::BlockStructure bs, disassembly::Decoded d);

  Statistics generateStatistics(std::list<analysis::Function>* fns);

  void printStatistics(Statistics stats, FILE* fp, bool verbose) ;

}

#endif /* _STATISTICS_HPP_ */
