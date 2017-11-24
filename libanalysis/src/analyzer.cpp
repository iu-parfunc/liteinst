
#include "analyzer.hpp"

namespace analysis {

using std::string;
using std::list;


/*
Analyzer::Analyzer() {
  CFGBuilder* cfg = new CFGBuilder;
  CGBuilder* cg = new CGBuilder;
  IndirectTargetAnalysis* ita = new IndirectTargetAnalysis;
  IndirectControlFlowDump* icf = new IndirectControlFlowDump;

  pass_map.insert(std::pair<string, AnalysisPass*>(cfg->name, cfg));
  pass_map.insert(std::pair<string, AnalysisPass*>(cg->name, cg));
  pass_map.insert(std::pair<string, AnalysisPass*>(ita->name, ita));
  pass_map.insert(std::pair<string, AnalysisPass*>(icf->name, icf));
}

void Analyzer::run() {
  if (g == nullptr) {
    FILE* fp = fopen(out_file.c_str(), "a");
    g = new Graph;
    for (AnalysisPass* pass : passes) {
      g = pass->run(g, fp);
    }

    // write the graphs to fp
    fclose(fp);
  }
}

void Analyzer::setOutFile(string name) {
  out_file = name;
}

bool Analyzer::registerPass(AnalysisPass* pass) {
  if (pass_map.find(pass->name) != pass_map.end()) {
    return false;
  }

  pass_map.insert(std::pair<string, AnalysisPass*>(pass->name, pass));
  return true;
}

bool Analyzer::configure(list<string> names) {
  for (string pass : names) {
    auto it = pass_map.find(pass);
    if (it != pass_map.end()) {
      passes.push_back(it->second);
      return true;
    } else {
      passes.clear();
    } 
  }
  return false;
} */

CFG* Analyzer::getControlFlowGraph() {

  if (g == nullptr) {
    FILE* fp = fopen(out_file.c_str(), "a");
    g = new Graph;
    for (AnalysisPass* pass : passes) {
      g = pass->run(g, fp);
    }

    // write the graphs to fp
    fclose(fp);
  }

  return g->cfg;
}

CG* Analyzer::getCallGraph() {

  if (g == nullptr) {
    FILE* fp = fopen(out_file.c_str(), "a");
    g = new Graph;
    for (AnalysisPass* pass : passes) {
      g = pass->run(g, fp);
    }

    // write the graphs to fp
    fclose(fp);
  }

  return g->cg;
}

} // End analysis
