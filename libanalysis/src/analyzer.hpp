
#ifndef ANALYZER_H
#define ANALYZER_H

#include <string>
#include <map>
#include <list>
#include "cfg.hpp"

namespace analysis {

class Graph {
  public:
    CFG* cfg = nullptr;
    CG*  cg  = nullptr;
};

class AnalysisPass {
  public:
    std::string name;

    AnalysisPass(std::string name) : name(name) {}

    virtual Graph* run(Graph* g, FILE* fp) = 0;
};

class Analyzer {
  private:
    std::string out_file;
    std::map<std::string, AnalysisPass*> pass_map;
    std::list<AnalysisPass*> passes;
    Graph* g = nullptr;
    // Process* process;

  public:

    Analyzer() {}

    ~Analyzer() {}

void setOutFile(std::string name) {
  out_file = name;
}
    
bool registerPass(AnalysisPass* pass) {
  if (pass_map.find(pass->name) != pass_map.end()) {
    return false;
  }

  pass_map.insert(std::pair<std::string, AnalysisPass*>(pass->name, pass));
  return true;
}

bool configure(std::list<std::string> names) {
  for (std::string pass : names) {
    printf("Configuring pass : %s\n", pass.c_str());
    auto it = pass_map.find(pass);
    if (it != pass_map.end()) {
      passes.push_back(it->second);
    } else {
      passes.clear();
      return false;
    } 
  }

  printf("Number of passes : %d\n", passes.size());

  return true;
}

void run() {
  if (g == nullptr) {
    printf("Run!!!\n");
    FILE* fp = fopen(out_file.c_str(), "a");
    g = new Graph;
    for (AnalysisPass* pass : passes) {
      printf("Running %s\n", pass->name.c_str());
      g = pass->run(g, fp);
    }

    // write the graphs to fp
    fclose(fp);
  }
}

    CFG* getControlFlowGraph();
    CG*  getCallGraph();
    // Process* getProcessInfo(); 

}; 

class CFGBuilder : public AnalysisPass {
  public:
    CFGBuilder() : AnalysisPass("cfg") {
    }

    Graph* run(Graph* g, FILE* fp);
};

class CGBuilder : public AnalysisPass {
  public:
    CGBuilder() : AnalysisPass("cg") {
    }

    Graph* run(Graph* g, FILE* fp);
};

class IndirectTargetAnalysis : public AnalysisPass {
  public:
    IndirectTargetAnalysis() : AnalysisPass("indirect_target_analysis") {
    }

    Graph* run(Graph* g, FILE* fp);
};

class IndirectControlFlowDump : public AnalysisPass {
  public:
    IndirectControlFlowDump() : AnalysisPass("indirect_control_flow_dump") {
    }

    Graph* run(Graph* g, FILE* fp);
};

} // End analysis

#endif /*ANALYZER_H*/
