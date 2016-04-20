
#include <string>
#include <cstdint>
#include <map>

#include "defs.hpp"

class Counter : public defs::Show {
  private:
    uint64_t total_count;
    std::map<std::string, uint64_t>* counts;

  public:
    Counter() {
      counts = new std::map<std::string, uint64_t>();
      total_count = 0;
    }

    void increment(std::string key, uint64_t value);
    uint64_t get(std::string key);
    std::map<std::string, uint64_t>* getAll();

    void show(FILE* fp, int nspaces) {
      std::string left_pad = getPadding(nspaces);
      for(auto it : *counts) {
        fprintf(fp, "%s[%s] %lu (%f%%)\n", left_pad.c_str(), it.first.c_str(),
            it.second, (double)it.second / total_count * 100);
      }
    }

};
