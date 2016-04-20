
#include "counter.hpp"

using std::string;
using std::map;
using std::pair;

void Counter::increment(std::string key, uint64_t value) {
  auto it = counts->find(key);
  if (it != counts->end()) {
    counts->erase(key); // TODO: This is stupid. But cannot get [] to work
    counts->insert(pair<string, uint64_t>(key, it->second + value));
  } else { 
    counts->insert(pair<string, uint64_t>(key, value));
  }

  total_count += value;
  
  return;
}

uint64_t Counter::get(string key) {
  auto it = counts->find(key);
  if (it != counts->end()) {
    return it->second;
  } 

  return 0;
}

map<string, uint64_t>* Counter::getAll() {
  return counts;
}
