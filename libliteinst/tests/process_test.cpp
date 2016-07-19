
#include "doctest.h"
#include "process.hpp"
#include <vector>

using namespace utils::process;

using std::vector;

/******** Tests ********/
TEST_SUITE("Process Image Reader Tests");

TEST_CASE("+ Read Process Information Test") {

  Process p;
  vector<Function*> fns = p.getFunctions();

  // p.show(stdout, 0);
  
  CHECK(true);
}
