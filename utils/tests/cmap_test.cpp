
#include "doctest.h"
#include "concurrency.hpp"
#include "concurrent_map_test.hpp"

#include <string>
#include <iostream>
#include <thread>

using std::cout;
using std::pair;
using std::string;
using std::thread;
using utils::concurrency::ConcurrentMap;

const int CONCURRENCY_LEVEL = 10;

// Global map used in some tests
ConcurrentMap<int, string> map;

void insertionTask(int val, int& thread_counter) {
  thread_counter++;
  map.insert(val, to_string(val));
  thread_counter--;
}

TEST_SUITE("Concurrent Map Tests");

TEST_CASE("+ Size test") {
  ConcurrentMap<int, string> map;
  CHECK(map.size() == 0);
}

TEST_CASE("+ Insert test") {
  ConcurrentMap<int, string> map;
  map.insert(2, "Hello");
  CHECK(map.size() == 1);
}

TEST_CASE("+ Emplace test") {
  ConcurrentMap<int, string> map;
  map.emplace(2, "Hello");
  CHECK(map.size() == 1);
}

TEST_CASE("+ Erase test") {
  ConcurrentMap<int, string> map;
  map.insert(2, "Hello");
  CHECK(map.size() == 1);

  map.erase(2);
  CHECK(map.size() == 0);
}

TEST_CASE("+ Find test") {
  ConcurrentMap<int, string> map;
  map.insert(2, "Hello");

  SUBCASE("++ Existing element") {
    auto it = map.find(2);
    CHECK(it != map.end());
    CHECK(it->first == 2);
    CHECK(string("hello").compare(it->second));
  }

  SUBCASE("++ Non existing element") {
    auto it = map.find(0);
    CHECK(it == map.end());
  }
}

TEST_CASE("+ Clear test") {
  ConcurrentMap<int, string> map;
  map.insert(2, "Hello");
  CHECK(map.size() == 1);

  map.clear();
  CHECK(map.size() == 0);
}

TEST_CASE("+ Exclusive lock test") {
  map.clear();

  REQUIRE(map.size() == 0);

  int thread_counter = 0;
  map.acquireUpdateLock();

  for (int i=0; i < CONCURRENCY_LEVEL; i++) {
    thread t(insertionTask, i, std::ref(thread_counter));
    t.detach();
  }

  while(thread_counter != CONCURRENCY_LEVEL);

  CHECK(map.size() == 0);
  map.releaseUpdateLock();

  while(thread_counter != 0);
  CHECK(map.size() == CONCURRENCY_LEVEL);

}

TEST_SUITE_END();
