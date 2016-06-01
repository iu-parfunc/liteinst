
#include "concurrency.hpp"
#include "bandit/bandit.h"
#include "concurrent_map_test.hpp"

#include <assert.h>
#include <string>

using namespace bandit;

go_bandit([](){
  describe("Testing Concurrent Map:\n", [](){
    describe("Testing basic operations:\n", [&](){

      utils::concurrency::ConcurrentMap<int, std::string> map;

      before_each([&](){
        map.clear();
      });

      it("Testing map insertion & size", [&]() {
         map.insert(2, "Hello");
         AssertThat(map.size(), Equals(1));
      });

      it("Testing map find", [&]() {
         map.insert(2, "Hello");
         auto it = map.find(2);

         AssertThat(it, !Equals(map.end()));
         AssertThat(it->first, Equals(2));
         AssertThat(it->second, Equals("Hello"));
      });

      it("Testing map erase", [&]() {
         map.insert(4, "World");
         AssertThat(map.size(), Equals(1));

         map.erase(4);
         AssertThat(map.size(), Equals(0));
      });
    });

    describe("Testing custom comparators:\n", [&](){

      utils::concurrency::ConcurrentMap<Key, Value, Comparator> map;

      before_each([&](){
        map.clear();
      });

      it("Testing map insertion & size", [&]() {
         map.insert(Key(1, 2), Value(1));
         auto it = map.find(Key(1, 2));

         AssertThat(map.size(), Equals(1));
         // AssertThat(it, !Equals(map.end()));
         AssertThat(it->first.start, Equals(1));
         AssertThat(it->first.end, Equals(2));
         AssertThat(it->second.value, Equals(1));
      });

      it("Testing map erase", [&]() {
         map.insert(Key(1, 2), Value(1));
         AssertThat(map.size(), Equals(1));

         map.erase(Key(1, 2));
         AssertThat(map.size(), Equals(0));
      });
    });


  });
});

int main(int argc, char* argv[]) {
  return bandit::run(argc, argv);
  /*
  utils::concurrency::ConcurrentMap<int, std::string> map;

  map.insert(2, "Hello");
  auto it = map.find(2);

  assert(it != map.end());
  assert(it->first == 2);
  assert(it->second == "Hello");
  map.erase(2);

  assert(it == map.end());
  */
}
