
#include "doctest.h"
#include "concurrency.hpp"
#include <unistd.h>
#include <pthread.h>

using utils::concurrency::SpinLock;

const int CONCURRENCY_LEVEL = 32;

/******** Helpers *********/
void completionHandler(void* param) {
  int* counter = (int*) param;
  __sync_fetch_and_add(counter, 1);
}

struct SpinLockData {
  int lockedCounter;
  int entryCounter;
  int completionCounter;
  SpinLock* lock;
};

void* tryLock(void* param) {

  SpinLockData* data = (SpinLockData*) param;
  pthread_cleanup_push(completionHandler, &data->completionCounter);

  __sync_fetch_and_add(&data->entryCounter, 1);
  
  data->lock->lock();
  // printf("[TRYLOCK] Increasting lockedCounter : %d\n", data->lockedCounter + 1);
  // __sync_fetch_and_add(&data->lockedCounter, 1); 
  data->lockedCounter++;
  data->lock->unlock();

  pthread_cleanup_pop(1);
  pthread_exit(0);
}

/****** Tests ********/
TEST_SUITE("SpinLock Tests");

TEST_CASE("+ Lock and unlock test") {
  SpinLock lock; 
  CHECK(!lock.isSet());

  lock.lock();
  CHECK(lock.isSet());

  lock.unlock();
  CHECK(!lock.isSet());
}

TEST_CASE("+ Ownership test") {
  SpinLock lock;
  CHECK(!lock.isOwner());

  lock.lock();
  CHECK(lock.isOwner());

  lock.unlock();
  CHECK(!lock.isOwner());
}

TEST_CASE("+ Mutual exclusion test") {
  SpinLock lock;
  CHECK(!lock.isSet());

  lock.lock();
  CHECK(lock.isSet());
  CHECK(lock.isOwner());

  SpinLockData spd = {0, 0, 0, &lock};
  pthread_t th[CONCURRENCY_LEVEL];

  int is_spawned = 0;
  for (int i=0; i < CONCURRENCY_LEVEL; i++) {
    is_spawned |=  pthread_create(&th[i], NULL, tryLock, &spd);
  }
  
  CHECK(!is_spawned);

  while (spd.entryCounter < CONCURRENCY_LEVEL);

  CHECK(spd.entryCounter == CONCURRENCY_LEVEL);
  CHECK(spd.lockedCounter == 0);

  // printf("Unlocking the lock..\n");
  lock.unlock();

  for (int i=0; i< CONCURRENCY_LEVEL; i++) {
    pthread_join(th[i], NULL);
  }

  CHECK(spd.entryCounter == CONCURRENCY_LEVEL);
  CHECK(spd.lockedCounter == CONCURRENCY_LEVEL);
  CHECK(spd.completionCounter == CONCURRENCY_LEVEL);
}

TEST_SUITE_END();
