
#include "doctest.h"
#include "concurrency.hpp"
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>

using utils::concurrency::ReadWriteLock;

const int CONCURRENCY_LEVEL = 32;

/****** Helpers *******/

void finishHandler(void* param) {
  int* completion_counter = (int*) param;
  *completion_counter += 1;
}

struct ReadFlags {
  int completionCount;
  int readerCount;
  ReadWriteLock* rwLock;
};

void* readLock(void* param) {
  int old;
  ReadFlags* flags = (ReadFlags*) param;
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old);
  pthread_cleanup_push(finishHandler, &flags->completionCount);

  int readerCount = flags->readerCount++;
  flags->rwLock->readLock();
  while (readerCount != CONCURRENCY_LEVEL) {
    ;
  }
  flags->rwLock->readUnlock();

  pthread_cleanup_pop(1);
  pthread_exit(0);
}

struct WriteFlags {
  int completionCount;
  int writerCount;
  int data;
  bool multi_writes_detected;
  ReadWriteLock* rwLock;
};

void* writeLock(void* param) {
  int old;
  WriteFlags* flags = (WriteFlags*) param;
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old);
  pthread_cleanup_push(finishHandler, &flags->completionCount);

  int writerCount = flags->writerCount++;
  while (writerCount != CONCURRENCY_LEVEL) {
    ;
  }
  flags->rwLock->writeLock();
  // printf("Entering while loop\n");
  /*
  while (writerCount != CONCURRENCY_LEVEL) {
    ;
  }
  */
  // printf("Exiting while loop\n");
  flags->data++;
  if (flags->data > 1) {
    printf("Done..\n");
    // flags->multi_writes_detected = false;
  }
  flags->data--;
  flags->rwLock->writeUnlock();

  pthread_cleanup_pop(1);
  pthread_exit(0);
}

struct ReadWriteFlags {
  bool writer_entered;
  bool reader_entered;
  bool invalid_data_detected;
  int data;
  int completionCount;
  ReadWriteLock* rwLock;
};

void* readerFirstLock(void* param) {
  int old;
  ReadWriteFlags* flags = (ReadWriteFlags*) param;
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old);
  pthread_cleanup_push(finishHandler, &flags->completionCount);

  flags->rwLock->readLock();
  flags->reader_entered = true;
  while (!flags->writer_entered) {
    ;
  }

  sleep(1); 
  if (flags->data == 1) {
    flags->invalid_data_detected = true;
  }

  flags->rwLock->readUnlock();

  pthread_cleanup_pop(0);
  pthread_exit(0);
}

void* writerSecondLock(void* param) {
  int old;
  ReadWriteFlags* flags = (ReadWriteFlags*) param;
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old);
  pthread_cleanup_push(finishHandler, &flags->completionCount);

  while (!flags->reader_entered) {
    ;
  }

  flags->writer_entered = true;
  flags->rwLock->writeLock();
  flags->data = 1;
  flags->rwLock->writeUnlock();

  pthread_cleanup_pop(0);
  pthread_exit(0);
}

void* writerFirstLock(void* param) {
  int old;
  ReadWriteFlags* flags = (ReadWriteFlags*) param;
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old);
  pthread_cleanup_push(finishHandler, &flags->completionCount);

  flags->rwLock->writeLock();
  flags->writer_entered = true;
  while (!flags->reader_entered) {
    ;
  }

  sleep(1);

  flags->data = 1;
  flags->rwLock->writeUnlock();

  pthread_cleanup_pop(0);
  pthread_exit(0);
}

void* readerSecondLock(void* param) {
  int old;
  ReadWriteFlags* flags = (ReadWriteFlags*) param;
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old);
  pthread_cleanup_push(finishHandler, &flags->completionCount);

  flags->reader_entered = true;
  flags->rwLock->readLock();

  if (flags->data != 1) {
    flags->invalid_data_detected = true;
  }

  flags->rwLock->readUnlock();

  pthread_cleanup_pop(0);
  pthread_exit(0);
}

/***** Tests ******/

TEST_SUITE("ReadWriteLock tests");

TEST_CASE("+ Multiple readers test") {

  ReadWriteLock rwLock;
  rwLock.clear();

  REQUIRE(!rwLock.isSet());

  ReadFlags flags = {0, 0, &rwLock};
  pthread_t th[CONCURRENCY_LEVEL];
  for (int i=0; i < CONCURRENCY_LEVEL; i++) {
    pthread_create(&th[i], NULL, readLock, &flags);
    pthread_detach(th[i]);
  }

  sleep(2);

  int retry_count = 0;
  if (flags.completionCount != CONCURRENCY_LEVEL) {
    if (retry_count > 2) {
      CHECK(false);
    }
    retry_count++;
    sleep(2);
  }

  for (int i=0; i < CONCURRENCY_LEVEL; i++) {
    pthread_cancel(th[i]);
  }
}

TEST_CASE("+ Multiple writers test") {

  ReadWriteLock rwLock;
  rwLock.clear();

  REQUIRE(!rwLock.isSet());

  WriteFlags flags = {0, 0, 0, false, &rwLock};
  pthread_t th[CONCURRENCY_LEVEL];
  for (int i=0; i < CONCURRENCY_LEVEL; i++) {
    pthread_create(&th[i], NULL, writeLock, &flags);
    pthread_detach(th[i]);
  }

  sleep(2);

  int retry_count = 0;
  if (flags.completionCount != CONCURRENCY_LEVEL) {
    if (retry_count > 2) {
      CHECK(flags.multi_writes_detected);
    }
    retry_count++;
    sleep(2);
  }

  for (int i=0; i < CONCURRENCY_LEVEL; i++) {
    // pthread_cancel(th[i]);
  }
}

TEST_CASE("+ Readers and writers (Reader first) test") {

  ReadWriteLock rwLock;
  rwLock.clear();

  REQUIRE(!rwLock.isSet());

  ReadWriteFlags flags = {false, false, false, 0, 0, &rwLock};
  pthread_t reader, writer;
  pthread_create(&reader, NULL, readerFirstLock, &flags);
  pthread_detach(reader);
  pthread_create(&writer, NULL, writerSecondLock, &flags);
  pthread_detach(writer);

  sleep(2);

  int retry_count = 0;
  if (flags.completionCount != 2) {
    if (retry_count > 2) {
      CHECK(!flags.invalid_data_detected);
    }
    retry_count++;
    sleep(2);
  }

  pthread_cancel(reader);
  pthread_cancel(writer);
}

TEST_CASE("+ Readers and writers (Writer first) test") {

  ReadWriteLock rwLock;
  rwLock.clear();

  REQUIRE(!rwLock.isSet());

  ReadWriteFlags flags = {false, false, false, 0, 0, &rwLock};
  pthread_t reader, writer;
  pthread_create(&writer, NULL, writerFirstLock, &flags);
  pthread_detach(writer);
  pthread_create(&reader, NULL, readerSecondLock, &flags);
  pthread_detach(reader);

  sleep(2);

  int retry_count = 0;
  if (flags.completionCount != 2) {
    if (retry_count > 2) {
      CHECK(!flags.invalid_data_detected);
    }
    retry_count++;
    sleep(2);
  }

  pthread_cancel(reader);
  pthread_cancel(writer);
}

TEST_SUITE_END();

// Read read 
// Read write
// Write write
// Multiple readers
// Multiple writers
