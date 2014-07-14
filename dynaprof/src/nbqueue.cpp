#include <queue>
#include "nbqueue.h"
#include <stdio.h>

void NBQueue::initialize(int val) {
  nbqueue = new queue<long>;
  rwlock = 0;
  capacity = val;
  size = 0;
}

void NBQueue::enqueue(long val) {
  while (!(__sync_bool_compare_and_swap(&rwlock, 0 , 1)));

  if (size < capacity-1) { 
    nbqueue->push(val);
    size++;
  } else {
    nbqueue->pop();
    nbqueue->push(val);
  }

  __sync_bool_compare_and_swap(&rwlock, 1 , 0);
}

long NBQueue::dequeue() {
  while (!(__sync_bool_compare_and_swap(&rwlock, 0 , 1)));

  int val;
  if (!nbqueue->empty()) {
    val = nbqueue->front();
    nbqueue->pop();
    size--;
  } else {  
    val = -1;
  }
  __sync_bool_compare_and_swap(&rwlock, 1 , 0);
  return val;
}

bool NBQueue::empty() {
  return nbqueue->empty(); 
}
