#include <queue>
#include "nbqueue.h"
#include <stdio.h>

void NBQueue::initialize() {
  nbqueue = new queue<int>;
  rwlock = 0;
}

void NBQueue::enqueue(int val) {
  while (!(__sync_bool_compare_and_swap(&rwlock, 0 , 1)));

  nbqueue->push(val);
  __sync_bool_compare_and_swap(&rwlock, 1 , 0);
}

int NBQueue::dequeue() {
  while (!(__sync_bool_compare_and_swap(&rwlock, 0 , 1)));

  int val;
  if (!nbqueue->empty()) {
    val = nbqueue->front();
    nbqueue->pop();
  } else {  
    val = -1;
  }
  __sync_bool_compare_and_swap(&rwlock, 1 , 0);
  return val;
}

bool NBQueue::empty() {
  return nbqueue->empty(); 
}
