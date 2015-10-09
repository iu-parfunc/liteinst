#ifndef NBQUEUE_HPP_
#define NBQUEUE_HPP_

#include <queue>

using namespace std;

class NBQueue {

private:
 queue<long>* nbqueue; 
 int rwlock;
 int capacity;
 int size;

public:
 NBQueue(int capacity) {initialize(capacity);}
 void initialize(int capacity);
 void enqueue(long val);
 long dequeue();
 bool empty();
 virtual ~NBQueue() {}
};

#endif /* NBQUEUE_HPP_ */

