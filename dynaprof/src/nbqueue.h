#ifndef NBQUEUE_HPP_
#define NBQUEUE_HPP_

#include <queue>

using namespace std;

class NBQueue {

private:
 queue<int>* nbqueue; 
 int rwlock;

public:
 NBQueue() {initialize();}
 void initialize();
 void enqueue(int val);
 int dequeue();
 bool empty();
 virtual ~NBQueue() {}
};

#endif /* NBQUEUE_HPP_ */

