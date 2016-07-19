
#include "proc.hpp"

using namespace proc;

long  for_func() {
  int sum = 0;
  for (int i=0; i < 10; i++) {
    sum += i;
  }

  return sum++;
}

long if_func() {
  int sum = 0;
  if (sum > 10) {
    sum *= sum;
  }

  sum *= (sum + 3);
  return sum++;
}

long while_func() {
  int counter = 20;
  int sum = 0; 
  while (counter >= 0) {
    sum += counter;
    counter--;
  }

  return sum;
}

long do_while_func() {
  int counter = 10;
  int sum = 0;
  do {
    sum += 1;
  } while (--counter >= 0);

  return sum;
}

long go_to_begin_func() {
  int sum = 0;

begin:
  if (sum > 10) {
    return sum;
  } else {
    while(0) {
      sum += 3;
      goto begin;
    }
  }
}

long go_to_end_func() {
  int sum = 0;

  while(0) {
    sum += 3;
    if (sum > 10) {
      goto end;
    }
  }

end:
  return sum;
}

void empty() {
  return;
}

extern "C" void audit();

int main() {

  // Do nothing.. 

}
