
#include <cstdio>

extern "C" void dummy();

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

int main() {

  // Do nothing.. 
  printf("Inside main..\n");

  // dummy();

  printf("do_while_func..\n");

  for (int i=0; i < 100000000; i++) {
    do_while_func();
    do_while_func();
  }

  /*
  printf("for_func..\n");
  for_func();
  for_func();
  printf("go_to_begin_func..\n");
  go_to_begin_func();
  go_to_begin_func();
  printf("go_to_end_func..\n");
  go_to_end_func();
  go_to_end_func();
  printf("if_func..\n");
  if_func();
  if_func();
  printf("while_func..\n");
  while_func();
  while_func();
  */
}
