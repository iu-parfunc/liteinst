
#include <cstdio>

extern "C" void dummy();

void __attribute__ ((noinline)) five_bytes_func() {
  __asm(".int 0x00401f0f");
  return;
}

void __attribute__ ((noinline)) six_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".byte 0x90");
  return;
}

void __attribute__ ((noinline)) seven_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  return;
}

void __attribute__ ((noinline)) eight_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  __asm(".byte 0x90");
  return;
}

void __attribute__ ((noinline)) nine_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  return;
}

void __attribute__ ((noinline)) ten_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  __asm(".byte 0x90");
  return;
}

void __attribute__ ((noinline)) eleven_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  return;
}

void __attribute__ ((noinline)) tweleve_bytes_func() {
  __asm(".int 0x00401f0f");
  __asm(".int 0x00401f0f");
  __asm(".short 0x9066");
  __asm(".byte 0x90");
  return;
}

int main() {

  // Do nothing.. 
  printf("Inside main..\n");

  // dummy();

  printf("FIVE byte func..\n");
  five_bytes_func();
  five_bytes_func();
  printf("SIX byte func..\n");
  six_bytes_func();
  six_bytes_func();
  printf("SEVEN byte func..\n");
  seven_bytes_func();
  seven_bytes_func();
  printf("EIGHT byte func..\n");
  eight_bytes_func();
  eight_bytes_func();
  printf("NINE byte func..\n");
  nine_bytes_func();
  nine_bytes_func();
  printf("TEN byte func..\n");
  ten_bytes_func();
  ten_bytes_func();
  printf("ELEVEN byte func..\n");
  eleven_bytes_func();
  eleven_bytes_func();
  printf("TWELVE byte func..\n");
  tweleve_bytes_func();
  tweleve_bytes_func();

  printf("Exiting main..\n");
}
