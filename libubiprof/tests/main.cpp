
#include <stdio.h>
#include <instrument.h>

#define nop "nop; nop; nop; nop; nop;"

bool initialized = false;
Address probe_address = 0;
Address stub_address = 0;

void foo() {

  printf("In foo..\n");
  __asm__(nop); // 5 byte NOOP. Just enough for a relative jump.
  asm_label: 
  probe_address = (Address) &&asm_label-5; // Label as value here

  // printf("Foo 2..\n");

  /*
  if (stub_address && !initialized) {
    // relative distance is guaranteed to be within 2^32
    long relative = (long)(stub_address - (long)probe_address - 5);

    // Add the jump to the stub.
    probe_address[0] = 0xE9;
    *(int*)(probe_address+1) = (int)relative;

    initialized = true;
  }
  */

}

int main() {

  // Initial calls initializes the probe site
  foo();
  stub_address = createStub(probe_address);

  fprintf(stderr, "Stub created at : %p\n", stub_address);

  // This will add the call to the stub
  foo();

  // This will call the trampoline function via the stub
  // foo();

  return 0;

}
