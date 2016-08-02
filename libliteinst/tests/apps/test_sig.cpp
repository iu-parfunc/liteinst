
extern "C" void dummy();

int main() {
  // dummy();
  while(true) {
    __asm(".byte 0x62");
    // __asm(".byte 0x62");
    // __asm(".byte 0xcc");
  }
}
