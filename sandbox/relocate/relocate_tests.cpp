
int conditional_jump() {
  int val = 0;
  if (val) {
    return val;
  } else {
    val++;
  }
  return val;
}

void mov_with_rip() {
  __asm("mov (%rip), %eax");
  __asm(".int 0x90909090");
}

int main() {
  mov_with_rip();
  conditional_jump();
}
