
int main() {
  // asm(".byte 0x66 0x0f 0x1f 0x84 0x00 0x00 0x02 0x00 0x00\n\t");
  // asm("nopw 512(%rax,%rax)"); 
  // asm("nopw %cs:512(%rax,%rax)");
  asm("movq $0x0011001100110011, %rcx");
  asm("cmpq %rcx, %rdi");
  asm("jne .+17");
  asm("movq $0x0011001100110011, %rax");
  asm("jmp .+4096"); 
  // asm("jecxz .+3");
  // asm("nop");

}
