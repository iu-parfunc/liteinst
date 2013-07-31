// [AsmJit]
// Complete JIT Assembler for C++ Language.
//
// [License]
// Zlib - See COPYING file in this package.

// This file is only included as an example and simple test if jit
// compiler works.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <AsmJit/AsmJit.h>

// This is type of function we will generate
typedef int (*MyFn)();

static void calledFn() { 
  printf("MADE IT -- to the called function!\n");
  printf("Hmm... this should print once\n");
  return; 
}

int main(int argc, char* argv[])
{
  using namespace AsmJit;

  // ==========================================================================
  // Create assembler.
  Assembler a;

  // Log assembler output.
  FileLogger logger(stderr);
  a.setLogger(&logger);

#define $PRO_EPI
#ifdef PRO_EPI
  // Prolog.
  a.push(nbp);
  a.mov(nbp, nsp);
#endif

  // Mov 1024 to EAX/RAX, EAX/RAX is also return value.
  a.mov(nax, 1024);
  //  a.mov(nax, 512);
  // a.call(imm((sysint_t)calledFn));
  a.jmp(imm((sysint_t)calledFn));

#ifdef PRO_EPI
  // Epilog.
  a.mov(nsp, nbp);
  a.pop(nbp);
  a.ret();
#endif
  // ==========================================================================

  // NOTE:
  // This function can be also completely rewritten to this form:
  //   a.mov(nax, 1024);
  //   a.ret();
  // If you are interested in removing prolog and epilog, please
  // study calling conventions and check register preservations.

  printf("Statically compiled function we're aiming to call is at: %p\n", &calledFn);

  int sz = a.getCodeSize();
  printf("Got the raw code, size %d, offset %d, assembler at %p\n", sz, a.getOffset(), &a);

#if 1
  // uint8_t* ptr = a.takeCode();
  // printf("Stole the code away %p\n", ptr);
  // for(int i=0; i<sz; i++)  {
  //   printf("  byte %d at %p = %d\n", i, &ptr[i], ptr[i]);
  //   fflush(stdout);
  // }


  printf("Next let's use the memory manager...\n");
  MemoryManager* memmgr = MemoryManager::getGlobal();
  void* p2 = memmgr->alloc(sz + 16, MEMORY_ALLOC_FREEABLE);
/*
  printf("Got handle on executable memory: %p\n", p2);
  sysuint_t code = a.relocCode(p2);
  printf("Relocated the code there, return code: %d\n", code);
*/

 // Alloc memory for your function.
 MyFn fn = function_cast<MyFn>( MemoryManager::getGlobal()->alloc(a.getCodeSize()) );
  // Relocate the code (will make the function).
 sysuint_t code = a.relocCode((void*)fn);
  printf("Relocated the code, return code: %d, next call it!\n", code);

  // fn();
   // That funcall generates this code:
  // 423d31:	48 8b 85 58 ff ff ff 	mov    -0xa8(%rbp),%rax
  // 423d38:	ff d0                	callq  *%rax
  // 423d3a:	bf 44 b2 42 00       	mov    $0x42b244,%edi
  // 423d3f:	e8 f4 cf fd ff       	callq  400d38 <puts@plt>

  // Therefore, this works:
  // __asm__  ( "mov    -0xa8(%rbp),%rax\n"
  // 	     "callq  *%rax"
  // 	     );

  // And this makes it there but not back (good):
  __asm__  ( "mov    -0xa8(%rbp),%rax\n"
	     "jmp  *%rax"
	     );

   printf("Returned from call.  Done.\n");
   return 0;

  // // Relocate the code.
  // sysuint_t relocatedSize = assembler->relocCode(p);
  // // Return unused memory to MemoryManager.
  // if (relocatedSize < codeSize)
  // {
  //   memmgr->shrink(p, relocatedSize);
  // }
  // // Mark memory if MemoryMarker provided.
  // if (_memoryMarker)
  // {
  //   _memoryMarker->mark(p, relocatedSize);
  // }
  // // Return the code.
  // *dest = p;
  // return ERROR_NONE;


//====================================================================================================
// Having lots of trouble trying to jump directly to the generated code
//====================================================================================================
#define jumpto(adr) asm("jmp *%0"::"r"(adr):)

  printf("Now to jump to that address (this will probably crash us...)\n");
  // jumpto(ptr);
  // asm("jmp *%0"::"r"(p2):);
  // asm("ljmp *%0"::"r"(p2):);
  // asm("ljmp *%0\n"::"m"(p2):);

  asm("ljmp *%0\n"::"m"(fn):);

  // uint cs_register = 0;
  // __asm__ volatile ("ljmp %0, $flush\n"
  //                         "flush:"
  //                         :: "r" (cs_register)
  //                         : "%al");

  // 48 bit addresses?
  // 0x 7f62 9a0fc000
  struct {unsigned int offset; unsigned short segment;} dest;
  unsigned long n2 = (unsigned long)p2;
  dest.segment = 0x00; // whatever value you want in CS
  dest.offset  = (unsigned short)n2;
  printf("Computed segment/offset to jump to: %d / %d\n", dest.segment, dest.offset);
  printf("Computed segment/offset to jump to: %p / %p\n", (long)dest.segment, (long)dest.offset);

  // asm volatile (
  //    "movl $1f, %0\n"
  //    "ljmp *%0\n"
  //    "1:" :: "m"(dest));

  // asm volatile (
  //    "movl $1f, %0\n"
  //    "ljmp *%0\n"
  //    "1:" :: "m"(p2));


    // __asm__ volatile ("pushw %[cs]\n"
    //                       "pushl $1f\n"
    //                       "retf\n"
    //                       ""
    //                       "1:"
    //                       :: [cs] "nr" (cs_register));


  printf("Wait, why are we still here?\n");
  return 0;
//====================================================================================================
#else

  // ==========================================================================
  // Make the function.
  MyFn fn = function_cast<MyFn>(a.make());

  printf("Got the MyFn, size %d, addr %p\n", sizeof(fn), &fn);

  // Call it.
  fn();

  // Free the generated function if it's not needed anymore.
  MemoryManager::getGlobal()->free((void*)fn);
  // ==========================================================================

  printf("All done\n");
  return 0;
#endif
}
