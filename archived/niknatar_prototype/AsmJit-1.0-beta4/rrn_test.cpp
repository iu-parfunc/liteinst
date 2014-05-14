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

#define EXTRA_SPACE 16

// This is type of function we will generate
typedef int (*MyFn)();

static void calledFn() { 
  printf("MADE IT -- to the called function!\n");
  printf("  Hmm... this should print once\n");
  return; 
}

using namespace AsmJit;

void call_properly(Assembler& a) 
{
  // Make the function.
  MyFn fn = function_cast<MyFn>(a.make());
  printf("Got the MyFn, size %d, addr %p\n", sizeof(fn), &fn);

  // Call it.
  fn();

  // Free the generated function if it's not needed anymore.
  MemoryManager::getGlobal()->free((void*)fn);

  printf("All done\n");
  return;
}

// ====================================================================================================

void manual_jmp_there(Assembler& a) 
{
  int sz = a.getCodeSize();
  printf("Got the raw code, size %d, offset %d, assembler at %p\n", sz, a.getOffset(), &a);
   // uint8_t* ptr = a.takeCode();
  // printf("Stole the code away %p\n", ptr);
  // for(int i=0; i<sz; i++)  {
  //   printf("  byte %d at %p = %d\n", i, &ptr[i], ptr[i]);
  //   fflush(stdout);
  // }

#if 1
  printf("Next let's use the memory manager...\n");
  MemoryManager* memmgr = MemoryManager::getGlobal();
  // This works just as well, don't need the function_cast magic:
  void* fn = memmgr->alloc(sz + EXTRA_SPACE, MEMORY_ALLOC_FREEABLE);
#else
 MyFn fn = function_cast<MyFn>( MemoryManager::getGlobal()->alloc(a.getCodeSize() + EXTRA_SPACE) );
#endif
 sysuint_t code = a.relocCode((void*)fn);
 printf("Relocated the code to location %p, return code: %d, next call it!\n", (void*)fn, code);

  // fn();
   // ^^ That funcall generates this code:
  // 423d31:	48 8b 85 58 ff ff ff 	mov    -0xa8(%rbp),%rax
  // 423d38:	ff d0                	callq  *%rax
  // 423d3a:	bf 44 b2 42 00       	mov    $0x42b244,%edi
  // 423d3f:	e8 f4 cf fd ff       	callq  400d38 <puts@plt>

  // (1) Therefore, this works:
  // __asm__  ( "mov    -0xa8(%rbp),%rax\n"
  // 	     "callq  *%rax"
  // 	     );

  // (2) And this makes it there but not back (good):
  // (RBP is the 64 bit stack pointer)
  // __asm__  ( "mov    -0xa8(%rbp),%rax\n"
  // 	     "jmp  *%rax"
  // 	     );

  // (3) And this removes the hard-coding and still gets there:
   asm("jmp *%0"::"r"(fn):);

   printf("Returned from call.  Done.\n");
   return;

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

  printf("Now to jump to that address \n");
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
  // struct {unsigned int offset; unsigned short segment;} dest;
  // unsigned long n2 = (unsigned long)p2;
  // dest.segment = 0x00; // whatever value you want in CS
  // dest.offset  = (unsigned short)n2;
  // printf("Computed segment/offset to jump to: %d / %d\n", dest.segment, dest.offset);
  // printf("Computed segment/offset to jump to: %p / %p\n", (long)dest.segment, (long)dest.offset);

  // Snips from the internet:
  // ----------------------------------------
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
  // ----------------------------------------
  printf("Wait, why are we still here?\n");
}


// ====================================================================================================

void generated_jmp_there(Assembler& a) 
{
  MyFn fn = function_cast<MyFn>( MemoryManager::getGlobal()->alloc(a.getCodeSize() + EXTRA_SPACE) );
  a.relocCode((void*)fn);

  printf("Put the first JITed code in addr %p, now JITing a second block to call the first.\n", (void*)fn);

  // This aims to make the same one-way jump as manual_jmp_there, except from JITed code.
  // --------------------------------------------------------------------------------
  Assembler a2;
  FileLogger logger(stderr);
  a2.setLogger(&logger);

  // This simulates the spliced-in jmp that will go inside the probe-ready site.
  // It has no proper calling conventions, because those are set up by the other
  // generated code.   Rather this is just a redirection.
  a2.jmp(imm((sysint_t)((void*)fn)));

  printf("Generated the jmp that we will need to splice into the probe_ready.  Size/offset %d %d\n", 
	 a2.getCodeSize(), a2.getOffset());

  // Why is it creating a 19-byte jmp??
  // It should be 6 bytes for an x86_64 jump.  The low cost annotations guide lists this:
  // Inte64 6 bytes -- jmp *.Lx(%rip)      Encoding:  FF 25 xx xx xx xx

  MyFn fn2 = function_cast<MyFn>(a2.make());
  fn2();
  // MemoryManager::getGlobal()->free((void*)fn);  
  printf("Actually got back from JITed code to static code..\n");
}

// ====================================================================================================

int main(int argc, char* argv[])
{
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
  a.call(imm((sysint_t)calledFn));
  // a.jmp(imm((sysint_t)calledFn));

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


  // Three options:
  // manual_jmp_there(a);  // Option (1)
  generated_jmp_there(a); // Option (2)
  // call_properly(a);    // Option (3)
}
