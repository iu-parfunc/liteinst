/*
    Test desc:
    tests parallel updates of a STRADDLING call site that is being executed
    by multiple threads in parallel.
     - One thread repeatedly patches (call, nop)

    Currently this test is considered a success if it does not crash
    and if foo has been executed at least once.

*/
#include <stdio.h>
#include <memory.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include <patcher.h>

#define __USE_GNU
#include <signal.h>

#if defined(__ICC) || defined(__INTEL_COMPILER)

#else
#ifndef __USE_GNU
#define __USE_GNU
#endif
#endif
#include <pthread.h>

#ifndef ITERS
#define ITERS 50000000
#endif

/* where within the call should the straddler occur */
int g_call_straddler_point = 1;

unsigned long g_foo_val = 0;

/* control */
volatile int g_running = true;
volatile bool g_first_run = true;
uint64_t g_init_lock = 0;


/* patch info */
uint64_t g_call_addr = 0;
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_nop_patch = 0;  /* replacement isntr sequence */

uint64_t g_locked_call = 0;
uint64_t g_locked_nop  = 0;

uint64_t g_expected_bogus_call = 0;
uint64_t g_expected_bogus_nop  = 0;

/* globals */
uint8_t* fun = NULL;
unsigned int start_addr = 0;



void activator(int *arg) {
  struct timespec t;
  struct timespec r;
  t.tv_sec = 0;
  t.tv_nsec = 20;

  printf("activator waiting\n");
  while (g_first_run); /* wait until setup phase is done */

  for (int i = 0; i < ITERS; i ++){
    patch_64((void*)g_call_addr, g_orig_call);

    //sched_yield();
    //nanosleep(&t,&r);
    /* potentially check r for remainder
       but I dont think that is important for this test */

    patch_64((void*)g_call_addr, g_nop_patch);
  }

  g_running = false;
}


void print_bytes(const char* tag, uint64_t bytes, int num, int straddler_loc) {
  printf("%s, LSB first: ", tag);
  for (int i=0; i<num; i++) {
    if (i == straddler_loc)
      printf("| ");
    printf("%02lx ", bytes % 256);
    bytes /= 256;
  }
  printf("\n");
}

// The function which we call from the patch site:
void foo(void) {

  if (g_first_run) {
    if (__sync_bool_compare_and_swap(&g_init_lock,0,1)) {
      /* do init stuff */
      uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
      g_call_addr = (uint64_t)((uint8_t*)addr - 5);

      init_patch_site((void*)g_call_addr, 8);

      g_orig_call = *(uint64_t*)g_call_addr;

      // Keep 3 bytes, wipe the low 5:
      uint64_t keep_mask = 0xFFFFFF0000000000;
      // 5-byte noop instruction:
      uint64_t nop_opcodes = 0x0000000000441F0F;

      g_nop_patch = (g_orig_call & keep_mask) | nop_opcodes;

      print_bytes("nop_patch", g_nop_patch, 5, g_call_straddler_point);
      print_bytes("orig_call", g_orig_call, 5, g_call_straddler_point);

      g_locked_nop  = (g_nop_patch & 0xFFFFFFFFFFFFFF00) | 0xCC;
      g_locked_call = (g_orig_call & 0xFFFFFFFFFFFFFF00) | 0xCC;
      print_bytes("Locked noop:", g_locked_nop, 5, g_call_straddler_point);
      print_bytes("Locked call:", g_locked_call, 5, g_call_straddler_point);

      g_expected_bogus_call = (g_nop_patch & 0xFFFFFFFFFFFFFF00) | (g_orig_call & 0xFF);
      g_expected_bogus_nop  = (g_orig_call & 0xFFFFFFFFFFFFFF00) | (g_nop_patch & 0xFF);

      print_bytes("Expected bogus noop",  g_expected_bogus_nop, 5,  g_call_straddler_point);
      print_bytes("Expected bogus call", g_expected_bogus_call, 5, g_call_straddler_point);

      printf("Double check, g_call_addr = %p\n", (void*)g_call_addr);


      // Manually add in the relative jump as a sanity check:
      int relative = (int)(g_orig_call >> 8);
      printf("Computed absolute destination: %p\n", (void*)(g_call_addr + 5 + relative));
      relative = (int)(g_expected_bogus_call >> 8);
      void *bogus_target = (void*)(g_call_addr + 5 + relative);
      printf("Computed bogus destination: %p\n", bogus_target);

      // Hack this up to make the bogus target executable with an invalid
      // opcode, so that we can get a useful sigill if we take that brancj.
      uintptr_t bt = (uintptr_t)bogus_target & ~(UINT64_C(4096)-1);
      void *base = (void*)bt;
      void *page = mmap(base, 4096, PROT_READ | PROT_WRITE | PROT_EXEC,
                        MAP_ANONYMOUS | MAP_PRIVATE,
                        -1, 0);

      if (page == MAP_FAILED) {
        if (mprotect(base, 4096, PROT_READ | PROT_WRITE | PROT_EXEC) ) {
          abort();
        }
      }

      memset(base, 0x0E, 4096);

      if (mprotect(base, 4096, PROT_READ | PROT_EXEC) ) {
          abort();
      }

      g_first_run = false;
    }
  }

  g_foo_val++;
}


void runner(int *arg) {

  while(g_running){
    /* the call site that we patch is within fun */
    ((void (*)(void ))&fun[start_addr])();
  }


}

// Takes just a 5 byte instruction in the low bytes.
static void check_instruction_observation(uint64_t x) {
  x &= 0xFFFFFFFFFF;
  if (  x == (g_locked_nop   & 0xFFFFFFFFFF)
     || x == (g_locked_call  & 0xFFFFFFFFFF)
     || x == (g_nop_patch    & 0xFFFFFFFFFF)
     || x == (g_orig_call    & 0xFFFFFFFFFF)
     )
  {
  }
  else if (  x == (g_expected_bogus_call & 0xFFFFFFFFFF)
          || x == (g_expected_bogus_nop  & 0xFFFFFFFFFF)
          )
  {
    print_bytes("Expected bad state observed", x, 5, g_call_straddler_point);
  }
  else
  {
    print_bytes("Unexpected bad state observed", x, 5, g_call_straddler_point);
  }
}



static void inspect_patch_site() {
  int reps = 20;
  char str[1024];
  volatile uint64_t* ptr = (uint64_t*)g_call_addr;

  printf("  Polling patch location, first slow:\n");
  for(int i=0; i<reps; i++) {
    sprintf(str, "   rep %2d: Contents of patch site", i);
    uint64_t tmp = *ptr;
    print_bytes(str, tmp, 5, g_call_straddler_point);
    check_instruction_observation(tmp);
  }
  printf("  Then fast:\n");
  uint64_t observations[reps];
  for(int i=0; i<reps; i++) {
    observations[i] = *ptr;
  }
  for(int i=0; i<reps; i++) {
    check_instruction_observation(observations[i]);
    sprintf(str, "   rep %2d: Contents of patch site", i);
    print_bytes(str, observations[i], 5, g_call_straddler_point);
  }
}

static void sigill_handler(int signo, siginfo_t *inf, void* ctx) {
  ucontext_t *ucontext = (ucontext_t*)ctx;
  void *rip = (void*)ucontext->uc_mcontext.gregs[REG_RIP];
  assert(!inf || !inf->si_addr || inf->si_addr == rip);
  fprintf(stderr,"SigILL Handler: address of fault: %p\n", rip);
  fflush(stderr);
  inspect_patch_site();

  exit(EXIT_FAILURE);
}

static void sigseg_handler(int signo, siginfo_t *inf, void*ctx) {
  ucontext_t *ucontext = (ucontext_t*)ctx;
  void *rip = (void*)ucontext->uc_mcontext.gregs[REG_RIP];
  assert(!inf || !inf->si_addr || inf->si_addr == rip);
  fprintf(stderr,"SigSEGV Handler: address of fault: %p (%p)\n", rip,
            *(void**)rip);
  fflush(stderr);
  inspect_patch_site();
  raise(SIGABRT);
  //exit(EXIT_FAILURE);
}


int main(int argc, char** argv) {

  pthread_t thread1, thread2;
  int r1,r2;
  int num_runners = 4;
  pthread_t *runners;

  unsigned long it = 0;

  /* setup aux sig handlers */
  struct sigaction ill;
  struct sigaction seg;

  memset(&ill, 0, sizeof(ill));
  memset(&seg, 0, sizeof(seg));

  ill.sa_sigaction = &sigill_handler;
  ill.sa_flags = SA_SIGINFO;
  sigemptyset(&(ill.sa_mask));
  sigaction(SIGILL,&ill,NULL);

  seg.sa_sigaction = &sigseg_handler;
  seg.sa_flags = SA_SIGINFO;
  sigemptyset(&(seg.sa_mask));
  sigaction(SIGSEGV,&seg,NULL);


  /* ********************* */

  printf("Testing parallel updates to a STRADDLING call_site as it is being executed by multiple threads.\n");
  printf("Number of iterations: %d\n",ITERS);


  fun=(uint8_t*)malloc(1024);
  memset(fun,0x90,1024);

  if (argc == 1){
    printf("NO ARGS: Running with default settings\n");
  } else if (argc == 3){ /* if there is an argument */
    g_call_straddler_point = atoi(argv[1]);
    num_runners = atoi(argv[2]);
  } else {
    printf("INCORRECT ARGS\n");
    exit(EXIT_FAILURE);
  }

  printf("Setting straddler point at %d (distance in byte into the patch site)\n",g_call_straddler_point);
  printf("Running with %d threads executing the call site\n", num_runners);


  /* find a straddling position within fun */
  uint64_t fun_address = (uint64_t)fun;
  size_t cache_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE);

  /* how many bytes into a cache line does the fun block begin */
  unsigned int fun_offset = fun_address % cache_line_size;
  printf("FUN_OFFSET = %d\n",fun_offset);

  unsigned int closest_straddler_offset = (cache_line_size - fun_offset);

  start_addr = (closest_straddler_offset - 4) - g_call_straddler_point;
  printf("start_addr = %d\n",start_addr);
  printf("call_instr_offset = %d\n",start_addr + fun_offset + 4);
  printf("cache_line_offset = %d\n",start_addr + fun_offset);


  uint32_t* addr = (uint32_t*)(&fun[start_addr+5]);

  addr[0] = (uint32_t)((uint64_t)( (uint64_t)foo - (uint64_t)&fun[start_addr + 9]) );

  printf("Patch address, absolute         : %p\n", fun + start_addr + 4);
  printf("Target function absolute address: %p\n", foo);
  //  printf("Relative distance from patch to dest: %x\n", addr[0]);
  printf("Relative distance from patch to dest: %ld / %x\n",
         (long)((uint8_t*)foo - (fun + start_addr + 4)),
         (unsigned int)((uint8_t*)foo - (fun + start_addr + 4))
         );

  /* generate some code containing a call at a straddling location */
  fun[start_addr] = 0x55;     /* push %rbp */
  fun[start_addr + 1] = 0x48; /* mov %rsp, %rbp */
  fun[start_addr + 2] = 0x89;
  fun[start_addr + 3] = 0xe5;
  fun[start_addr + 4] = 0xe8; /* Call instr */
  /* Address */
  fun[start_addr + 9] = 0x5d; /* pop %rbp */
  fun[start_addr + 10] = 0xc3;


  init_patch_site((void*)&fun[start_addr],1024);


  pthread_create(&thread1,
         NULL,
         (void *) activator,
         (void *) &r1);


  /* Start the runners */
  runners = (pthread_t*)malloc(sizeof(pthread_t)*num_runners);
  int *ids = (int*)malloc(sizeof(int)*num_runners);

  for (int i = 0; i < num_runners; i ++) {
    ids[i] = i;
    pthread_create(&runners[i],
           NULL,
           (void *) runner,
           &ids[i]);
  }


  pthread_join(thread1, NULL);

  for (int i = 0; i < num_runners; i ++) {
    pthread_join(runners[i],NULL);
  }

  printf("function foo executed %ld times.\n",g_foo_val);

  printf("foo: %ld\n",
     g_foo_val);

  /* if (g_foo_val + g_bar_val == ITERS) { */
  if (g_foo_val > 0 ) {
    printf("Success\n");
    return 0;
  } else {
    printf("Failure\n");
    return 1;
  }


  /* if test reaches this point without crashing it is
     considered a success */

}
