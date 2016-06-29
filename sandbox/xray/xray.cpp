
/* 
   Testing out Google X-Ray patching protocol 
   */ 
#include <unistd.h>
#include <memory.h>
#include <assert.h>
#include <cstdint> 
#include <cstdlib>
#include <atomic>
#include <sys/mman.h>

#include <pthread.h> 

#include <iostream>

using std::cout;

constexpr int NUM_RUNNERS = 32;

typedef uint8_t* Address;

int g_page_size=sysconf(_SC_PAGESIZE);

Address func = NULL;
Address sled = NULL;

void stub() {
  // cout << "Inside stub..\n";
}

bool set_page_rwe(Address addr,size_t nbytes) {

  void* start = reinterpret_cast<void*>(addr - 
    (reinterpret_cast<uint64_t>(addr)%g_page_size));

  uint64_t offset_into_page = ((reinterpret_cast<uint64_t>(addr))%g_page_size);
  size_t bytes = offset_into_page + nbytes;

  int r = mprotect(start, bytes,
      PROT_READ | PROT_WRITE | PROT_EXEC);
  if (r == 0) return true;
  else return false;
}

void* activator(void *arg) { 

  bool spin = *(bool*)arg;
  int32_t func_id = 1;
  static constexpr uint8_t call_Opcode = 0xE8;
  static constexpr uint16_t movR10_Seq = 0xBA41;
  static constexpr uint8_t jmp_Opcode = 0xE9;

  while(true) { 
    *reinterpret_cast<uint32_t *>(sled + 2) = func_id;
    *reinterpret_cast<uint8_t *>(sled + 6) = call_Opcode;
    *reinterpret_cast<uint32_t *>(sled + 7) = reinterpret_cast<int64_t>(stub) -
      (reinterpret_cast<int64_t>(sled) + 11);
    std::atomic_store_explicit(
      reinterpret_cast<std::atomic<uint16_t> *>(sled), movR10_Seq,
      std::memory_order_release);

    if (!spin) {
      break;
    }
  }  
} 

void* deactivator(void *arg) { 

  bool spin = *(bool*)arg;
  while (true) { 

    static constexpr uint16_t jmp_seq = 0x09EB;

    std::atomic_store_explicit(
      reinterpret_cast<std::atomic<uint16_t> *>(sled), jmp_seq,
      std::memory_order_release);

    *reinterpret_cast<uint64_t*>(sled + 2) = 0x00020000841F0F66; 
    *reinterpret_cast<uint8_t*>(sled + 10) = 0x00;

    if (!spin) {
      break;
    }
  }
}

void* toggler(void* arg) {

  /*
    // Do toggling once. If both patch and unpatch operations are accuate
    // it should call stub for a while initially and then the calls
    // should stop forever.
    bool spin = false;
    activator(&spin);
    sleep(4);
    deactivator(&spin);
    */

  while(true) {
    bool spin = false;
    activator(&spin);
    deactivator(&spin);
  }
}

void* runner(void *arg) { 

  while(true){
    (reinterpret_cast<void (*)(void )>(func))(); 
  }

}

void jit_func() {
  // Populate the function prolog
  *reinterpret_cast<uint8_t*>(func) = 0x55; // push %rbp
  *reinterpret_cast<uint8_t*>(func + 1) = 0x48; // push %rbp
  *reinterpret_cast<uint8_t*>(func + 2) = 0x89; // push %rbp
  *reinterpret_cast<uint8_t*>(func + 3) = 0xe5; // push %rbp

  // Populate the function epilog
  *reinterpret_cast<uint8_t*>(func + 15) = 0x5d; // pop %bp 
  *reinterpret_cast<uint8_t*>(func + 16) = 0xC3; // ret

  // Now populate the entry sled
  *reinterpret_cast<uint16_t*>(sled) = 0x09EB; // jmp rel8

  // nopw 512(%rax,%rax)
  *reinterpret_cast<uint64_t*>(sled + 2) = 0x00020000841F0F66; 
  *reinterpret_cast<uint8_t*>(sled + 10) = 0x00;
}

int main(int argc, char** argv) {

  /* where within the sled should the straddler occur */ 
  int straddler_point = 3;

  if (argc == 2){ /* if there is an argument */
    straddler_point = atoi(argv[1]);

    if (straddler_point < 2 || straddler_point > 10) {
      cout << "Straddling point should be within 2 and 10\n";
      exit(-1);
    }
  } 

  cout << "Setting straddler point at "<< straddler_point << 
    "(distance in byte into the sled)\n";

  pthread_t thread1, thread2; 
  int r1,r2; 
  pthread_t runners[NUM_RUNNERS]; 

  pthread_create(&thread1,
      NULL, 
      toggler, 
      reinterpret_cast<void*>(&r1));

  /*
  pthread_create(&thread1,
      NULL, 
      activator, 
      reinterpret_cast<void*>(&r1));

  pthread_create(&thread2,
      NULL, 
      deactivator, 
      reinterpret_cast<void*>(&r2)); 
      */

  Address buf = new uint8_t[1024]; 
  memset(buf,0x90,1024); 
  
  assert(set_page_rwe(buf, 1024));

  /* find a straddling position within buf */
  uint64_t buf_address = (uint64_t)buf; 
  size_t cache_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE);
  cout << "page size = " << g_page_size << "\n";
  cout << "cache line size = " << cache_line_size << "\n";

  /* how many bytes into a cache line does the buffer block begin */ 
  int64_t buf_offset = buf_address % cache_line_size; 
  cout << "BUF_OFFSET = " << buf_offset << "\n";

  int64_t closest_straddler_offset = (cache_line_size - buf_offset);

  Address cache_line_boundary = buf + closest_straddler_offset;
  // Deal with a start address having not enough space contain the sled 
  // entirely within a cacheline
  if (closest_straddler_offset < 15) {
    cache_line_boundary += cache_line_size;
  }

  func = cache_line_boundary - 4 - straddler_point; // JITTed func address
  sled = func + 4; // Address of the entry sled within the JITTed function

  cout << "func start addr = " << reinterpret_cast<void*>(func) << "\n";
  cout << "sled start addr = " << reinterpret_cast<void*>(sled) << "\n";
  cout << "cache line boundary = " << 
    reinterpret_cast<void*>(cache_line_boundary) << "\n";

  jit_func(); // JIT the function
 
  for (int i = 1; i < NUM_RUNNERS; i ++) { 
    pthread_create(&runners[i],
		   NULL, 
		   runner, 
		   NULL);
  }
  
  for (int i = 0; i < NUM_RUNNERS; i ++) { 
    pthread_join(runners[i],NULL); 
  }

  delete buf;

}

