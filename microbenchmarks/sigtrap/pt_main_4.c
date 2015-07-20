#include <stdio.h>
#include <inttypes.h>
#include <time.h>
#include <pthread.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>


volatile int active = 1; // Whether the function is toggled on or off. Not used currently
uint64_t* funcAddr = 0; // Address where function call happens which we need to toggle on/off
uint64_t activeSequence = 0; // Byte sequence for toggling on the function CALL
uint64_t deactiveSequence = 0; // NOP byte sequence for toggling off the function CALL
// Handling straddlers
uint64_t* straddle_part_1_start; // Start address of the first portion of the straddler
uint64_t* straddle_part_2_start; // Start address of the second portion of the straddler
uint64_t straddle_int3_sequence; // Sequence with int3 intermediate sequence
int64_t counter = 0;
int64_t invocations = 10000000;
volatile int ready_to_go = 0;
volatile int done = 0;
volatile int initial = 0;

const int NUM_THREADS = 5;

#define asm0 "call _Z3foov;"
#define asm1 "nop;  call _Z3foov;"
#define asm2 "nop;  nop;  call _Z3foov;"
#define asm3 "nop;  nop;  nop;  call _Z3foov;"
#define asm4 "nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm5 "nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm6 "nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm7 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm8 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm9 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm10 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm11 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm12 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm13 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm14 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm15 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm16 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm17 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm18 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm19 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm20 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm21 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm22 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm23 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm24 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm25 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm26 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm27 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm28 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm29 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm30 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm31 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm32 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm33 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm34 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm35 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm36 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm37 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm38 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm39 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm40 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm41 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm42 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm43 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm44 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm45 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm46 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm47 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm48 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm49 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm50 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm51 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm52 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm53 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm54 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm55 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm56 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm57 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm58 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm59 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm60 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm61 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm62 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm63 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm64 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"
#define asm65 "nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  nop;  call _Z3foov;"


inline int modify_page_permissions(uint8_t* addr) {

    long page_size = sysconf(_SC_PAGESIZE);
    int code = mprotect((void*)(addr - (((uint64_t)addr)%page_size)), page_size,
            PROT_READ | PROT_WRITE | PROT_EXEC);

    if (code) {
        fprintf(stderr, "mprotect was not successfull! code %d\n", code);
        fprintf(stderr, "errno value is : %d\n", errno);
        return 0;
    }

    // If the 8 bytes we need to modify straddles a page boundary make the next page writable too
    if (page_size - ((uint64_t)addr)%page_size < 8) {
        code = mprotect((void*)(addr-((uint64_t)addr)%page_size+ page_size) , page_size,
                PROT_READ | PROT_WRITE | PROT_EXEC);
        if (code) {
            fprintf(stderr, "mprotect was not successfull! code %d\n", code);
            fprintf(stderr, "errno value is : %d\n", errno);
            return 0;;
        }
    }

    return 1;
}

void add_call() {
    if (activeSequence != 0) {

        uint64_t* start_addr = funcAddr - 1;

        // fprintf(stderr, "Activating foo..\n");
	
        /* uint64_t res = __sync_val_compare_and_swap((uint64_t*) start_addr, */
        /*         *((uint64_t*)start_addr), activeSequence); */
	__sync_val_compare_and_swap((uint64_t*) start_addr,
                *((uint64_t*)start_addr), activeSequence);
        // *(uint64_t*) start_addr = activeSequence;
        active = 1;
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}

void remove_call() {

    if (deactiveSequence != 0) {
        /*
        int status = modify_page_permissions(funcAddr);
        if (!status) {
            return -1;
        }
        */

        uint64_t* start_addr = funcAddr - 1;

        //fprintf(stderr, "Deactivating foo..\n");
        /* uint64_t res = __sync_val_compare_and_swap((uint64_t*)start_addr, */
        /*         *((uint64_t*)start_addr), deactiveSequence); */
        __sync_val_compare_and_swap((uint64_t*)start_addr,
                *((uint64_t*)start_addr), deactiveSequence);
        // *(uint64_t*) start_addr = deactiveSequence;
        active = 0;
    } else {
        fprintf(stderr, "Active sequence not initialized..\n");
    }

}


__attribute__((noinline))
void foo() {

    if (!initial) {
      uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

    printf("------------------------------------\n");
    printf("address of call instr: %lx\n", (unsigned long)addr);
    
    for (int apa = 1; apa < 17; apa*=2) {  
        if ((unsigned long)addr % apa == 0)
        printf("address is %dbytes aligned\n",apa);
    } 
 
    uint64_t *probe_start = (uint64_t*)((uint8_t*)addr-5);
    uint64_t sequence =  *((uint64_t*)((uint8_t*)addr-5));
    printf("start address of probe site: %lx\n", (unsigned long)(addr-1));   
    unsigned long pm64 = (unsigned long) probe_start % 64; 
    int straddler = 0;
    int cutoff_point = 0;
    if (pm64 > 56) {
      printf("Probe site straddles cache line\nsince %lx %% 64 > 56\n",(unsigned long) probe_start);
      printf("%lx %% 64 = %ld\n",(unsigned long)probe_start, pm64);
      straddler = 1;
      cutoff_point = 64 - pm64;
    }
  
    unsigned long pm8 = (unsigned long)probe_start % 8; 
    if ( pm8 != 0) { 
      printf("Probe site straddles word boundary\nsince %lx %% 8 != 0\n",(unsigned long) probe_start);
      printf("%lx %% 8 = %ld\n", (unsigned long)probe_start, pm8);  
    }
    for (int apa = 1; apa < 17; apa*=2) {  
        if ((unsigned long)probe_start % apa == 0)
        printf("probe_site is %dbytes aligned\n",apa);
    }
    printf("------------------------------------\n");
    

    uint64_t mask = 0x0000000000FFFFFF;
    uint64_t deactive = (uint64_t) (sequence & mask);
    mask = 0x0000441F0F000000; // We NOP 5 bytes of CALL instruction and leave rest of the 3 bytes as it is
    
    activeSequence = sequence; // Saves the active 
    deactiveSequence = deactive |  mask;

    if (straddler) {
      uint64_t* straddle_point = (uint8_t*)probe_start + cutoff_point;
      straddle_part_1_start = straddle_point - 1;
      straddle_part_2_start = straddle_point;
      uint64_t straddle_part_1_sequence = *straddle_part_1_start; 
      int shift_size = (sizeof(uint8_t) * (cutoff_point-1));
      uint64_t int3mask = 0xCC;
      uint64_t ormask = 0xFF;
      if (cutoff_point > 1) {
        int3mask = (int3mask << shift_size);
        ormask = (ormask << shift_size);
      }
      straddle_int3_sequence = straddle_part_1_sequence & (~ormask | int3mask);
    }
   
    funcAddr = (uint64_t*)addr; // Save the call site address to a global variable so that it is visible to other threads
    fprintf(stderr, "INITIAL CALL\n");
    
    int status = modify_page_permissions((uint8_t*)funcAddr);
    if (!status) {
      fprintf(stderr, "ERROR : Failed to modify page permissions\n");
      return;
    }
    
    initial = 1;
  }
  
  if ( counter % 4000000 == 0)  
    printf("call of Cthu... I mean foo()\n");
  counter++;
    // fprintf(stderr, "Foo counter : %d\n", counter++);
}

void* stress_add(void*param) {
    while(!ready_to_go) {
        ;
    }

    while(!done) {
        add_call();
    }
    // I don't know what this return value means. 
    return NULL;
}

void* stress_remove(void* param) {
    while(!ready_to_go) {
        ;
    }

    while (!done) {
        remove_call();
    }
    return NULL;
}

/* void validate_patching() { */
/*     long invocs = 0; */

/* loop: */
/*     for (long i=0; i<invocations; i++) { */
/*         foo(); // This is the call site that we patch */
/*         invocs++; */
/*     } */

/*     if (done != 0) { */
/*       goto incr; */
/*     } else { */
/*       goto remove; */
/*     } */
/*     // No Jump to check  */
/*     //check: */
/*     if (done == 0) { */
/*       goto loop; */
/* remove: */
/*       remove_call(); */
/*     } else if (done == 1) { */
/*       goto loop; */
/*     } else { */
/*       add_call(); */
/*       goto loop; */
/*     } */

/* incr: */
/*    if (done < 2) { */
/*      done++; */
/*      goto loop;  */
/*    } */

    // Printing diff to make sure that there indeed have been some deactivations
//    fprintf(stderr, "Final count : %ld Invocations : %ld Diff : %ld..\n\n\n", counter, invocs, invocs - counter);
//}

int main() {
 
    // validate_patching();

    pthread_t threads[2*NUM_THREADS];
    int rc;

    // Initialize the globals used for patching
    // foo(3);

    for (int i=0; i < 2*NUM_THREADS; i+=2) {
      //int j=i;
      //int k=i+1;

        rc = pthread_create(&threads[i], NULL, stress_add, (void*)0);
        if (rc) {
            printf("ERROR: thread creation failed with error %d\n", rc);
        }

	rc = pthread_create(&threads[i+1], NULL, stress_remove, (void*)0);
        if (rc) {
           printf("ERROR: thread creation failed with error %d\n", rc);
        }
    }

    long i;
    for (i=0; i<invocations; i++) {
      //__asm__ ("call foo");
#ifdef ASM0
  __asm__(asm0);
#endif

#ifdef ASM1
  __asm__(asm1);
#endif

#ifdef ASM2
  __asm__(asm2);
#endif

#ifdef ASM3
  __asm__(asm3);
#endif

#ifdef ASM4
  __asm__(asm4);
#endif

#ifdef ASM5
  __asm__(asm5);
#endif

#ifdef ASM6
  __asm__(asm6);
#endif

#ifdef ASM7
  __asm__(asm7);
#endif

#ifdef ASM8
  __asm__(asm8);
#endif

#ifdef ASM9
  __asm__(asm9);
#endif

#ifdef ASM10
  __asm__(asm10);
#endif

#ifdef ASM11
  __asm__(asm11);
#endif

#ifdef ASM12
  __asm__(asm12);
#endif

#ifdef ASM13
  __asm__(asm13);
#endif

#ifdef ASM14
  __asm__(asm14);
#endif

#ifdef ASM15
  __asm__(asm15);
#endif

#ifdef ASM16
  __asm__(asm16);
#endif

#ifdef ASM17
  __asm__(asm17);
#endif

#ifdef ASM18
  __asm__(asm18);
#endif

#ifdef ASM19
  __asm__(asm19);
#endif

#ifdef ASM20
  __asm__(asm20);
#endif

#ifdef ASM21
  __asm__(asm21);
#endif

#ifdef ASM22
  __asm__(asm22);
#endif

#ifdef ASM23
  __asm__(asm23);
#endif

#ifdef ASM24
  __asm__(asm24);
#endif

#ifdef ASM25
  __asm__(asm25);
#endif

#ifdef ASM26
  __asm__(asm26);
#endif

#ifdef ASM27
  __asm__(asm27);
#endif

#ifdef ASM28
  __asm__(asm28);
#endif

#ifdef ASM29
  __asm__(asm29);
#endif

#ifdef ASM30
  __asm__(asm30);
#endif

#ifdef ASM31
  __asm__(asm31);
#endif

#ifdef ASM32
  __asm__(asm32);
#endif

#ifdef ASM33
  __asm__(asm33);
#endif

#ifdef ASM34
  __asm__(asm34);
#endif

#ifdef ASM35
  __asm__(asm35);
#endif

#ifdef ASM36
  __asm__(asm36);
#endif

#ifdef ASM37
  __asm__(asm37);
#endif

#ifdef ASM38
  __asm__(asm38);
#endif

#ifdef ASM39
  __asm__(asm39);
#endif

#ifdef ASM40
  __asm__(asm40);
#endif

#ifdef ASM41
  __asm__(asm41);
#endif

#ifdef ASM42
  __asm__(asm42);
#endif

#ifdef ASM43
  __asm__(asm43);
#endif

#ifdef ASM44
  __asm__(asm44);
#endif

#ifdef ASM45
  __asm__(asm45);
#endif

#ifdef ASM46
  __asm__(asm46);
#endif

#ifdef ASM47
  __asm__(asm47);
#endif

#ifdef ASM48
  __asm__(asm48);
#endif

#ifdef ASM49
  __asm__(asm49);
#endif

#ifdef ASM50
  __asm__(asm50);
#endif

#ifdef ASM51
  __asm__(asm51);
#endif

#ifdef ASM52
  __asm__(asm52);
#endif

#ifdef ASM53
  __asm__(asm53);
#endif

#ifdef ASM54
  __asm__(asm54);
#endif

#ifdef ASM55
  __asm__(asm55);
#endif

#ifdef ASM56
  __asm__(asm56);
#endif

#ifdef ASM57
  __asm__(asm57);
#endif

#ifdef ASM58
  __asm__(asm58);
#endif

#ifdef ASM59
  __asm__(asm59);
#endif

#ifdef ASM60
  __asm__(asm60);
#endif

#ifdef ASM61
  __asm__(asm61);
#endif

#ifdef ASM62
  __asm__(asm62);
#endif

#ifdef ASM63
  __asm__(asm63);
#endif

#ifdef ASM64
  __asm__(asm64);
#endif

#ifdef ASM65
  __asm__(asm65);
#endif


      
      //foo(5); // This is the call site that we patch
        if (i==0) {
            fprintf(stderr, "Initial call done..\n");
            ready_to_go =1;
        }
    }

    done =1; 
    int *k = NULL;
    // Wait for all threads to finish
    for (int i=0; i<2*NUM_THREADS; i++) {
        pthread_join(threads[i], (void**)&k);
    }


    // Printing diff to make sure that there indeed have been some deactivations
    fprintf(stderr, "Final count : %ld Invocations : %lu Diff : %ld..\n\n\n", counter, i, (long)i - counter);
}