
#ifndef _CYG_FUNCTIONS_HPP_
#define _CYG_FUNCTIONS_HPP_

#include <cstdint>

/* Register encodings */
#define RAX 0
#define RCX 1
#define RDX 2
#define RBX 3
#define RSP 4
#define RBP 5
#define RSI 6
#define RDI 7

/* -- MOV operand encodings -- */
/* 
 * Reference: 
 *   http://www.intel.com/content/dam/www/public/us/en/documents/manuals/
 *   64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf
 *   [page 165]
 */

/* Register to memory movs */
#define MOV_REG_8 0x88
#define MOV_REG_16 0x89

/* Memory to register movs */
#define MOV_MEM_8 0x8A
#define MOV_MEM_16 0x8B

/* Immediate 8bit operand movs */
#define MOV_IMM_8_RAX 0xB0 // 0xB0 + RAX
#define MOV_IMM_8_RCX 0xB1 // 0xB0 + RCX
#define MOV_IMM_8_RDX 0xB2 // 0xB0 + RDX
#define MOV_IMM_8_RBX 0xB3 // 0xB0 + RBX
#define MOV_IMM_8_RSP 0xB4 // 0xB0 + RSP
#define MOV_IMM_8_RBP 0xB5 // 0xB0 + RBP
#define MOV_IMM_8_RSI 0xB6 // 0xB0 + RSI
#define MOV_IMM_8_RDI 0xB7 // 0xB0 + RDI

/* Immediate 16bit operand movs */
#define MOV_IMM_16_RAX 0xB8 // 0xB8 + RAX
#define MOV_IMM_16_RCX 0xB9 // 0xB8 + RCX
#define MOV_IMM_16_RDX 0xBA // 0xB8 + RDX
#define MOV_IMM_16_RBX 0xBB // 0xB8 + RBX
#define MOV_IMM_16_RSP 0xBC // 0xB8 + RSP
#define MOV_IMM_16_RBP 0xBD // 0xB8 + RBP
#define MOV_IMM_16_RSI 0xBE // 0xB8 + RSI
#define MOV_IMM_16_RDI 0xBF // 0xB8 + RDI

#define REX_PREFIX_0 0x48
#define REX_PREFIX_1 0x4c

#define PROBESIZE 16

#define ARGUMENT_PATCH_STRATEGY 0
#define CALL_REDIRECT_STRATEGY 1

//#ifdef __cplusplus
extern "C"
{
  void __cyg_profile_func_enter(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
  void __cyg_profile_func_exit(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
  void fake_cyg_profile_func_enter(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
  void fake_cyg_profile_func_exit(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
}
//#endif

#endif /* _CYG_FUNCTIONS_HPP__ */
