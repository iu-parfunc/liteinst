
#include "code_jitter.hpp"
#include "assembly.hpp"
#include "relocator.hpp"

#include <cstring>

namespace liteinst {
namespace liteprobes {

using namespace utils::assembly;

using std::unique_ptr;
using std::list;
using std::memcpy;
using utils::Address;
using utils::range::Range;

uint8_t g_short_circuit_near[] = { 0x0f, 0x1f, 0x00 }; /* nop */ 
int g_short_circuit_near_size = sizeof(g_short_circuit_near) /sizeof(uint8_t); 

uint8_t g_short_circuit_far[] = { 0x0f, 0x1f, 0x44, 0x00, 0x00 }; /* nop */ 
int g_short_circuit_far_size = sizeof(g_short_circuit_far) /sizeof(uint8_t); 

uint8_t g_context_save[] =
{ 0x9c, /* pushfq */ 
  0x56, /* push %rsi */
  0x57, /* push %rdi */
  0x50, /* push %rax */
  0x51, /* push %rcx */
  0x52, /* push %rdx */
  0x41, 0x50, /* push %r8 */
  0x41, 0x51, /* push %r9 */
  0x41, 0x52, /* push %r10 */
  0x41, 0x53, /* push %r11 */
  0x41, 0x54, /* push %r12 */
  0x41, 0x55, /* push %r13 */
  0x41, 0x56, /* push %r14 */
  0x41, 0x57 /* push %r15 */
};
int g_context_save_size = sizeof(g_context_save) / sizeof(uint8_t);

uint8_t g_args[] =
{ 0x48, 0xbf, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* movabs $00,%rdi */ 
  0x48, 0xbe, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* movabs $00,%rsi */ 
  0x66, 0xba, 0x00, 0x00, /* mov $00,%dx */
  0x41, 0xb2, 0x00,       /* mov $00,%r10b */
  0x49, 0xb9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* movabs $00,%r9 */ 
};
int g_args_size = sizeof(g_args) / sizeof(uint8_t);

uint8_t g_call[] = 
{ 0xff, 0x15, 0x00, 0x00, 0x00, 0x00,  /* callq *0x00(%rip) */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* trampoline fn address */
};
int g_call_size = sizeof(g_call) / sizeof(uint8_t); 

uint8_t g_context_restore[] =
{ 
  0x41, 0x5f, /* pop %r15 */
  0x41, 0x5e, /* pop %r14 */
  0x41, 0x5d, /* pop %r13 */
  0x41, 0x5c, /* pop %r12 */
  0x41, 0x5b, /* pop %r11 */
  0x41, 0x5a, /* pop %r10 */
  0x41, 0x59, /* pop %r9 */
  0x41, 0x58, /* pop %r8 */
  0x5a, /* pop %rdx */
  0x59, /* pop %rcx */
  0x58, /* pop %rax */
  0x5f, /* pop %rdi */
  0x5e, /* pop %rsi */
  0x9d /* popfq */
};
int g_context_restore_size = sizeof(g_context_restore)/ sizeof(uint8_t);

inline ShortCircuit emitShortCircuit(Address start) {
  memcpy(start, g_short_circuit_near, 
      g_short_circuit_near_size); 

  ShortCircuit sc;
  sc.start = start;
  sc.size = g_short_circuit_near_size;
  return sc;
}

inline ContextSave emitContextSave(Address start) {
  memcpy(start, g_context_save, g_context_save_size);

  ContextSave cs;
  cs.start = start;
  cs.size = g_context_save_size; 

  return cs;
}

inline Args emitArgs(Address start, const ProbeContext& context) {
  memcpy(start, g_args, g_args_size);

  Args args;
  args.start = start;
  args.pg_id_offset = 2; // Offsets directly from g_args
  args.p_id_offset = 12;
  args.i_id_offset = 22;
  args.placement_offset = 26;
  args.u_regs_offset = 29;
  args.size = g_args_size;

  // Now write the arguments to the trampoline buffer
  *reinterpret_cast<uint64_t*>(start + args.pg_id_offset) = context.pg_id;
  *reinterpret_cast<uint64_t*>(start + args.p_id_offset)  = context.p_id;
  *reinterpret_cast<uint16_t*>(start + args.i_id_offset)  = context.i_id;
  *reinterpret_cast<uint8_t*>(start + args.placement_offset) = 
    static_cast<uint8_t>(context.placement);
  *reinterpret_cast<uint64_t*>(start + args.u_regs_offset) = 0;

  return args;
} 

inline Call emitCall(Address start, InstrumentationFunction fn) {
  memcpy(start, g_call, g_call_size);

  Call call;
  call.start = start;
  call.target = start + 6;
  call.size = g_call_size;

  *reinterpret_cast<uint64_t*>(call.target) = reinterpret_cast<uint64_t>(fn);
  
  return call;
} 

inline ContextRestore emitContextRestore(Address start) {
  memcpy(start, g_context_restore, g_context_restore_size);

  ContextRestore cr;
  cr.start = start;
  cr.size = g_context_restore_size;

  return cr;
}

unique_ptr<Springboard> CodeJitter::emitSpringboard(const CoalescedProbes& cp, 
    Address target, ProbeContext& context, InstrumentationFunction fn) {
  list<Address> addrs = cp.probes;
  for (Springboard* sb : cp.springboards) {
    addrs.insert(addrs.end(), sb->probed_addrs.begin(), sb->probed_addrs.end());
  }

  addrs.sort();
  assert(addrs.size() > 0);

  Disassembler disas;
  const Sequence* seq = disas.disassemble(cp.range.start, cp.range.end);

  Springboard* sb = new Springboard;
  sb->base = cp.range.start;
  sb->type = (addrs.size() > 1) ? SpringboardType::SUPER_TRAMPOLINE : 
    SpringboardType::TRAMPOLINE;
  sb->probed_addrs = addrs;
  sb->range = cp.range;

  Relocator relocator;
  int* relocation_offsets = new int[seq->n_instructions];
  int relocation_ptr = 0;
  Address tramp_ip = target;
  Address code_ip = target;

  Relocations relocations;
  for (auto it = addrs.begin();  it != addrs.end(); ++it) {
    Address probed_addr = *it;

    relocations = relocator.relocate(code_ip, probed_addr, tramp_ip);

    if (relocations.n_instructions > 0) {
      int offset_from_start = tramp_ip - target;

      for (int i=0; i < relocations.n_instructions; i++) {
        relocations.relocation_offsets[i] += offset_from_start;
      }
    
      memcpy(&relocation_offsets[relocation_ptr], relocations.relocation_offsets, 
          relocations.n_instructions);
      relocation_ptr += relocations.n_instructions;
    }

    tramp_ip += (probed_addr - code_ip);
    code_ip = probed_addr;

    // The address where the callout for the probe really begins
    Address callout_addr = tramp_ip; 

    ShortCircuit sc = emitShortCircuit(tramp_ip);
    tramp_ip += sc.size;

    ContextSave cs = emitContextSave(tramp_ip);
    tramp_ip += cs.size;

    Args args = emitArgs(tramp_ip, context);
    tramp_ip += args.size;

    Call call = emitCall(tramp_ip, fn);
    tramp_ip += call.size;

    ContextRestore cr = emitContextRestore(tramp_ip);
    tramp_ip += cr.size;

    Callout* callout = new Callout;
    callout->start = callout_addr;
    callout->short_circuit = sc;
    callout->ctx_save = cs;
    callout->args = args;
    callout->call = call;
    callout->ctx_restore = cr;

    sb->callouts.emplace(probed_addr, unique_ptr<Callout>(callout));
  }

  // Finish relocating the last part of the code
  relocations = relocator.relocate(code_ip, cp.range.end, tramp_ip);

  if (relocations.n_instructions > 0) { 
    int offset_from_start = tramp_ip - target;

    for (int i=0; i < relocations.n_instructions; i++) {
      relocations.relocation_offsets[i] += offset_from_start;
    }

    memcpy(&relocation_offsets[relocation_ptr], relocations.relocation_offsets, 
        relocations.n_instructions);
    relocation_ptr += relocations.n_instructions;
  }

  assert(relocation_ptr == seq->n_instructions);

  sb->relocation_offsets = relocation_offsets;

  return unique_ptr<Springboard>(sb);
}

}  
}
