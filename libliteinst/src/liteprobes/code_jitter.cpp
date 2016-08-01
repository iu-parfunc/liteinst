
#include "code_jitter.hpp"
#include "assembly.hpp"
#include "relocator.hpp"

#include <map>
#include <cstring>

namespace liteinst {
namespace liteprobes {

using namespace utils::assembly;

using std::unique_ptr;
using std::map;
using std::list;
using std::memcpy;
using utils::Address;
using utils::range::Range;

uint8_t g_short_circuit_near[] = { 0x66, 0x90 }; /* nop */ 
int g_short_circuit_near_size = sizeof(g_short_circuit_near) /sizeof(uint8_t); 

uint8_t g_short_circuit_far[] = { 0x0f, 0x1f, 0x44, 0x00, 0x00 }; /* nop */ 
int g_short_circuit_far_size = sizeof(g_short_circuit_far) /sizeof(uint8_t); 

uint8_t g_context_save[] =
{ 0x9c, /* pushfq */ 
  0x56, /* push %rsi */
  0x57, /* push %rdi */
  0x50, /* push %rax */
  0x53, /* push %rbx */
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
{ 0x48, 0xbf, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* movabs $00,%rdi - pg_id*/ 
  0x48, 0xbe, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* movabs $00,%rsi - p_id*/ 
  0x66, 0xba, 0x00, 0x00, /* mov $00,%dx - i_id */
  0x41, 0xb2, 0x00,       /* mov $00,%r10b - placement */
  0x49, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* movabs $00,%r8 - u_regs */ 
  0x49, 0xb9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 /* movabs $00,%r9 - address */ 
};
int g_args_size = sizeof(g_args) / sizeof(uint8_t);

uint8_t g_call[] = 
{ 
  0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* movabs $00,%rax */ 
  0x48, 0xff, 0xd0 /* call %rax */
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
  0x5b, /* pop %rbx */
  0x58, /* pop %rax */
  0x5f, /* pop %rdi */
  0x5e, /* pop %rsi */
  0x9d /* popfq */
};
int g_context_restore_size = sizeof(g_context_restore)/ sizeof(uint8_t);

uint8_t g_control_return[] = 
{ 
  0xe9, 0x00, 0x00, 0x00, 0x00 /* jmp $00 */
};
int g_control_return_size = sizeof(g_control_return) / sizeof(uint8_t);

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

inline Args emitArgs(Address start, Address probed_addr,
    const ProbeContext& context) {
  memcpy(start, g_args, g_args_size);

  Args args;
  args.start = start;
  args.pg_id_offset = 2; // Offsets directly from g_args
  args.p_id_offset = 12;
  args.i_id_offset = 22;
  args.placement_offset = 26;
  args.u_regs_offset = 29;
  args.address_offset = 39; 
  args.size = g_args_size;

  // Now write the arguments to the trampoline buffer
  *reinterpret_cast<uint64_t*>(start + args.pg_id_offset) = context.pg_id;
  *reinterpret_cast<uint64_t*>(start + args.p_id_offset)  = context.p_id;
  *reinterpret_cast<uint16_t*>(start + args.i_id_offset)  = context.i_id;
  *reinterpret_cast<uint8_t*>(start + args.placement_offset) = 
    static_cast<uint8_t>(context.placement);
  *reinterpret_cast<uint64_t*>(start + args.u_regs_offset) = 0;
  *reinterpret_cast<uint64_t*>(start + args.address_offset) = 
    reinterpret_cast<uint64_t>(probed_addr);

  return args;
} 

inline Call emitCall(Address start, const InstrumentationFunction fn) {
  memcpy(start, g_call, g_call_size);

  Call call;
  call.start = start;
  call.target = start + 2;
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

inline Return emitControlReturn(Address start, Address target) {
  memcpy(start, g_control_return, g_control_return_size);

  Return cr;
  cr.start = start;
  cr.target = start + 1;
  cr.size = g_control_return_size;

  *reinterpret_cast<int32_t*>(cr.target) = (int32_t)(target - start - 
      g_control_return_size);

  return cr;
}

unique_ptr<Springboard> CodeJitter::emitSpringboard(const CoalescedProbes& cp, 
    Address target, const InstrumentationProvider& provider) {
  map<Address, Probe*> probes = cp.probes;
  for (Springboard* sb : cp.springboards) {
    probes.insert(sb->probes.begin(), sb->probes.end());
  }

  assert(probes.size() > 0);

  Disassembler disas;
  const Sequence* seq = disas.disassemble(cp.range.start, cp.range.end);
  _DInst* decoded = static_cast<_DInst*>(seq->instructions);

  Springboard* sb = new Springboard;
  sb->base = cp.range.start;
  sb->type = (probes.size() > 1) ? SpringboardType::SUPER_TRAMPOLINE : 
    SpringboardType::TRAMPOLINE;
  sb->probes = probes;
  sb->displaced = cp.range;
  sb->instruction_offsets = new int[seq->n_instructions];
  sb->n_relocated = seq->n_instructions;

  int offset = 0;
  for (int i = 0; i < seq->n_instructions; i++) {
    sb->instruction_offsets[i] = offset;
    offset += decoded[i].size;
  }

  int probe_size = 0;
  int index = 0;
  while (probe_size < 5 && index < seq->n_instructions) {
    probe_size += decoded[index++].size;
  }

  assert(probe_size >= 5);
  
  sb->probe_length = probe_size;

  Relocator relocator;
  int* relocation_offsets = new int[seq->n_instructions];
  int relocation_ptr = 0;
  Address tramp_ip = target;
  Address code_ip = cp.range.start;

  Relocations relocations;
  for (auto it = probes.begin();  it != probes.end(); ++it) {
    Address probed_addr = it->first;
    ProbeContext context = it->second->context;  

    relocations = relocator.relocate(code_ip, probed_addr, tramp_ip);

    if (relocations.n_instructions > 0) {
      int offset_from_start = tramp_ip - target;

      for (int i=0; i < relocations.n_instructions; i++) {
        relocations.relocation_offsets[i] += offset_from_start;
      }
    
      memcpy(&relocation_offsets[relocation_ptr], relocations.relocation_offsets, 
          relocations.n_instructions * sizeof(int));
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

    Args args = emitArgs(tramp_ip, probed_addr, context);
    tramp_ip += args.size;

    const InstrumentationFunction fn = 
      (context.placement == ProbePlacement::ENTRY) ?
      provider.getEntryInstrumentation() : provider.getExitInstrumentation();

    Call call = emitCall(tramp_ip, fn);
    tramp_ip += call.size;

    ContextRestore cr = emitContextRestore(tramp_ip);
    tramp_ip += cr.size;

    int8_t jump_distance = tramp_ip - sc.start - sc.size;

    assert(jump_distance < 128);

    uint64_t original = *reinterpret_cast<uint64_t*>(sc.start);
    uint64_t mask = 0xFFFFFFFFFFFF0000;
    uint64_t jump = 0x0;
    uint64_t nop = 0x9066; /* nop */
    reinterpret_cast<uint8_t*>(&jump)[0] = 0xEB;
    reinterpret_cast<int8_t*>(&jump)[1] = jump_distance;
    sc.off_state = (original & mask) | jump; 
    sc.on_state = (original & mask) | nop; 

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
        relocations.n_instructions * sizeof(int));
    relocation_ptr += relocations.n_instructions;
  }

  tramp_ip += (cp.range.end - code_ip);

  assert(relocation_ptr == seq->n_instructions);

  Return ret;
  if (!cp.is_end_a_control_transfer) {
    ret = emitControlReturn(tramp_ip, cp.range.end);
    sb->control_return = ret;
  }

  tramp_ip += ret.size;

  sb->relocation_offsets = relocation_offsets;
  sb->range = Range(target, tramp_ip);

  return unique_ptr<Springboard>(sb);
}

int64_t CodeJitter::getSpringboardSize(const CoalescedProbes& cp) {
  Relocator r;
  map<Address, Probe*> probes = cp.probes;

  // Gets the size of all relocated regions for this springboard
  int64_t relocation_size = 0; 
  Address start = cp.range.start;
  Address end;
  for (const auto& it : probes) {
    end = it.first;
    relocation_size += r.getRelocationSize(start, end, nullptr);
    start = it.first;   
  }

  relocation_size += r.getRelocationSize(start, cp.range.end, nullptr);

  for (Springboard* sb : cp.springboards) {
    probes.insert(sb->probes.begin(), sb->probes.end());
  }

  int num_trampolines = probes.size();
  int64_t trampoline_size = g_short_circuit_near_size + g_context_save_size + 
    g_args_size + g_call_size +  g_context_restore_size;  
  
  int64_t springboard_size = relocation_size + num_trampolines * 
    trampoline_size;

  if(!cp.is_end_a_control_transfer) {
    springboard_size += g_control_return_size;
  }

  return springboard_size;
}

}  
}
