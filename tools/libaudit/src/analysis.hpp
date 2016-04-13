
#ifndef _ANALYSIS_HPP_
#define _ANALYSIS_HPP_

#include <list>
#include <string>

#include "defs.hpp"
#include "disassembly.hpp"

namespace analysis {

  class ProbeMetaData : public defs::Show {
    public:
      uint32_t total_instruction_count;
      uint32_t probe_able_count;
      uint32_t pii_count;
      uint8_t start_instruction_size;
      int32_t start_probe_able_inst_distance;
      uint8_t end_instruction_size;
      int32_t end_probe_able_inst_distance;

      void show(FILE* fp, int nspaces) {
        std::string left_pad = getPadding(nspaces);

        /*
           os << left_pad << "Instructions : " << total_instruction_count << "\n"; 
           os << left_pad << "Probe able instructions : " << probe_able_count 
           << "\n"; 
           os << left_pad << "Position independent instructions : " << pii_count
           << "\n";
           os << left_pad << "Start instruction size : " << start_instruction_size 
           << "\n";
           os << left_pad << "End instruction size : " << end_instruction_size 
           << "\n";
           os << left_pad << "Distance to first probe able instruction from the " 
           << "start : " << start_probe_able_inst_distance << "\n";
           os << left_pad << "Distance to first probe able instruction from the "
           << "end : " << end_probe_able_inst_distance << "\n"; 
           */


        fprintf(fp, "%sInstructions : %d\n", 
            left_pad.c_str(), total_instruction_count); 
        fprintf(fp, "%sProbe able instructions : %d\n", 
            left_pad.c_str(), probe_able_count); 
        fprintf(fp, "%sPosition independent instructions : %d\n", 
            left_pad.c_str(), pii_count); 
        fprintf(fp, "%sStart instruction size : %d\n", 
            left_pad.c_str(), start_instruction_size); 
        fprintf(fp, "%sEnd instruction size : %d\n", 
            left_pad.c_str(), end_instruction_size); 
        fprintf(fp, "%sDistance to first probe able instruction " 
            "from the start : %d\n", left_pad.c_str(), 
            start_probe_able_inst_distance); 
        fprintf(fp, "%sDistance to first probe able instruction " 
            "from the end : %d\n", left_pad.c_str(), 
            end_probe_able_inst_distance); 
      }
  };

  class BasicBlock : public defs::Show {
    public:
      defs::Address start;
      defs::Address end;

      // Instruction offset from the function start to this basic block 
      uint64_t start_ins_offset; 
      // Instruction offset from the function start to this end of this basic
      // block 
      uint64_t end_ins_offset;
      ProbeMetaData pmd;

      void show(FILE* fp, int nspaces) {

        std::string left_pad = getPadding(nspaces);

        /*
           os << left_pad << "Block : \n";
           os << left_pad << "  Address range : [" << start << " - " << end 
           << "]\n";
           os << left_pad << " Instruction range : [" << start_ins_offset << " - "
           << end_ins_offset << "]\n";
           pmd.show(os, nspaces + 2);
           os << "\n";
           */

        fprintf(fp, "%sBlock : \n", left_pad.c_str());
        fprintf(fp, "%s  Address range : [%p - %p]\n", left_pad.c_str(), start,
            end);
        fprintf(fp, "%s  Instruction range : [%ld - %ld]\n", left_pad.c_str(),  
            start_ins_offset, end_ins_offset);
        pmd.show(fp, nspaces + 2);

        fprintf(fp, "\n"); 
      }
  };

  enum ReturnType { RET, TAIL_CALL };

  class ControlReturn : public defs::Show {
    public:
      defs::Address addr;
      defs::Address target;
      ReturnType type;

      void show(FILE* fp, int nspaces) {

        std::string left_pad = getPadding(nspaces);
        std::string type_str;
        if (type == RET) {
          type_str = "RETURN";
        } else {
          type_str = "TAIL CALL";
        }

        /*
           os << left_pad << "Return point : \n";
           os << left_pad << "  Address : " << addr << "\n";
           os << left_pad << "  Target  : " << target << "\n";
           os << left_pad << "  Type    : " << type_str << "\n"; 
           */

        fprintf(fp, "%sReturn point : \n", left_pad.c_str());
        fprintf(fp, "%s  Address : %p\n", left_pad.c_str(), addr);
        fprintf(fp, "%s  Target  : %p\n", left_pad.c_str(), target);
        fprintf(fp, "%s  Type    : %s\n", left_pad.c_str(), 
            (RET == type) ? "RETURN" : "TAIL CALL");
      }
  };

  class Function : public defs::Show {
    public: 
      std::string name;
      defs::Address start;
      defs::Address end;
      ProbeMetaData pmd;
      std::list<BasicBlock*> bbl;
      std::list<ControlReturn*> returns;
      uint64_t end_padding_size;

      void show(FILE* fp, int nspaces) {

        std::string left_pad = getPadding(nspaces);

        /*
           os << left_pad << "Function : \n";
           os << left_pad << "  Address range : [" << start << " - " << end 
           << "\n";
           pmd.show(os, nspaces + 2);

           if (bbl != NULL) {
           for (BasicBlock bb : *bbl) {
           bb.show(os, nspaces + 2);
           }
           }

           if (returns != NULL) {
           for (ControlReturn r : *returns) {
           r.show(os, nspaces + 2);
           }
           }

           os << "\n";
           */

        fprintf(fp, "%sFunction: %s\n", left_pad.c_str(), name.c_str());
        fprintf(fp, "%s  Address range : [%p - %p]\n", left_pad.c_str(), start,
            end);
        fprintf(fp, "%s  End padding   : %lu\n", left_pad.c_str(), 
            end_padding_size);
        pmd.show(fp, nspaces + 2);

        for (BasicBlock* bb : bbl) {
          bb->show(fp, nspaces + 2);
        }

        for (ControlReturn* r : returns) {
          r->show(fp, nspaces + 2);
        }

        fprintf(fp, "\n");
      }
  };

  class BlockStructure {
    public:
      std::list<BasicBlock*> bbl;
      std::list<ControlReturn*> returns; 
      defs::Address fn_end;
      uint64_t end_padding_size;
  };

  BlockStructure getBlockStructure(defs::Address start, defs::Address end, 
      disassembly::Decoded d);

}

#endif /* _ANALYSIS_HPP_ */
