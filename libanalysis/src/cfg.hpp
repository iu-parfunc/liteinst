
#ifndef CFG_H
#define CFG_H

#include <cstdio>
#include <set>
#include <string>
#include <vector>
#include <map>
#include <memory>

#include "assembly.hpp"
#include "defs.hpp"

namespace analysis { 

/// Different types of control return from within a function
enum class ReturnType { 
  RET,       ///< Return from function 
  TAIL_CALL, ///< Tail call 
  HALT       ///< Program exit
};

class ControlTransfer; 
class DirectCallEdge;
class IndirectCallEdge;

/// Holds metadata related to a basic block
class BasicBlock : public utils::Show, public utils::Optional {
  public:
    utils::Address start  = nullptr; ///< Start address of the basic block
    utils::Address end    = nullptr; ///< Ending address of the basic block
    ControlTransfer* out_edge;        ///< End of block control transfer

    /** /brief Prints basic block information to given file descriptor
     */
    void show(FILE* fp, int nspaces) {
      std::string left_pad = getPadding(nspaces);
      fprintf(fp, "%sBlock : \n", left_pad.c_str());
      fprintf(fp, "%s  Address range : [%p - %p]\n", left_pad.c_str(), start,
          end);
      // pmd.show(fp, nspaces + 2);

      fprintf(fp, "\n");
    }
};

/// Holds meta data related to a function
class Function : public utils::Show, public utils::Optional {
  public:
    std::string name;    ///< Mangled name of the function
    utils::Address start = nullptr;  ///< Start address of the function
    utils::Address end   = nullptr;  ///< Ending address of the function

    ///< Basic blocks belonging to this function
    std::vector<BasicBlock*> basic_blocks; 

    /// Control returns of this function
    std::vector<ControlTransfer*> returns; 

    std::vector<DirectCallEdge*> direct_call_edges;

    std::vector<IndirectCallEdge*> indirect_call_edges;

    Function() {}

    ~Function() {}

    /** /brief Prints function information to given file descriptor
     */
    void show(FILE* fp, int nspaces);

};

class DirectCallEdge : public utils::Show, public utils::Optional {
  public:
    utils::Address address = nullptr; ///< Address where direct call is being made
    Function* target       = nullptr; ///< Direct called function

    void show(FILE* fp, int nspaces) {

    }
};

class IndirectCallEdge : public utils::Show, public utils::Optional {
  public:
    utils::Address address = nullptr;          ///< Address where indirect call is being made
    std::vector<Function*> targets;  ///< Direct called function(s)
    _DInst decoded;

    void show(FILE* fp, int nspaces) {

    }
};

/// Holds meta data related to a control return from within a function
class ControlTransfer : public utils::Show, public utils::Optional {
  public:
    utils::Address address = nullptr;   ///< Instruction address 
    std::set<BasicBlock*> targets;   /**< Control transfer targets.
                      *   May be null or more than one for indirect jumps where target is 
                      *   only known at runtime. */
    bool is_indirect = false;  ///< If this is an indirect control transfer
    bool is_return = false;  ///< If this is a return from a function
    ReturnType type;         ///< Return type

    /** /brief Prints control return information to given file descriptor
     */
    void show(FILE* fp, int nspaces) {
      std::string left_pad = getPadding(nspaces);
      std::string type_str;
      if (type == ReturnType::RET) {
        type_str = "RETURN";
      } else if (type == ReturnType::TAIL_CALL) {
        type_str = "TAIL CALL";
      } else if (type == ReturnType::HALT) {
        type_str = "HALT";
      }

      fprintf(fp, "%s  Address : %p\n", left_pad.c_str(), address);
      fprintf(fp, "%s  Type    : %s\n", left_pad.c_str(),
          (ReturnType::RET == type) ? "RETURN" : "TAIL CALL");

    }
};

class CFG : public utils::Show, public utils::Optional {
  public:
    std::vector<BasicBlock*> basic_blocks; 
    std::vector<Function*> functions;

    /// Basic block start address mappings
    std::map<utils::Address, BasicBlock*> basic_blocks_by_addr; 

    /// Function start address mappings
    std::map<utils::Address, Function*> fn_by_addr; 

    /** /brief Gets the basic block starting with given address
     *  /param addr The starting address of a baisc block
     *  /return     The basic block starting with the address. For an invalid
     *    start address the returned basic block instance would contain the
     *    invalid flag.
     */
    BasicBlock* getBasicBlock(utils::Address addr) {
      auto it = basic_blocks_by_addr.lower_bound(addr);
      if (it != basic_blocks_by_addr.end()) {
        if (it->second->start == addr) {
          return it->second;
        } else {
          if (it != basic_blocks_by_addr.begin()) {
            it--;
            return it->second;
          }
        }
      }

      return nullptr;
    }

    /** /brief Prints control flow graph information to given file descriptor
     */
    void show(FILE* fp, int nspaces) {
      // TODO
    }

};

class CG : public utils::Show, public utils::Optional {
  public:
    std::map<std::string, Function*> cf_map;  

    Function* getCallGraphNode(std::string func_name) {
      auto it = cf_map.find(func_name);
      if (it != cf_map.end()) {
        return it->second;
      }

      return nullptr;
    }

    /** /brief Prints call graph information to given file descriptor
     */
    void show(FILE* fp, int nspaces) {
      // TODO
    }

};

} // End analysis

#endif /*CFG_H*/
