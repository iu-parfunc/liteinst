
#ifndef PROCESS_H
#define PROCESS_H

#include <cstdio>
#include <string>
#include <vector>
#include <map>
#include <memory>

#include "concurrency.hpp"
#include "defs.hpp"
#include "addr_range.hpp"

namespace utils { 
namespace process { 

/// Different types of control return from within a function
enum class ReturnType { 
  RET,       ///< Return from function 
  TAIL_CALL, ///< Tail call 
  HALT       ///< Program exit
};

/// Holds meta data related to a mapped region in virtual memory for this 
/// process
class MappedRegion : public Show, public Optional {
  public:
    utils::Address start;    ///< Start address of the mapped region
    utils::Address end;      ///< Ending address of the mapped region
    std::string file; ///< Backing file for this mapped region

    /** /brief Prints mapped region information to given file descriptor
     */
    void show(FILE* fp, int nspaces) {
      std::string left_pad = getPadding(nspaces);

      fprintf(fp, "%s[%p-%p] %s\n", left_pad.c_str(), start, end, 
          file.c_str());
    }
};

/// Holds meta data related to a control return from within a function
class ControlReturn : public Show, public Optional {
  public:
    utils::Address addr;    ///< Return instruction address 
    utils::Address target;  /**< Control transfer target.
                      *   May be null for indirect jumps where target is 
                      *   only known at runtime. */
    ReturnType type; ///< Return type

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

      fprintf(fp, "%sReturn point : \n", left_pad.c_str());
      fprintf(fp, "%s  Address : %p\n", left_pad.c_str(), addr);
      fprintf(fp, "%s  Target  : %p\n", left_pad.c_str(), target);
      fprintf(fp, "%s  Type    : %s\n", left_pad.c_str(),
          (ReturnType::RET == type) ? "RETURN" : "TAIL CALL");

    }
};

/// Holds metadata related to a basic block
class BasicBlock : public Show, public Optional {
  public:
    utils::Address start; ///< Start address of the basic block
    utils::Address end;   ///< Ending address of the basic block

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
class Function : public Show, public Optional {
  public:
    std::string name;    ///< Mangled name of the function
    utils::Address start;       ///< Start address of the function
    utils::Address end;         ///< Ending address of the function
    utils::Address next;        ///< Starting address of the next function after this
    int64_t end_padding; ///< The padding space between the current and next 
                         ///< functions

    /// Basic block start address mappings
    std::map<utils::Address, std::unique_ptr<BasicBlock>> basic_blocks; 
    /// Control returns of this function
    std::vector<std::unique_ptr<ControlReturn>> returns; 
    std::vector<utils::Address> probe_ready_sites;     ///< Probe ready 
                                                       ///< instruction sites of
                                                       ///< this function
    static utils::concurrency::SpinLock init_lock;
    static bool is_initialized;

    static std::unique_ptr<BasicBlock> invalid_bb;
    static std::unique_ptr<ControlReturn> invalid_cr;

    Function();
    ~Function();

    /** /brief Gets the basic block starting with given address
     *  /param addr The starting address of a baisc block
     *  /return     The basic block starting with the address. For an invalid
     *    start address the returned basic block instance would contain the
     *    invalid flag.
     */
    BasicBlock* getBasicBlock(utils::Address addr);

    /** /brief Gets the basic blocks contained within the current function.
     *  /return     The list of basic blocks of current function.
     */
    std::vector<BasicBlock*> getBasicBlocks();

    /** /brief Gets the control returns contained within the current function.
     *  /return     The list of control returns of current function.
     */
    std::vector<ControlReturn*> getReturns();

    /** /brief Prints function information to given file descriptor
     */
    void show(FILE* fp, int nspaces);
};

/// Holds meta data related to the program process image
class Process : public Show, public Optional {
  public:

    Process();
    ~Process();

    /** /brief Gets the function which the given address is in.
     *  /param addr The address to for which the containing function needs to 
     *    be found
     *  /return     The function containing the address. For address without 
     *    any associated function the returned function instance would contain
     *    the invalid flag.
     */
    Function* getContainedFunction(utils::Address addr);

    /** /brief Gets the function starting with given address. 
     *  /param addr The starting address of a function 
     *  /return     The function starting with the address. For an invalid 
     *    start address the returned function instance would contain the 
     *    invalid flag.
     */
    Function* getFunction(utils::Address addr);

    /** /brief Gets the function starting with given function name. 
     *  /param name The name of a function 
     *  /return     The function starting with the address. For an invalid 
     *    name the returned function instance would contain the 
     *    invalid flag.
     */
    Function* getFunction(std::string name);


    /** /brief Gets the functions associated with the current process. 
     *  /return     The list of functions in the current process.
     */
    std::vector<Function*> getFunctions();

    /** /brief Gets the number of functions associate with the current process.
     *  /return The number of functions in the current processs.
     */
    int getNumberOfFunctions();

    /** /brief Gets the mapped region which the given address is in.
     *  /param addr The address to for which the containing region needs to 
     *    be found
     *  /return     The region containing the address. For address without 
     *    any associated region the returned function instance would contain
     *    the invalid flag.
     *
     *  Returning an invalid region doesn't mean the a mapping may not exist by
     *  the time the return value is used however, since mapping 
     *  information are only read at the start of the process execution.
     */
    MappedRegion* getContainedMappedRegion(utils::Address addr);

    /** /brief Gets the region starting with given address. 
     *  /param addr The starting address of a region 
     *  /return     The region starting with the address. For an invalid 
     *    start address the returned function instance would contain the 
     *    invalid flag.
     *
     *  Returning an invalid region doesn't mean the a mapping may not exist by
     *  the time the return value is used however, since mapping 
     *  information are only read at the start of the process execution.
     */
    MappedRegion* getMappedRegion(utils::Address addr);

    /** /brief Gets the regions associated with the current process at the 
     *   first initialization of Process. 
     *  /return     The list of mapped region for the current process.
     */
    std::vector<MappedRegion*> getMappedRegions();

    /** /brief Gets the range that the stack occupies at the first 
     *   initialization of Process.
     *  /return The stack range
     */
    utils::range::Range getStack();

    /** /brief Gets the range that the heap occupies at the first
     *   initialization of Process. 
     *  /return The heap range
     */
    utils::range::Range getHeap();

    /** /brief Gets the range that the text occupies.
     *  /return The range of text
     */
    utils::range::Range getText();

    /** /brief Prints process information to given file descriptor
     */
    void show(FILE* fp, int nspaces);

  private:
    static utils::concurrency::SpinLock init_lock;
    static bool is_initialized;
    static std::unique_ptr<MappedRegion> invalid_mr;
    static std::unique_ptr<Function> invalid_fn;

    static utils::range::Range text;
    static utils::range::Range stack;
    static utils::range::Range heap;

    /// Function start address mappings
    static std::map<utils::Address, Function*> fn_by_address;  

    /// Function name mappings
    static std::map<std::string, std::unique_ptr<Function>> fn_by_name;  

    /// Memory region start address mappings
    static std::map<utils::Address, std::unique_ptr<MappedRegion>>* mapped; 


};

} // End process 
} // End utils 

#endif /*PROCESS_H*/
