
#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <string>
#include <map>

#include "process.hpp"
#include "assembly.hpp"

namespace utils { 
namespace process {

typedef std::map<std::string, std::unique_ptr<Function>> FunctionsByName;
typedef std::map<utils::Address, Function*> FunctionsByAddress;
typedef std::map<utils::Address, std::unique_ptr<MappedRegion>> 
  MappedRegionsByAddress;
typedef std::map<utils::Address, std::unique_ptr<BasicBlock>> 
  BasicBlocksByAddress;

/** \brief Analyses both program executable on disk and process image to 
 *         gather meta data about the current process.
 *
 * Following methods are used to gather information. 
 *  - Reading ELF symbol tables for reading function symbol meta data (Linux).
 *  - Reading /proc/self/maps for discovering mapped memory regions (Linux).
 * 
 */
class ProcessAnalyzer {
  public:

    /** \brief Populate function meta data of the current process.
     */
    virtual void populateFunctions(FunctionsByAddress* fn_by_addr,
        FunctionsByName* fn_by_name);

    /** \brief Populate the mapped region meta data within the current process.
     */
    virtual void populateMappedRegions(MappedRegionsByAddress* mapped);

    /** \brief Gets the path of the executable of current process.
     *  \return The path of the executable
     */
    virtual std::string getProgramPath();

  private:
    std::string program_path;

};

/** \brief Analyses functions to derive structural information like basic 
 *   blocks and control returns.
 * 
 *  Function bodies are disassembled for discovering function structure 
 *  (i.e: basic blocks and control returns).
 */
class FunctionAnalyzer {
  public:
    static const int PROBE_READY_INSTRUCTION_SIZE;

    /** \brief Analyses structural details of a function.
     *  \param func A Function with function start and end fields set 
     *  \return     Sets the  structural information for the Function given
     *
     *  Discovers the following structural information
     *  1. Basic blocks and their boundaries
     *  2. Where control returns (i.e: returns, exits or tail calls) happen 
     *     within the function
     *  3. Locations of probe ready instructions within the function
     */
    void analyzeFunction(Function& func);

  private:
    utils::assembly::Disassembler disas;

};

} // End utils 
} // End process 

#endif /*ANALYSIS_H*/
