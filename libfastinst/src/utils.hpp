
#ifndef _UTILS_HPP_
#define _UTILS_HPP_

#include <cstdint>
#include <string>
#include <vector>
#include "patcher.h"

namespace utils {
  /// Tokenize a given string according to provided delimiter
  /* \param str The string to be tokenized
   * \param tokens The vector of tokens in the string
   * \param delimiter The delimite to tokenize with.
   */
  inline int tokenize(const std::string& str, std::vector<std::string>& tokens, const std::string& delimiter = " ") {
    std::string::size_type lastPos = str.find_first_not_of(delimiter, 0);
    std::string::size_type pos = str.find_first_of(delimiter, lastPos);

    int count = 0;
    while (std::string::npos != pos || std::string::npos != lastPos)
    {
      tokens.push_back(str.substr(lastPos, pos - lastPos));
      lastPos = str.find_first_not_of(delimiter, pos);
      pos = str.find_first_of(delimiter, lastPos);
      count++;
    }
    return count;
  } 

  inline void patch_first_argument(void* func_addr, void* call_addr, uint32_t probe_id) {
    Decoded d = decode_range(func_addr, call_addr);
    if (!DECODED_OK(d)) {
      fprintf(stderr, "Decode range did not work\n");
      exit(EXIT_FAILURE);
    }

    /* Looking for setter of EDI or RDI */
    int64_t setter = find_reg_setter(R_EDI,d);
    if (setter == -1) {
      setter = find_reg_setter(R_RDI,d);
      if (setter == -1) {
        printf("error finding setter\n");
        exit(EXIT_FAILURE);
      }
    }

    // Calculate the patch site content
    uint64_t sequence = *((uint64_t*)call_addr);
    uint64_t mask = 0xFFFFFF0000000000;
    uint64_t patch = (uint64_t) (sequence & mask);
    uint8_t* patch_ptr = (uint8_t*) (&patch);
    patch_ptr[0] = ((uint8_t*)call_addr)[0]; // MOV REG opcode
    *(uint32_t*)(patch_ptr+1) = probe_id; 

    // Caculate the patch site and patch it
    void* patch_site = (void*)((uint64_t)call_addr - setter);
    init_patch_site(call_addr, 8);
    init_patch_site(patch_site, 8);
    patch_64(patch_site, patch); 

  }

}

#endif /* _UTILS_HPP_ */
