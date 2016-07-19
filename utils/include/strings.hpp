
#ifndef _STRINGS_HPP_
#define _STRINGS_HPP_

#include <string>
#include <sstream>
#include <vector>
#include <iomanip>

namespace utils {
namespace strings {

/// Tokenize a given string according to provided delimiter
/* \param str The string to be tokenized
 * \param tokens The vector of tokens in the string
 * \param delimiter The delimite to tokenize with.
 */
inline int tokenize(const std::string& str, std::vector<std::string>& tokens, const std::string& delimiter = " ") {
  std::string::size_type lastPos = str.find_first_not_of(delimiter, 0);
  std::string::size_type pos = str.find_first_of(delimiter, lastPos);

  int count = 0;
  while (std::string::npos != pos || std::string::npos != lastPos){
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    lastPos = str.find_first_not_of(delimiter, pos);
    pos = str.find_first_of(delimiter, lastPos);
    count++;
  }
  return count;
}

template<typename T>
T concat(T v) {
    return v;
}

template<typename T, typename... Args>
T concat(T first, Args... args) {
    return first + adder(args...);
}

template< typename T >
std::string int_to_hex_str(T i) {
  std::stringstream stream;
  stream << "0x" 
         // << std::setfill ('0') << std::setw(sizeof(T)*2) 
         << std::hex << i;
  return stream.str();
}

uint64_t hex_str_to_int(std::string str) {
  std::stringstream converter(str);
  unsigned int value;
  converter >> std::hex >> value;
  return value;
}

} /* End strings */
} /* End utils */

#endif /* _STRINGS_HPP_ */

