
#include <string>
#include <vector>

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
}
