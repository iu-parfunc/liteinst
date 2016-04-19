
#include <string>
#include <vector>

namespace utils { 

  char* getProgramPath();

  int tokenize(const std::string& str, std::vector<std::string>& tokens, 
      const std::string& delimiter);

}
