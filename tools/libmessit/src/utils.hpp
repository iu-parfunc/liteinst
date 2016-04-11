
#include <string>
#include <vector>

int tokenize(const std::string& str, std::vector<std::string>& tokens, const std::string& delimiter = " ") {
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
