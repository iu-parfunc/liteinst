
#include "utils.hpp"

#include <sys/param.h> // MAXPATHLEN
#include <unistd.h> // getcwd

#include <cstdlib>

namespace utils { 

  char* program_path = NULL;

  // TODO : Write atexit to free program_path buffer
  char* getProgramPath() {
    if (program_path == NULL) {
      program_path = (char*) malloc(sizeof(char) * MAXPATHLEN);
      ssize_t len = 0;
      if (program_path != NULL) {
        if ((len = readlink("/proc/self/exe", program_path, sizeof(char) *
                MAXPATHLEN)) == -1) {
          free(program_path);
          program_path = NULL;
          return NULL;
        }
      }
      program_path[len] = '\0';
    }
    return program_path;
  }
  
  int tokenize(const std::string& str, std::vector<std::string>& tokens, 
      const std::string& delimiter = " ") {
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
