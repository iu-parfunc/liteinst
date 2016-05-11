
#include <cstdio>
#include <cstdlib>

#include <sys/param.h>  /* for MAXPATHLEN */

#include "proc.hpp"

namespace proc { 

  using std::vector;
  using std::string;

  using namespace defs;

  vector<MappedRegion>* readMappedRegions() {

    unsigned long long addr, endaddr, offset, inode;
    char permissions[8], device[8];
    char* filename = (char*) malloc(sizeof(char) * MAXPATHLEN);
    // char* tmp = (char*) malloc(512);

    FILE* fp;
    if ((fp = fopen ("/proc/self/maps", "r")) == NULL) {
      fprintf(stderr, "Could not open %s\n", "/proc/self/maps");
    }

    char* line = NULL;
    size_t len = 0;
    ssize_t read;
    vector<MappedRegion>* regions = new vector<MappedRegion>;
    // regions->reserve(100);
    while ((read = getline(&line, &len, fp)) != -1) {
      MappedRegion mr;
      sscanf (line,  "%llx-%llx %s %llx %s %llx %s", 
          &addr, &endaddr, permissions, &offset, device, &inode, filename);
      mr.start = (Address) addr;
      mr.end = (Address) endaddr;
      mr.file = string(filename);
      regions->push_back(mr);

    }

    free(line);

    return regions;

  }

}
