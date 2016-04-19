
#include <cstdio>
#include <cstdlib>
#include <sys/param.h>  /* for MAXPATHLEN */

#include "proc.hpp"

namespace proc { 

  using std::vector;

  using namespace defs;

  vector<MappedRegion> readMappedRegions() {

    long long addr, endaddr, size, offset, inode;
    char permissions[8], device[8];
    char* filename = (char*) malloc(sizeof(char) * filename[MAXPATHLEN]);

    FILE* fp;
    if ((fp = fopen ("/proc/self/maps", "r")) == NULL) {
      fprintf(stderr, "Could not open %s\n", "/proc/self/maps");
    }

    char* line = NULL;
    size_t len = 0;
    ssize_t read;
    vector<MappedRegion> regions;
    while ((read = getline(&line, &len, fp)) != -1) {
      MappedRegion mr;
      sscanf (line,  "%llx-%llx %s %llx %s %llx %s", 
          addr, endaddr, permissions, offset, device, inode, filename);
      mr.start = (Address) addr;
      mr.end = (Address) endaddr;
      mr.file = filename;
      regions.push_back(mr);

      free(line);
    }

    return regions;

  }

}
