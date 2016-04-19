
#include <vector>

#include "defs.hpp"

namespace proc {

  class MappedRegion : public defs::Show {
    public:
      defs::Address start;
      defs::Address end;
      char* file;

      void show(FILE* fp, int nspaces) {
        std::string left_pad = getPadding(nspaces);

        fprintf(fp, "%s[%p-%p] %s\n", start, end, file);
      }
  };

  std::vector<MappedRegion> readMappedRegions();

}
