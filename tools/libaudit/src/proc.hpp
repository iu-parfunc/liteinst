
#include <string>
#include <vector>

#include "defs.hpp"

namespace proc {

  class MappedRegion : public defs::Show {
    public:
      defs::Address start;
      defs::Address end;
      std::string file;

      void show(FILE* fp, int nspaces) {
        std::string left_pad = getPadding(nspaces);

        fprintf(fp, "%s[%p-%p] %s\n", left_pad.c_str(), start, end, 
            file.c_str());
      }
  };

  std::vector<MappedRegion>* readMappedRegions();

}
