
#ifndef _DEFS_HPP_
#define _DEFS_HPP_

#include <ostream>
#include <cstdint>
#include <string>

namespace defs {

  typedef uint8_t* Address;

  class Show {

    public: 
      // NOTE: Doing C I/O instead of C++ stream I/O here
      // due C++ streams not working pre main for some reason.
      virtual void show(FILE* fp, int nspaces) = 0;

      std::string getPadding(int nspaces) {
        std::string pad = "";
        while (nspaces-- > 0) {
          pad += " ";
        }

        return pad;
      }
  };

}

#endif /* _DEFS_HPP_ */
