
#ifndef _DEFS_HPP_
#define _DEFS_HPP_

#include <cstdint>
#include <ostream>
#include <cstdint>
#include <string>

namespace utils {

/// A byte addressible data type
typedef uint8_t* Address;

/// An implementable marker interface for printing data members of a class
class Show {

  public: 
    /** \brief Prints formatted information about data members of the class 
     *         to given file stream
     *
     * NOTE: Doing C I/O instead of C++ stream I/O here C++ streams not
     * working pre main.
     */
    virtual void show(FILE* fp, int nspaces) = 0;

    std::string getPadding(int nspaces) {
      std::string pad = "";
      while (nspaces-- > 0) {
        pad += " ";
      }

      return pad;
    }
};

/** \brief An implementable marker interface for denoting an optional 
 *         reference.
 *  
 *  Achieves something similar to boost::optional via inheritance. Not taking 
 *  a boost dependency just for this.
 */
class Optional {
  public:
    bool is_valid = true; ///< Is this reference valid?
}; 

} /* End utils */

#endif /* _DEFS_HPP_ */
