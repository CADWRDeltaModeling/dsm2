/**
 *
 */
#include "PTM2PostprocessorParameters.h"
#include <algorithm>

namespace PTM2
{

PTM2PostprocessorParameters::PTM2PostprocessorParameters()
: accumulated_(false)
{
}

PTM2Group* PTM2PostprocessorParameters::findGroup(const std::string& name)
{
  PTM2Group target(name);
  std::vector<PTM2Group>::iterator re = std::find(groups_.begin(), groups_.end(), target);
  if (re != groups_.end()) {
    return &(*re);
  }
  else {
    return NULL;
  }
}

}
