/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _SourceFlow_h_
#define _SourceFlow_h_

#include "Waterbody.h"

namespace PTM2
{

/**
 * @brief SourceFlow waterbody
 **/
class SourceFlow : public Waterbody
{
protected:

public:
  SourceFlow(const int& id, const std::string& externalId)
    : Waterbody(id, externalId)
  {}
  virtual int getType() const { return SOURCEFLOW; }
};

}

#endif